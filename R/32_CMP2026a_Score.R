# ============================================================
# 32_CMP2026a_Score.R
#
# Score the CMP-update manifestos (new + changed, from R/30) with the
# PRODUCTION v4-safe pipeline of R/15b (Claude), R/16 (GPT-4.1-mini) and
# R/18 (Gemini). Nothing is reimplemented: the production function and
# constant definitions are evaluated directly from those scripts (their
# top-level pipelines are NOT executed), together with R/1, R/3c, R/12.
# The only addition is a thin pass-through wrapper around each chat
# function that logs token usage to CSV (for cost accounting).
#
# Usage:
#   Rscript R/32_CMP2026a_Score.R anchor   <model> [gemini_model_id]
#   Rscript R/32_CMP2026a_Score.R estimate
#   Rscript R/32_CMP2026a_Score.R score    <model> [gemini_model_id]
#   Rscript R/32_CMP2026a_Score.R finalize <model>
#   <model> in {claude, gpt41mini, gemini}
#
#   anchor   : re-score ANCHOR_N already-scored manifestos and compare with
#              the original raw chunk scores (drift / alias-resolution test)
#   estimate : token + cost estimate for the update set (all three models)
#   score    : checkpointed, resumable scoring of the update set
#   finalize : combine checkpoints -> raw parquet, R/12 span validation
#              (no aggregation: that is R/1_Aggregation.R)
#
# Outputs (all under data/CMP/update_<VER>/, never touches existing files):
#   checkpoints_<model>/batch_NNNN.RData
#   poplib_v4_<model>_raw.parquet / _validated_spans.parquet  (_agg.parquet: no longer written)
#   anchor_<model>[_<gemini id>].parquet
#   usage_logs/usage_<pid>.csv, cost_estimate.parquet
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(readxl); library(lubridate)
  library(jsonlite); library(httr); library(furrr)
})
source("R/1_Functions.R")
source("R/3c_Populism_v4_safe.R")
source("R/12_Validate_Span_Quality.R")

args  <- commandArgs(trailingOnly = TRUE)
MODE  <- args[1] %||% "estimate"
WHICH <- args[2] %||% NA_character_
GEMINI_OVERRIDE <- args[3] %||% NA_character_

VER      <- Sys.getenv("CMP_UPDATE_VER", "2026a")
UPD_DIR  <- file.path("data/CMP", paste0("update_", VER))
UPD_FILE <- file.path("data/CMP", paste0("full_data_update_", VER, ".parquet"))
USAGE_DIR <- file.path(UPD_DIR, "usage_logs")
ensure_dir(USAGE_DIR)

CHECKPOINT_EVERY_UPD <- 20L
WORKERS   <- c(claude = 2L, gpt41mini = 3L, gemini = 2L)   # modest: other agents share the keys
if (nzchar(Sys.getenv("CLAUDE_WORKERS"))) WORKERS[["claude"]] <- as.integer(Sys.getenv("CLAUDE_WORKERS"))
if (nzchar(Sys.getenv("GEMINI_WORKERS"))) WORKERS[["gemini"]] <- as.integer(Sys.getenv("GEMINI_WORKERS"))
ANCHOR_N  <- as.integer(Sys.getenv("ANCHOR_N", "8"))
ANCHOR_SEED <- as.integer(Sys.getenv("ANCHOR_SEED", "2026"))

# ---- Load production definitions without executing the driver scripts ----

load_defs <- function(file, keep) {
  ex <- parse(file, keep.source = FALSE)
  got <- character()
  for (e in ex) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% keep) {
      eval(e, envir = globalenv()); got <- c(got, as.character(e[[2]]))
    }
  }
  miss <- setdiff(keep, got)
  if (length(miss)) stop(file, ": definitions not found: ", paste(miss, collapse = ", "))
  invisible(got)
}

CHUNK_CONSTS <- c("FULL_TEXT_TOKEN_CAP", "CHUNK_TOKENS", "OVERLAP_TOKENS", "V4_DIMS",
                  "MAX_RETRIES_PER_CHUNK", "RETRY_BASE_SLEEP_SEC")
chunk_fingerprint <- function() paste(FULL_TEXT_TOKEN_CAP, CHUNK_TOKENS, OVERLAP_TOKENS,
                                      paste(V4_DIMS, collapse = ","))

load_defs("R/15b_Claude_Full_Corpus.R",
          c(CHUNK_CONSTS, "MODEL_CLAUDE", "anthropic_v4_schema", "anthropic_v4_chat",
            "score_chunk_v4_claude", "decode_dim_string", "flatten_parsed_v4_claude", "score_manifesto_v4_claude"))
fp_claude <- chunk_fingerprint()
load_defs("R/16_Score_Full_Corpus_gpt41mini.R",
          c(CHUNK_CONSTS, "MODEL", "score_chunk_v4_robust", "flatten_parsed_v4",
            "score_manifesto_v4_robust"))
fp_gpt <- chunk_fingerprint()
load_defs("R/18_Score_Full_Corpus_Gemini_flash_latest.R",
          c(CHUNK_CONSTS, "MODEL_GEMINI", "to_gemini_schema", "gemini_v4_schema",
            "gemini_v4_chat", "score_chunk_v4_gemini", "flatten_parsed_v4_gemini",
            "score_manifesto_v4_gemini"))
stopifnot("chunking config must be identical across production scripts" =
            fp_claude == fp_gpt && fp_gpt == chunk_fingerprint())

# Gemini: production uses the floating alias "gemini-flash-latest". An explicit
# pinned ID can be passed as 3rd argument (decided from the anchor test).
if (!is.na(GEMINI_OVERRIDE)) MODEL_GEMINI <- GEMINI_OVERRIDE
MODEL_ID <- c(claude = MODEL_CLAUDE, gpt41mini = MODEL, gemini = MODEL_GEMINI)

# Phase 1 (2026-10-13) options, via environment:
#   CMP_TARGET     update (default; the 83 new/changed docs) | norm (131 normalisation-only
#                  changed docs) | retry (failed June chunks, see Data section) |
#                  parseloss (Claude only, 2026-10-24: the 363 merged v2026a chunks with
#                  parse-lost dimensions, re-scored with the patched R/15b flattener; cap
#                  PARSELOSS_CAP_USD, default 45)
#   GEMINI_BACKEND aistudio (default; production endpoint) | vertex (Vertex AI, global)
#   EXPECT_GEMINI  if set, any Gemini response whose modelVersion differs aborts the run
TARGET         <- Sys.getenv("CMP_TARGET", "update")
GEMINI_BACKEND <- Sys.getenv("GEMINI_BACKEND", "aistudio")
EXPECT_GEMINI  <- Sys.getenv("EXPECT_GEMINI", "")
SUFFIX         <- if (TARGET == "update") "" else paste0("_", TARGET)
ABORT_FILE     <- file.path(UPD_DIR, "ABORT_gemini_model_version")
STOP_FILE      <- function(provider) file.path(UPD_DIR, paste0("STOP_billing_", provider))
BILLING_PAT    <- paste0("credit balance|usage limit|billing|insufficient_quota|",
                         "exceeded your current quota|spending cap|PERMISSION_DENIED")

# ---- Usage-logging pass-through wrappers (do not change requests/outputs) ----

# The production chat functions drop the response's model field. To log the
# RESOLVED model string for every call, an exit trace on httr::content records
# `model` (Anthropic/OpenAI) or `modelVersion` (Gemini) of each parsed response.
# Requests and returned values are unchanged.
ensure_model_trace <- function() {
  if (isTRUE(getOption("upd_model_trace"))) return(invisible())
  suppressMessages(trace("content", where = asNamespace("httr"), print = FALSE,
    exit = quote({
      .rv <- returnValue()
      if (is.list(.rv)) {
        .mv <- if (!is.null(.rv$modelVersion)) .rv$modelVersion else .rv$model
        if (is.character(.mv)) assign(".upd_last_model", .mv, envir = .GlobalEnv)
      }
    })))
  options(upd_model_trace = TRUE)
}
traced_call <- function(fun, ...) {
  ensure_model_trace()
  assign(".upd_last_model", NA_character_, envir = .GlobalEnv)
  r <- fun(...)
  list(r = r, resolved = get0(".upd_last_model", envir = .GlobalEnv, ifnotfound = NA_character_))
}
doc_of <- function(user_prompt) {
  m <- regmatches(user_prompt, regexpr("DOC_ID: \\S+", user_prompt))
  if (length(m)) sub("DOC_ID: ", "", m) else NA_character_
}
log_usage <- function(provider, model, usage, resolved = NA_character_, doc = NA_character_,
                      extra = NA_character_) {
  u <- usage %||% list()
  row <- data.frame(
    ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), provider = provider, model = model,
    resolved_model = resolved %||% NA_character_, doc_id = doc %||% NA_character_,
    in_tok  = as.numeric(u$input_tokens %||% u$promptTokenCount %||% NA),
    out_tok = as.numeric(u$output_tokens %||% u$candidatesTokenCount %||% NA),
    thought_tok = as.numeric(u$thoughtsTokenCount %||%
                               u$output_tokens_details$reasoning_tokens %||% 0),
    extra = extra)
  f <- file.path(USAGE_DIR, paste0("usage_v2_", Sys.getpid(), ".csv"))
  utils::write.table(row, f, sep = ",", append = file.exists(f),
                     col.names = !file.exists(f), row.names = FALSE)
}
# Provider guard: refuse further calls once a billing/quota error was seen (STOP
# file) or a wrong Gemini model version was served (ABORT file).
guarded_call <- function(provider, fun, ...) {
  if (file.exists(STOP_FILE(provider))) stop("STOP: billing/quota error earlier for ", provider)
  if (provider == "google" && file.exists(ABORT_FILE)) stop("ABORT: wrong Gemini model version served")
  tryCatch(traced_call(fun, ...), error = function(e) {
    if (grepl(BILLING_PAT, conditionMessage(e), ignore.case = TRUE))
      writeLines(conditionMessage(e), STOP_FILE(provider))
    stop(e)
  })
}
anthropic_v4_chat_prod <- anthropic_v4_chat
anthropic_v4_chat <- function(...) {
  a <- list(...); t <- guarded_call("anthropic", anthropic_v4_chat_prod, ...)
  log_usage("anthropic", a$model %||% MODEL_CLAUDE, t$r$usage, t$resolved, doc_of(a[[2]])); t$r
}
oa_chat_json_prod <- oa_chat_json
oa_chat_json <- function(...) {
  a <- list(...); t <- guarded_call("openai", oa_chat_json_prod, ...)
  log_usage("openai", a$model %||% NA_character_, t$r$usage, t$resolved,
            doc_of(a$user_prompt %||% ""), t$r$system_fingerprint %||% NA_character_); t$r
}

# Vertex AI (global endpoint) twin of the production gemini_v4_chat (R/18):
# identical systemInstruction, schema, temperature, maxOutputTokens, thinkingBudget
# and timeout. Only differences: OAuth bearer auth, and contents carry
# role = "user" (Vertex rejects role-less contents; AI Studio defaults to "user").
VERTEX_URL <- paste0("https://aiplatform.googleapis.com/v1/projects/613604909070/locations/global/",
                     "publishers/google/models/%s:generateContent")
VERTEX_KEY <- path.expand("~/.gcp/vertex-key.json")
vertex_token <- function() {
  cred <- get0(".vertex_cred", envir = .GlobalEnv)
  if (is.null(cred)) {
    cred <- gargle::credentials_service_account(
      scopes = "https://www.googleapis.com/auth/cloud-platform", path = VERTEX_KEY)
    assign(".vertex_cred", cred, envir = .GlobalEnv)
  }
  if (!cred$validate()) cred$refresh()
  cred$credentials$access_token
}
gemini_v4_chat_vertex <- function(system_prompt, user_prompt, model = MODEL_GEMINI,
                                  max_output_tokens = 12000L, timeout_s = 180) {
  body <- list(
    systemInstruction = list(parts = list(list(text = system_prompt))),
    contents = list(list(role = "user", parts = list(list(text = user_prompt)))),
    generationConfig = list(
      temperature = 0,
      maxOutputTokens = max_output_tokens,
      responseMimeType = "application/json",
      responseSchema = gemini_v4_schema,
      thinkingConfig = list(thinkingBudget = 0)
    )
  )
  req <- httr::POST(sprintf(VERTEX_URL, model),
                    httr::add_headers(Authorization = paste("Bearer", vertex_token())),
                    content_type_json(),
                    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
                    httr::timeout(timeout_s))
  if (httr::status_code(req) >= 400)
    stop("Gemini API error: ", httr::content(req, "text", encoding = "UTF-8"))
  cont <- httr::content(req, "parsed", type = "application/json")
  txt <- cont$candidates[[1]]$content$parts[[1]]$text
  if (is.null(txt) || !nzchar(txt)) stop("Gemini returned empty text")
  list(text = txt, usage = cont$usageMetadata)
}
gemini_v4_chat_prod <- if (GEMINI_BACKEND == "vertex") gemini_v4_chat_vertex else gemini_v4_chat
gemini_v4_chat <- function(...) {
  a <- list(...); t <- guarded_call("google", gemini_v4_chat_prod, ...)
  log_usage(if (GEMINI_BACKEND == "vertex") "google-vertex" else "google",
            a$model %||% MODEL_GEMINI, t$r$usage, t$resolved, doc_of(a[[2]]))
  if (nzchar(EXPECT_GEMINI) && !identical(t$resolved, EXPECT_GEMINI)) {
    writeLines(paste("served", t$resolved, "expected", EXPECT_GEMINI), ABORT_FILE)
    stop("ABORT: Gemini served ", t$resolved, " instead of ", EXPECT_GEMINI)
  }
  t$r
}

# Retry of individual failed chunks: chunking exactly as in the production
# score_manifesto_* functions, then the production per-chunk scorer + flattener.
CHUNK_FUNS <- list(
  claude    = c(score = "score_chunk_v4_claude", flat = "flatten_parsed_v4_claude"),
  gpt41mini = c(score = "score_chunk_v4_robust", flat = "flatten_parsed_v4"),
  gemini    = c(score = "score_chunk_v4_gemini", flat = "flatten_parsed_v4_gemini"))
score_selected_chunks <- function(doc_id_root, text, country, year, ids, n_expected,
                                  score_fun, flat_fun) {
  est <- tryCatch(estimate_tokens(text), error = function(e) Inf)
  chunks <- if (is.finite(est) && est <= FULL_TEXT_TOKEN_CAP) list(text)
            else as.list(chunk_text(text, CHUNK_TOKENS, OVERLAP_TOKENS))
  if (length(chunks) == 0L) chunks <- list(text)
  if (length(chunks) != n_expected)
    stop("chunking mismatch for ", doc_id_root, ": ", length(chunks), " vs June ", n_expected)
  rows <- list(); spans <- list(); errors <- character()
  for (ci in ids) {
    cdi <- paste0(doc_id_root, "_ch", ci)
    sr <- score_fun(chunks[[ci]], cdi, country, year)
    if (is.null(sr$parsed)) {
      errors <- c(errors, paste0(cdi, ": ", sr$last_error %||% "no_parse"))
      rows[[length(rows) + 1]] <- tibble(doc_id = cdi, doc_id_root = doc_id_root, chunk_id = ci,
                                         schema_version = NA_character_,
                                         scoring_error = sr$last_error %||% "no_parse")
      next
    }
    f <- flat_fun(sr$parsed, cdi, doc_id_root, ci, chunks[[ci]])
    if (!is.null(sr$raw_json)) f$row$raw_tool_json <- sr$raw_json   # Claude, patched R/15b
    rows[[length(rows) + 1]] <- f$row; spans[[length(spans) + 1]] <- f$spans
  }
  list(raw = bind_rows(rows), spans = bind_rows(spans), errors = errors, doc_id_root = doc_id_root)
}

SCORER <- list(
  claude    = list(fn = "score_manifesto_v4_claude",
                   globals = c("MODEL_CLAUDE", "score_manifesto_v4_claude", "score_chunk_v4_claude",
                               "decode_dim_string", "flatten_parsed_v4_claude", "anthropic_v4_chat", "anthropic_v4_chat_prod",
                               "anthropic_v4_schema")),
  gpt41mini = list(fn = "score_manifesto_v4_robust",
                   globals = c("MODEL", "score_manifesto_v4_robust", "score_chunk_v4_robust",
                               "flatten_parsed_v4", "oa_chat_json", "oa_chat_json_prod")),
  gemini    = list(fn = "score_manifesto_v4_gemini",
                   globals = c("MODEL_GEMINI", "score_manifesto_v4_gemini", "score_chunk_v4_gemini",
                               "flatten_parsed_v4_gemini", "gemini_v4_chat", "gemini_v4_chat_prod",
                               "gemini_v4_schema", "to_gemini_schema", "gemini_v4_chat_vertex",
                               "vertex_token", "VERTEX_URL", "VERTEX_KEY", "GEMINI_BACKEND",
                               "EXPECT_GEMINI", "ABORT_FILE"))
)
COMMON_GLOBALS <- c("MAX_RETRIES_PER_CHUNK", "RETRY_BASE_SLEEP_SEC", "FULL_TEXT_TOKEN_CAP",
                    "CHUNK_TOKENS", "OVERLAP_TOKENS", "V4_DIMS", "system_prompt_v4_safe",
                    "user_prompt_v4", "poplib_v4_schema", "estimate_tokens", "chunk_text",
                    "truncate_tokens", "%||%", "log_usage", "USAGE_DIR", "ensure_model_trace",
                    "traced_call", "doc_of", "guarded_call", "STOP_FILE", "BILLING_PAT",
                    "UPD_DIR", "score_selected_chunks")

score_rows <- function(rows, which, workers) {
  sc <- SCORER[[which]]
  f  <- get(sc$fn)
  cf <- get(CHUNK_FUNS[[which]][["score"]]); ff <- get(CHUNK_FUNS[[which]][["flat"]])
  plan(multisession, workers = workers)
  on.exit(plan(sequential), add = TRUE)
  future_map(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    tryCatch(if ("chunk_ids" %in% names(r))
               score_selected_chunks(r$doc_id, r$text, r$countryname, year(r$date),
                                     r$chunk_ids[[1]], r$n_chunks_june, cf, ff)
             else f(r$doc_id, r$text, r$countryname, year(r$date)),
             error = function(e) list(raw = tibble(doc_id = paste0(r$doc_id, "_ch1"),
                                                   doc_id_root = r$doc_id, chunk_id = 1L,
                                                   schema_version = NA_character_,
                                                   scoring_error = conditionMessage(e)),
                                      spans = tibble(), errors = conditionMessage(e),
                                      doc_id_root = r$doc_id))
  }, .options = furrr_options(
    globals = c("rows", "f", "cf", "ff", sc$globals, COMMON_GLOBALS),
    packages = c("tibble", "dplyr", "purrr", "tidyr", "httr", "jsonlite", "rtiktoken", "lubridate"),
    seed = TRUE))
}

# Keep each manifesto's rows from its LATEST attempt (resume re-scores failures
# in later batches); a manifesto is ok only if all its chunks parsed.
latest_attempt <- function(raw) {
  if (!nrow(raw)) return(raw)
  raw |> group_by(doc_id_root) |> filter(.batch == max(.batch)) |> ungroup()
}
ok_docs_of <- function(raw) {
  raw <- latest_attempt(raw)
  if (!nrow(raw)) return(character())
  if (!"scoring_error" %in% names(raw)) raw$scoring_error <- NA_character_
  raw |> group_by(doc_id_root) |>
    summarise(ok = all(is.na(scoring_error) & !is.na(schema_version)), .groups = "drop") |>
    filter(ok) |> pull(doc_id_root)
}

# ---- Data ----

if (MODE != "anchor") {
  upd <- read_parquet(UPD_FILE) |> mutate(date = as.Date(date))
  # Only text-bearing changes need (re-)scoring; metadata-only changes are re-scored
  # only if a prompt input (countryname, election year) changed.
  meta_chg <- read_parquet(file.path(UPD_DIR, "meta_changes.parquet"))
  prompt_meta_chg <- meta_chg |>
    filter(variable == "countryname" |
             (variable == "date" & substr(old, 1, 4) != substr(new, 1, 4)) |
             (variable == "date_num" & substr(old, 1, 4) != substr(new, 1, 4))) |>
    distinct(doc_id) |> pull(doc_id)
  if (TARGET == "update") {
    to_score_all <- upd |>
      filter(update_status %in% c("new", "changed_text", "recoded_changed_text") |
               doc_id %in% prompt_meta_chg) |>
      # scope A (team-lead 2026-10-07): normalisation-only text changes keep old scores
      filter(!(MODE != "estimate" & text_change_type == "normalisation_only")) |>
      arrange(doc_id)
  } else if (TARGET == "norm") {
    # Phase 1 step 2: the 131 normalisation-only changed manifestos, 2026-1 text
    to_score_all <- upd |>
      filter(update_status == "changed_text", text_change_type == "normalisation_only") |>
      arrange(doc_id)
  } else if (TARGET == "retry") {
    # Phase 1 step 3: chunks that failed in the June run (error rows, or chunk ids
    # missing for this model relative to the June chunking). Manifestos whose text
    # changed in 2026-1 are re-scored in full by the update/norm targets instead.
    june_raw <- c(claude = "data/CMP/poplib_v4_claude_full_raw.parquet",
                  gpt41mini = "data/CMP/poplib_v4_gpt41mini_raw.parquet",
                  gemini = "data/CMP/poplib_v4_gemini_flash_latest_raw.parquet")
    june_chunks <- read_parquet(june_raw[["claude"]], col_select = c("doc_id", "doc_id_root", "chunk_id"))
    jr <- read_parquet(june_raw[[WHICH]], col_select = c("doc_id", "doc_id_root", "chunk_id",
                                                        "schema_version", "scoring_error"))
    ok_chunks <- jr |> filter(is.na(scoring_error), !is.na(schema_version)) |> distinct(doc_id)
    failed <- june_chunks |> anti_join(ok_chunks, by = "doc_id")
    status <- read_parquet(file.path(UPD_DIR, "update_status.parquet"))
    rescored_elsewhere <- status |> filter(update_status != "unchanged",
                                           update_status != "changed_meta") |> pull(doc_id)
    june_text <- read_parquet("data/CMP/full_data.parquet") |>
      filter(!is.na(manifesto_id), !is.na(text)) |> mutate(date = as.Date(date)) |>
      distinct(doc_id, .keep_all = TRUE)
    to_score_all <- failed |>
      filter(!doc_id_root %in% rescored_elsewhere) |>
      group_by(doc_id = doc_id_root) |> summarise(chunk_ids = list(sort(chunk_id)), .groups = "drop") |>
      left_join(june_chunks |> count(doc_id = doc_id_root, name = "n_chunks_june"), by = "doc_id") |>
      inner_join(june_text |> select(doc_id, countryname, date, text), by = "doc_id") |>
      arrange(doc_id)
    message(sprintf("[retry %s] %d June chunks failed/missing; %d chunks in %d manifestos to retry here (rest re-scored with 2026-1 text elsewhere)",
                    WHICH, nrow(failed), sum(lengths(to_score_all$chunk_ids)), nrow(to_score_all)))
  } else if (TARGET == "parseloss") {
    # 2026-10-24: chunks of data/CMP/v2026a/raw_claude.parquet with >= 1 missing evidence_status
    # (parse losses). Text = corpus 2026-1, cleaned as in R/30; for june2026 rows this is
    # byte-identical to the text they were scored on (checked below), for norm/update rows it
    # is the text itself. Chunking must reproduce the merged chunk count of each manifesto.
    stopifnot("parseloss is a Claude-only target" = WHICH %in% c("claude", NA))
    source("R/2_Helpers.R")
    mr <- read_parquet("data/CMP/v2026a/raw_claude.parquet")
    st_cols <- grep("_status$", names(mr), value = TRUE)
    lost <- mr[rowSums(is.na(mr[st_cols])) > 0, c("doc_id", "doc_id_root", "chunk_id", "corpus_version")]
    meta26 <- read_parquet("data/CMP/full_data_2026a_meta.parquet") |> filter(has_text) |>
      distinct(doc_id, .keep_all = TRUE) |> transmute(doc_id, countryname, date = as.Date(date))
    txt26 <- read_parquet(file.path(UPD_DIR, "corpus_texts_20260904101355.parquet"), col_select = c("doc_id", "text_raw")) |>
      filter(doc_id %in% lost$doc_id_root) |> distinct(doc_id, .keep_all = TRUE) |>
      transmute(doc_id, text = map_chr(text_raw, clean_manifesto_text))
    june_txt <- read_parquet("data/CMP/full_data.parquet", col_select = c("doc_id", "text")) |>
      filter(!is.na(text)) |> distinct(doc_id, .keep_all = TRUE)
    j_docs <- lost |> filter(startsWith(corpus_version, "2025-1")) |> distinct(doc_id_root) |> pull()
    chk <- inner_join(txt26 |> filter(doc_id %in% j_docs), june_txt, by = "doc_id", suffix = c("", "_june"))
    stopifnot("2026-1 text must equal the June text for june2026 rows" = all(chk$text == chk$text_june))
    to_score_all <- lost |> group_by(doc_id = doc_id_root) |>
      summarise(chunk_ids = list(sort(chunk_id)), .groups = "drop") |>
      left_join(mr |> count(doc_id = doc_id_root, name = "n_chunks_june"), by = "doc_id") |>
      inner_join(meta26, by = "doc_id") |> inner_join(txt26, by = "doc_id") |> arrange(doc_id)
    stopifnot(sum(lengths(to_score_all$chunk_ids)) == nrow(lost))
    message(sprintf("[parseloss] %d chunks in %d manifestos (%d with june2026 text checked identical)",
                    nrow(lost), nrow(to_score_all), nrow(chk)))
  }
}

# ============================================================
# MODE: estimate
# ============================================================
if (MODE %in% c("estimate", "score") && TARGET == "parseloss") {
  # chunk-exact estimate: only the selected chunks are sent
  sys_tok <- estimate_tokens(system_prompt_v4_safe("Germany", 2025L))
  schema_tok <- estimate_tokens(as.character(toJSON(poplib_v4_schema, auto_unbox = TRUE)))
  ch_tok <- map2_dbl(to_score_all$text, to_score_all$chunk_ids, function(t, ids) {
    est <- estimate_tokens(t)
    chunks <- if (est <= FULL_TEXT_TOKEN_CAP) list(t) else as.list(chunk_text(t, CHUNK_TOKENS, OVERLAP_TOKENS))
    sum(map_dbl(chunks[ids], estimate_tokens))
  })
  n_calls <- sum(lengths(to_score_all$chunk_ids))
  # Claude reports ~1.35x the o200k token count; output ~2,000 tokens per call (Phase 1 logs)
  usd <- ((sum(ch_tok) + n_calls * (sys_tok + schema_tok + 600)) * 1.35 * 3 + n_calls * 2000 * 15) / 1e6
  cap <- as.numeric(Sys.getenv("PARSELOSS_CAP_USD", "45"))
  message(sprintf("[parseloss] estimate: %d calls, %.2fM input tokens (o200k), USD %.2f (cap %.0f)",
                  n_calls, sum(ch_tok) / 1e6, usd, cap))
  if (usd > cap) stop("estimate exceeds the cap; not scoring")
  if (MODE == "estimate") quit(save = "no")
}
if (MODE == "estimate") {
  sys_tok <- estimate_tokens(system_prompt_v4_safe("Germany", 2025L))
  schema_tok <- estimate_tokens(as.character(toJSON(poplib_v4_schema, auto_unbox = TRUE)))
  chunks <- to_score_all |>
    mutate(tok = map_dbl(text, ~ tryCatch(estimate_tokens(.x), error = function(e) NA_real_)),
           n_chunks = map_int(seq_along(text), function(i) {
             if (!is.na(tok[i]) && tok[i] <= FULL_TEXT_TOKEN_CAP) 1L
             else length(chunk_text(text[i], CHUNK_TOKENS, OVERLAP_TOKENS))
           }),
           chunk_in_tok = tok + (n_chunks - 1) * OVERLAP_TOKENS)
  # per-call output tokens + input overhead: measured in the anchor runs if available
  usage <- list.files(USAGE_DIR, full.names = TRUE) |> map_dfr(read_csv, show_col_types = FALSE,
                                                                col_types = cols(.default = "c")) |>
    mutate(across(c(in_tok, out_tok, thought_tok), as.numeric))
  out_per_call <- function(prov, default) {
    u <- usage |> filter(provider == prov, model != "gpt-4o-mini-2024-07-18")
    if (nrow(u) >= 5) mean(u$out_tok + u$thought_tok, na.rm = TRUE) else default
  }
  PRICE <- tribble(                       # USD per 1M tokens (in, out)
    ~model,       ~p_in, ~p_out, ~overhead, ~out_call,
    "claude",      3.00, 15.00, sys_tok + schema_tok + 600, out_per_call("anthropic", 2000),
    "gpt41mini",   0.40,  1.60, sys_tok + schema_tok + 200, out_per_call("openai", 1800),
    "gemini",      0.30,  2.50, sys_tok + schema_tok + 200, out_per_call("google", 1500)
  )
  # Gemini price for the currently served model may differ (see report);
  # override via env GEMINI_PRICE_IN / GEMINI_PRICE_OUT
  if (nzchar(Sys.getenv("GEMINI_PRICE_IN")))
    PRICE$p_in[PRICE$model == "gemini"]  <- as.numeric(Sys.getenv("GEMINI_PRICE_IN"))
  if (nzchar(Sys.getenv("GEMINI_PRICE_OUT")))
    PRICE$p_out[PRICE$model == "gemini"] <- as.numeric(Sys.getenv("GEMINI_PRICE_OUT"))
  n_calls <- sum(chunks$n_chunks); in_tok <- sum(chunks$chunk_in_tok, na.rm = TRUE)
  est <- PRICE |> mutate(
    n_manifestos = nrow(chunks), n_calls = n_calls,
    in_tok = in_tok + n_calls * overhead, out_tok = n_calls * out_call,
    scoring_usd = (in_tok * p_in + out_tok * p_out) / 1e6,
    # judge: ~13 spans/call, ~40% go to judge, ~150 in / 5 out tok each on gpt-4o-mini
    judge_usd = n_calls * 13 * 0.4 * (150 * 0.15 + 5 * 0.60) / 1e6,
    total_usd = scoring_usd + judge_usd)
  # breakdown by update type (per-doc share of calls / input tokens)
  tc <- read_parquet(file.path(UPD_DIR, "text_changes.parquet")) |> select(doc_id, norm_only)
  by_doc <- chunks |> left_join(tc, by = "doc_id") |>
    mutate(group = case_when(update_status == "changed_text" & norm_only ~ "changed_text_normalisation_only",
                             update_status == "changed_text" ~ "changed_text_substantive",
                             TRUE ~ update_status)) |>
    select(doc_id, countryname, date, group, tok, n_chunks, chunk_in_tok)
  by_group <- tidyr::crossing(by_doc, PRICE) |>
    mutate(usd = ((chunk_in_tok + n_chunks * overhead) * p_in + n_chunks * out_call * p_out) / 1e6) |>
    group_by(group, model) |> summarise(n_docs = n(), n_calls = sum(n_chunks), usd = sum(usd), .groups = "drop") |>
    pivot_wider(names_from = model, values_from = usd) |>
    mutate(total_usd = claude + gpt41mini + gemini)
  print(by_group, width = 200)
  write_parquet(by_doc, file.path(UPD_DIR, "cost_estimate_by_doc.parquet"))
  print(est, width = 200)
  cat(sprintf("\nTOTAL estimated: USD %.2f (%d manifestos, %d calls/model, %.1fM input tok/model)\n",
              sum(est$total_usd), nrow(chunks), n_calls, in_tok / 1e6))
  write_parquet(est |> mutate(estimated_at = Sys.time()), file.path(UPD_DIR, "cost_estimate.parquet"))
  quit(save = "no")
}

# ============================================================
# MODE: anchor  (re-score already-scored manifestos, compare with original)
# ============================================================
if (MODE == "anchor") {
  stopifnot(WHICH %in% names(SCORER))
  orig_raw_path <- c(claude = "data/CMP/poplib_v4_claude_full_raw.parquet",
                     gpt41mini = "data/CMP/poplib_v4_gpt41mini_raw.parquet",
                     gemini = "data/CMP/poplib_v4_gemini_flash_latest_raw.parquet")
  base <- read_parquet("website/data/manifesto_scores.parquet")
  gem_ok <- read_parquet(orig_raw_path["gemini"]) |> distinct(doc_id_root) |> pull()
  old <- read_parquet("data/CMP/full_data.parquet") |>
    filter(!is.na(manifesto_id), !is.na(text)) |> mutate(date = as.Date(date)) |>
    distinct(doc_id, .keep_all = TRUE) |>
    filter(year(date) >= 2010, token_count < 6000, doc_id %in% gem_ok) |>   # single-chunk, cheap
    inner_join(base |> select(doc_id, populism_overall), by = "doc_id")
  set.seed(ANCHOR_SEED)
  anchors <- bind_rows(old |> filter(populism_overall >= 5) |> slice_sample(n = ANCHOR_N / 2),
                       old |> filter(populism_overall < 3)  |> slice_sample(n = ANCHOR_N / 2))
  res <- score_rows(anchors, WHICH, WORKERS[[WHICH]])
  new_raw <- bind_rows(map(res, "raw"))
  orig <- read_parquet(orig_raw_path[WHICH]) |> filter(doc_id %in% new_raw$doc_id)
  if ("scoring_error" %in% names(orig)) orig <- orig |> filter(is.na(scoring_error), !is.na(schema_version))
  cmp <- map_dfr(V4_DIMS, function(d) {
    sc <- paste0(d, "_score"); st <- paste0(d, "_status")
    inner_join(orig |> select(doc_id, s_old = all_of(sc), st_old = all_of(st)),
               new_raw |> select(doc_id, s_new = any_of(sc), st_new = any_of(st)), by = "doc_id") |>
      mutate(dim = d)
  })
  summ <- cmp |> summarise(
    model = MODEL_ID[[WHICH]], n_docs = n_distinct(doc_id), n_cells = n(),
    status_agree = mean(st_old == st_new, na.rm = TRUE),
    score_exact  = mean(s_old == s_new, na.rm = TRUE),
    score_mad    = mean(abs(s_old - s_new), na.rm = TRUE),
    score_cor    = cor(s_old, s_new, use = "complete.obs"),
    mean_old     = mean(s_old, na.rm = TRUE), mean_new = mean(s_new, na.rm = TRUE))
  print(summ, width = 200)
  tag <- if (WHICH == "gemini") paste0("_", if (GEMINI_BACKEND == "vertex") "vertex_" else "", MODEL_GEMINI) else ""
  # Gemini: also compare with the Oct-7 AI-Studio gemini-3.5-flash anchor re-score
  prev <- file.path(UPD_DIR, "anchor_gemini_gemini-3.5-flash.parquet")
  if (WHICH == "gemini" && GEMINI_BACKEND == "vertex" && file.exists(prev) &&
      any(read_parquet(prev)$doc_id %in% cmp$doc_id)) {
    pv <- read_parquet(prev) |> select(doc_id, dim, s_prev = s_new, st_prev = st_new)
    vs_prev <- cmp |> inner_join(pv, by = c("doc_id", "dim")) |> summarise(
      comparison = "vertex vs Oct-7 AI-Studio 3.5", n_cells = n(),
      status_agree = mean(st_prev == st_new, na.rm = TRUE),
      score_exact = mean(s_prev == s_new, na.rm = TRUE),
      score_mad = mean(abs(s_prev - s_new), na.rm = TRUE),
      score_cor = cor(s_prev, s_new, use = "complete.obs"))
    print(vs_prev, width = 200)
  }
  write_parquet(cmp |> mutate(model = MODEL_ID[[WHICH]]), file.path(UPD_DIR, paste0("anchor_", WHICH, tag,
                         if (ANCHOR_SEED != 2026L) paste0("_seed", ANCHOR_SEED, "_n", ANCHOR_N) else "", ".parquet")))
  quit(save = "no")
}

# ============================================================
# MODE: score  (checkpointed, resumable, stops on persistent quota/billing errors)
# ============================================================
ckpt_dir <- file.path(UPD_DIR, paste0("checkpoints_", WHICH, SUFFIX))
read_ckpts <- function() {
  fs <- list.files(ckpt_dir, pattern = "^batch_\\d+\\.RData$", full.names = TRUE)
  imap(fs, function(f, i) {
    e <- new.env()
    tryCatch(load(f, envir = e), error = function(err) stop("unreadable checkpoint: ", f))
    br <- e$batch_result
    if (nrow(br$raw))   br$raw$.batch   <- i
    if (nrow(br$spans)) br$spans$.batch <- i
    br
  })
}

if (MODE == "score") {
  stopifnot(WHICH %in% names(SCORER))
  ensure_dir(ckpt_dir)
  prev <- read_ckpts()
  done <- ok_docs_of(bind_rows(map(prev, "raw")))
  todo <- to_score_all |> filter(!doc_id %in% done)
  message(sprintf("[%s | %s] to score: %d (already ok: %d)", WHICH, MODEL_ID[[WHICH]], nrow(todo), length(done)))
  b0 <- length(prev)
  n_b <- ceiling(nrow(todo) / CHECKPOINT_EVERY_UPD)
  for (b in seq_len(n_b)) {
    rows <- todo[((b - 1) * CHECKPOINT_EVERY_UPD + 1):min(b * CHECKPOINT_EVERY_UPD, nrow(todo)), ]
    t0 <- Sys.time()
    res <- score_rows(rows, WHICH, WORKERS[[WHICH]])
    batch_result <- list(raw = bind_rows(map(res, "raw")), spans = bind_rows(map(res, "spans")),
                         errors = unlist(map(res, "errors")), timestamp = Sys.time(),
                         model = MODEL_ID[[WHICH]])
    # atomic checkpoint (lesson 2026-10-13: a kill during save() left a truncated
    # file): write to a temp file in the same directory, then rename
    ck_final <- sprintf("%s/batch_%04d.RData", ckpt_dir, b0 + b)
    ck_tmp   <- paste0(ck_final, ".tmp")
    save(batch_result, file = ck_tmp)
    stopifnot(file.rename(ck_tmp, ck_final))
    n_err <- length(batch_result$errors)
    message(sprintf("  batch %d/%d: %d docs, %d chunk rows, %d errors, %.1f min",
                    b, n_b, nrow(rows), nrow(batch_result$raw), n_err,
                    as.numeric(Sys.time() - t0, units = "mins")))
    if (file.exists(ABORT_FILE) && WHICH == "gemini") {
      message("  ABORT: ", readLines(ABORT_FILE)[1]); quit(save = "no", status = 4)
    }
    prov <- c(claude = "anthropic", gpt41mini = "openai", gemini = "google")[[WHICH]]
    quota <- sum(grepl(BILLING_PAT, batch_result$errors, ignore.case = TRUE))
    if (file.exists(STOP_FILE(prov)) || quota > 0) {
      message("  STOP: persistent quota/billing errors (", quota, "). Re-run later to resume.")
      message("  first error: ", substr(batch_result$errors[1], 1, 300))
      quit(save = "no", status = 3)
    }
  }
  MODE <- "finalize"
}

# ============================================================
# MODE: finalize  (combine, R/12 validation, production aggregation)
# ============================================================
if (MODE == "finalize") {
  stopifnot(WHICH %in% names(SCORER))
  ck <- read_ckpts()
  raw_all <- bind_rows(map(ck, "raw")); spans_all <- bind_rows(map(ck, "spans"))
  ok <- ok_docs_of(raw_all)
  # keep the LAST successful attempt per manifesto (resume may have re-scored failures)
  last_b <- latest_attempt(raw_all) |> distinct(doc_id_root, .batch)
  raw_all <- raw_all |> semi_join(last_b, by = c("doc_id_root", ".batch")) |>
    filter(doc_id_root %in% ok) |> select(-.batch)
  spans_all <- spans_all |> semi_join(last_b, by = c("doc_id_root", ".batch")) |>
    filter(doc_id_root %in% ok) |> select(-.batch)
  raw_all$model_id <- unique(map_chr(ck, ~ .x$model %||% NA_character_)) |> paste(collapse = ";")
  # resolved model string per chunk (last logged call for that chunk id)
  prov <- c(claude = "anthropic", gpt41mini = "openai", gemini = "google")[[WHICH]]
  mv <- list.files(USAGE_DIR, "^usage_v2_", full.names = TRUE) |>
    map_dfr(~ read_csv(.x, col_types = cols(.default = "c"))) |>
    filter(provider %in% c(prov, paste0(prov, "-vertex")), model == MODEL_ID[[WHICH]], !is.na(doc_id)) |>
    arrange(ts) |> group_by(doc_id) |> slice_tail(n = 1) |> ungroup() |>
    select(doc_id, resolved_model)
  raw_all <- raw_all |> left_join(mv, by = "doc_id")
  message("  resolved model versions: ",
          paste(names(table(raw_all$resolved_model, useNA = "ifany")),
                table(raw_all$resolved_model, useNA = "ifany"), sep = "=", collapse = " | "))
  write_parquet(raw_all, file.path(UPD_DIR, paste0("poplib_v4_", WHICH, SUFFIX, "_raw.parquet")))
  message(sprintf("[%s] ok manifestos: %d / %d targeted | chunk rows: %d | spans: %d",
                  WHICH, length(ok), nrow(to_score_all), nrow(raw_all), nrow(spans_all)))

  # R/12 span validation, called exactly as in R/15b/16/18
  chunk_text_lookup <- setNames(spans_all$chunk_text, spans_all$doc_id)
  sp_for_check <- system_prompt_v4_safe("X", 2020L)
  validated <- validate_spans_table_with_judge(
    spans_all |> select(doc_id, doc_id_root, dim_name, quote, context),
    chunk_text_lookup, sp_for_check, TOPIC_KEYWORDS_V4, use_judge = TRUE, verbose = TRUE)
  write_parquet(validated, file.path(UPD_DIR, paste0("poplib_v4_", WHICH, SUFFIX, "_validated_spans.parquet")))

  # 2026-10-26 (chain-review #4): the per-model *_agg.parquet (old R/15b/R/18 rule: verified|mixed
  # & quality_verified, plain mean; empty for the *_overall dims) is no longer written. The only
  # aggregation path is R/1_Aggregation.R; existing *_agg files are deprecated and read by nothing.
  message("  saved raw + validated spans for ", WHICH)
}
