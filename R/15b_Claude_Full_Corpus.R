# ============================================================
# 15_CrossLLM_v4_Claude.R   (BRANCH: pipeline-v4-safe)
#
# CROSS-MODEL ROBUSTNESS CHECK FOR THE V4 SAFE PIPELINE
#
# Re-scores a stratified 500-manifesto subset under Claude Sonnet 4.6
# using the SAME v4 prompt and schema as the OpenAI primary run
# (R/14). Computes pairwise agreement (Pearson r, Spearman ρ, MAD)
# between gpt-4o-mini-v4 and Claude-v4 scores per dimension.
#
# Why 500: gives statistical power for per-country and per-parfam
# agreement estimates at ~$50 — vs ~$250 for full-corpus Claude.
#
# Stratification: country × populism quartile (from v1 production
# scores), set.seed(42) for reproducibility. Manifestos with v4
# results from R/14 are required (this script joins on doc_id).
#
# Robustness features (mirroring R/14):
#   - Per-chunk retry with exponential backoff
#   - Per-batch checkpoints (every 50 manifestos)
#   - Resume capability
#   - Parallel via furrr (4 workers — Anthropic rate-limit safe)
#   - LLM judge + multilingual regex for span quality (same R/12)
#
# Outputs:
#   data/CMP/v4_claude_full_checkpoints/batch_NNNN.RData
#   data/CMP/poplib_v4_claude_full_raw.parquet
#   data/CMP/poplib_v4_claude_full_validated_spans.parquet
#   data/CMP/poplib_v4_claude_full_agg.parquet
#   data/CMP/poplib_v4_cross_agreement.RData      (gpt-4o-mini × Claude)
#
# Cost: ~$50 (500 × ~1.5 chunks × ~$0.07/chunk on Sonnet 4.6).
# Wall time: ~2-3h with 4 parallel workers.
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(readxl); library(lubridate)
  library(jsonlite); library(httr); library(furrr)
})
source("R/1_Functions.R")
source("R/3c_Populism_v4_safe.R")
source("R/12_Validate_Span_Quality.R")

if (!nzchar(Sys.getenv("OPENAI_API_KEY")))    stop("OPENAI_API_KEY missing")
if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) stop("ANTHROPIC_API_KEY missing")

message("=== Cross-LLM v4: Claude Sonnet 4.6 subset run ===")
message("Timestamp: ", Sys.time())

# ============================================================
# Configuration
# ============================================================

N_WORKERS              <- 3L     # Anthropic rate limit; 3 workers conservative for parallel run alongside R/14
N_SUBSET               <- 9999L     # IGNORED in 15b — full corpus mode
CHECKPOINT_EVERY       <- 50L
CHECKPOINT_DIR         <- "data/CMP/v4_claude_full_checkpoints/"
MAX_RETRIES_PER_CHUNK  <- 5L
RETRY_BASE_SLEEP_SEC   <- 3
FULL_TEXT_TOKEN_CAP    <- 18000L
CHUNK_TOKENS           <- 20000L
OVERLAP_TOKENS         <- 2000L
MODEL_CLAUDE           <- "claude-sonnet-4-6"
SAMPLE_SEED            <- 42L

ensure_dir(CHECKPOINT_DIR)

V4_DIMS <- c(
  "pop_anti_elitism","pop_people_centrism","pop_manichean","populism_overall",
  "pop_ideology_left","pop_ideology_right","pop_ideology_centrist","pop_ideology_overall",
  "lib_political","lib_social","lib_economic","lib_financial_market","liberalism_overall"
)

# ============================================================
# Anthropic Messages API wrapper for v4 (uses tool_use)
# Schema differences from gpt-4o: Claude doesn't accept
# c("integer","null") in input_schema — use only "integer" and
# rely on the prompt's no_evidence rule to produce null. Claude
# can still emit null in tool_use arguments even if schema says
# "integer" (it leaves the field undefined).
# ============================================================

# Strip nullable types from the schema for Claude
anthropic_v4_schema <- (function() {
  s <- poplib_v4_schema
  # Walk and rewrite c("integer","null") -> "integer"
  walk_strip_null <- function(node) {
    if (is.list(node)) {
      if (!is.null(node$type) &&
          is.character(node$type) &&
          all(node$type %in% c("integer", "null"))) {
        node$type <- "integer"
      }
      for (nm in names(node)) node[[nm]] <- walk_strip_null(node[[nm]])
    }
    node
  }
  walk_strip_null(s)
})()

anthropic_v4_chat <- function(system_prompt, user_prompt,
                              model = MODEL_CLAUDE,
                              max_output_tokens = 5000,
                              timeout_s = 180) {
  key <- Sys.getenv("ANTHROPIC_API_KEY")
  tool <- list(
    name        = "submit_poplib_v4",
    description = "Submit populism/liberalism scores under the v4 schema with per-dimension evidence_status. Use null for score when evidence_status is no_evidence.",
    input_schema = anthropic_v4_schema
  )
  body <- list(
    model       = model,
    max_tokens  = max_output_tokens,
    temperature = 0,
    system      = system_prompt,
    tools       = list(tool),
    tool_choice = list(type = "tool", name = "submit_poplib_v4"),
    messages    = list(list(role = "user", content = user_prompt))
  )
  req <- httr::POST("https://api.anthropic.com/v1/messages",
    httr::add_headers("x-api-key" = key, "anthropic-version" = "2023-06-01",
                      "content-type" = "application/json"),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    encode = "raw", httr::timeout(timeout_s))
  if (httr::status_code(req) >= 400)
    stop("Anthropic API error: ", httr::content(req, "text", encoding = "UTF-8"))
  cont <- httr::content(req, "parsed", type = "application/json")
  tu <- NULL
  for (b in cont$content) if (identical(b$type, "tool_use")) { tu <- b; break }
  if (is.null(tu)) stop("Claude did not return a tool_use block.")
  # PATCH 2026-10-24: also return the raw tool_use input as JSON text, so a response can be
  # re-parsed later (363 June/Oct chunks were lost to unparsed dimensions; nothing was stored)
  raw_json <- as.character(jsonlite::toJSON(tu$input, auto_unbox = TRUE, null = "null", digits = NA))
  list(parsed = tu$input, usage = cont$usage, raw_json = raw_json)
}

# ============================================================
# Robust per-chunk scoring + per-manifesto wrapper (parallel-safe)
# ============================================================

score_chunk_v4_claude <- function(chunk_text, doc_id_in, country, year,
                                  max_retries = MAX_RETRIES_PER_CHUNK,
                                  base_sleep = RETRY_BASE_SLEEP_SEC) {
  sysp <- system_prompt_v4_safe(country, year)
  usp  <- user_prompt_v4(chunk_text, doc_id_in)
  for (attempt in seq_len(max_retries)) {
    res <- tryCatch(
      anthropic_v4_chat(sysp, usp, model = MODEL_CLAUDE),
      error = function(e) list(parsed = NULL, error = conditionMessage(e))
    )
    if (!is.null(res$parsed)) return(list(parsed = res$parsed, attempts = attempt, raw_json = res$raw_json))
    Sys.sleep(base_sleep * (2^(attempt - 1)))
  }
  list(parsed = NULL, attempts = max_retries, last_error = res$error %||% "unknown")
}

# PATCH 2026-10-24 (refactored 2026-10-26): decode a dimension object that Claude returned as a
# JSON-encoded string. jsonlite if the string is valid JSON; otherwise a deterministic field
# extraction (score, evidence_status, quote up to the "context" key, context up to the closing
# braces), used when the quote or context contains unescaped quotation marks. Returns NULL if
# `obj` is not such a string or no evidence_status can be found. `lenient_only = TRUE` skips
# jsonlite (used by R/36 to test the extractor against jsonlite on valid strings).
decode_dim_string <- function(obj, lenient_only = FALSE) {
  if (!(is.character(obj) && length(obj) == 1L && !is.na(obj) && grepl("^\\s*\\{", obj))) return(NULL)
  dec <- if (lenient_only) NULL else tryCatch(jsonlite::fromJSON(obj, simplifyVector = FALSE), error = function(e) NULL)
  if (is.list(dec)) return(dec)
  grab <- function(pat) { m <- regmatches(obj, regexec(pat, obj, perl = TRUE))[[1]]; if (length(m) > 1) m[2] else NA_character_ }
  unesc <- function(x) if (is.na(x)) x else gsub('\\\\(["\\\\/])', "\\1", x, perl = TRUE)
  sc  <- grab('"score"\\s*:\\s*(null|-?[0-9]+(?:\\.[0-9]+)?)')
  stt <- grab('"evidence_status"\\s*:\\s*"(verified|mixed|no_evidence)"')
  q   <- grab('"quote"\\s*:\\s*"(.*?)"\\s*,\\s*"context"')
  if (is.na(q)) q <- grab('"quote"\\s*:\\s*"(.*?)"\\s*\\}')
  cx  <- grab('"context"\\s*:\\s*"(.*)"\\s*\\}\\s*(?:,[^{}]*)?\\}\\s*$')
  if (is.na(stt)) return(NULL)
  list(score = if (is.na(sc) || sc == "null") NULL else as.numeric(sc), evidence_status = stt,
       span = list(quote = if (is.na(q)) "" else unesc(q), context = if (is.na(cx)) "" else unesc(cx)))
}

flatten_parsed_v4_claude <- function(parsed, chunk_doc_id, doc_id_root,
                                     chunk_id, chunk_text) {
  row <- tibble(
    doc_id         = chunk_doc_id,
    doc_id_root    = doc_id_root,
    chunk_id       = chunk_id,
    schema_version = parsed$schema_version %||% NA_character_
  )
  # PATCH 2026-10-24: Claude sometimes returns a dimension object as a JSON-encoded STRING
  # ("{\"score\": 5, ...}"), typically when the quote or context contains quotation marks,
  # which it then leaves unescaped, so the string is not valid JSON. The branches below sent
  # such dims to "unknown" (all fields NA), which lost 703 chunk-dims in the June/Oct runs.
  # Decode them first: jsonlite if the string is valid JSON, otherwise a deterministic field
  # extraction (score, evidence_status, quote up to the "context" key, context up to the closing
  # braces). Repaired dims are listed in parse_repaired.
  repaired <- character()
  for (d in V4_DIMS) {
    dec <- decode_dim_string(parsed[[d]])
    if (!is.null(dec)) { parsed[[d]] <- dec; repaired <- c(repaired, d) }
  }
  # DEFENSIVE: Claude sometimes returns a dimension as a flat integer
  # rather than the nested {score, evidence_status, span} object that the
  # schema requests. Handle both shapes gracefully.
  for (d in V4_DIMS) {
    obj <- parsed[[d]]
    if (is.list(obj)) {
      # Expected nested form
      row[[paste0(d, "_score")]]   <- if (is.null(obj$score)) NA_real_ else as.numeric(obj$score)
      row[[paste0(d, "_status")]]  <- as.character(obj$evidence_status %||% NA)
      row[[paste0(d, "_quote")]]   <- as.character(obj$span$quote   %||% NA)
      row[[paste0(d, "_context")]] <- as.character(obj$span$context %||% NA)
    } else if (length(obj) > 0L &&
               !is.na(suppressWarnings(as.numeric(obj[1])))) {
      # Flat-integer response — assume "verified" since Claude gave a score
      row[[paste0(d, "_score")]]   <- suppressWarnings(as.numeric(obj[1]))
      row[[paste0(d, "_status")]]  <- "verified"
      row[[paste0(d, "_quote")]]   <- NA_character_
      row[[paste0(d, "_context")]] <- NA_character_
    } else {
      # Unknown / missing
      row[[paste0(d, "_score")]]   <- NA_real_
      row[[paste0(d, "_status")]]  <- NA_character_
      row[[paste0(d, "_quote")]]   <- NA_character_
      row[[paste0(d, "_context")]] <- NA_character_
    }
  }
  row$parse_repaired            <- if (length(repaired)) paste(repaired, collapse = ",") else NA_character_
  row$populism_confidence       <- as.numeric(parsed$populism_confidence %||% NA)
  row$pop_ideology_confidence   <- as.numeric(parsed$pop_ideology_confidence %||% NA)
  row$liberalism_confidence     <- as.numeric(parsed$liberalism_confidence %||% NA)
  row$pop_justification         <- as.character(parsed$pop_justification %||% NA)
  row$pop_ideology_justification<- as.character(parsed$pop_ideology_justification %||% NA)
  row$lib_justification         <- as.character(parsed$lib_justification %||% NA)
  spans <- map_dfr(V4_DIMS, function(d) {
    obj <- parsed[[d]]
    if (!is.list(obj)) return(tibble())     # flat-integer case → no span available
    q <- obj$span$quote %||% ""
    if (is.na(q) || !nzchar(q)) return(tibble())   # FIX 2026-10-26: nzchar(NA) is TRUE
    tibble(doc_id = chunk_doc_id, doc_id_root = doc_id_root, chunk_id = chunk_id,
           dim_name = d, quote = q,
           context = obj$span$context %||% "",
           chunk_text = chunk_text)
  })
  list(row = row, spans = spans)
}

score_manifesto_v4_claude <- function(doc_id_root, text, country, year) {
  est <- tryCatch(estimate_tokens(text), error = function(e) Inf)
  chunks <- if (is.finite(est) && est <= FULL_TEXT_TOKEN_CAP) list(text)
            else as.list(chunk_text(text, CHUNK_TOKENS, OVERLAP_TOKENS))
  if (length(chunks) == 0L) chunks <- list(text)
  rows <- vector("list", length(chunks))
  spans <- vector("list", length(chunks))
  errors <- character()
  for (ci in seq_along(chunks)) {
    chunk_doc_id <- paste0(doc_id_root, "_ch", ci)
    sr <- score_chunk_v4_claude(chunks[[ci]], chunk_doc_id, country, year)
    if (is.null(sr$parsed)) {
      errors <- c(errors, paste0(chunk_doc_id, ": ", sr$last_error %||% "no_parse"))
      rows[[ci]] <- tibble(doc_id = chunk_doc_id, doc_id_root = doc_id_root,
                           chunk_id = ci, schema_version = NA_character_,
                           scoring_error = sr$last_error %||% "no_parse")
      next
    }
    f <- flatten_parsed_v4_claude(sr$parsed, chunk_doc_id, doc_id_root, ci, chunks[[ci]])
    f$row$raw_tool_json <- sr$raw_json %||% NA_character_   # PATCH 2026-10-24
    rows[[ci]]  <- f$row
    spans[[ci]] <- f$spans
  }
  list(raw = bind_rows(rows), spans = bind_rows(spans), errors = errors,
       doc_id_root = doc_id_root)
}

# ============================================================
# Sample selection (deterministic, stratified)
# ============================================================

message("Loading data...")
# FULL-CORPUS: take all 3,327 manifestos with valid text (same pool as R/14/R/16
# for apples-to-apples cross-LLM comparison). No date / token filters.
dat <- read_parquet("data/CMP/full_data.parquet") |>
  filter(!is.na(manifesto_id), !is.na(text)) |>
  mutate(date = as.Date(date)) |>
  distinct(doc_id, .keep_all = TRUE)

sample_tbl <- dat
message("FULL-CORPUS sample: ", nrow(sample_tbl), " manifestos | ",
        n_distinct(sample_tbl$countryname), " countries")

# ============================================================
# Resume: skip already-scored
# ============================================================

existing_files <- list.files(CHECKPOINT_DIR, pattern = "^batch_\\d+\\.RData$", full.names = TRUE)
already_done <- character()
if (length(existing_files) > 0) {
  for (f in existing_files) {
    e <- new.env(); load(f, envir = e)
    # FIX 2026-10-13: count a manifesto as done only if ALL its chunks parsed in this
    # batch. Previously error rows counted as done, so failed chunks were never retried.
    raw_b <- e$batch_result$raw
    err_b <- if ("scoring_error" %in% names(raw_b)) !is.na(raw_b$scoring_error) else rep(FALSE, nrow(raw_b))
    bad_b <- raw_b$doc_id_root[err_b | is.na(raw_b$schema_version)]
    already_done <- c(already_done, setdiff(unique(raw_b$doc_id_root), bad_b))
  }
  already_done <- unique(already_done)
  message("  resume: ", length(already_done), " manifestos already scored")
}
to_score <- sample_tbl |> filter(!doc_id %in% already_done)
message("  manifestos remaining to score: ", nrow(to_score))

# ============================================================
# Score in batches
# ============================================================

if (nrow(to_score) > 0) {
  plan(multisession, workers = N_WORKERS)
  batch_idx_start <- length(existing_files) + 1L
  n_batches <- ceiling(nrow(to_score) / CHECKPOINT_EVERY)
  for (b in seq_len(n_batches)) {
    batch_idx <- batch_idx_start + b - 1L
    chunk_start <- (b - 1L) * CHECKPOINT_EVERY + 1L
    chunk_end   <- min(b * CHECKPOINT_EVERY, nrow(to_score))
    batch_rows  <- to_score[chunk_start:chunk_end, ]
    message(sprintf("\n--- Claude batch %d (%d-%d of %d remaining) ---",
                    batch_idx, chunk_start, chunk_end, nrow(to_score)))
    t0 <- Sys.time()
    results <- future_map(
      seq_len(nrow(batch_rows)),
      function(i) {
        r <- batch_rows[i, ]
        score_manifesto_v4_claude(r$doc_id, r$text, r$countryname, year(r$date))
      },
      .options = furrr_options(
        globals = c("batch_rows",
                    "MODEL_CLAUDE", "MAX_RETRIES_PER_CHUNK", "RETRY_BASE_SLEEP_SEC",
                    "FULL_TEXT_TOKEN_CAP", "CHUNK_TOKENS", "OVERLAP_TOKENS", "V4_DIMS",
                    "score_manifesto_v4_claude", "score_chunk_v4_claude",
                    "flatten_parsed_v4_claude", "anthropic_v4_chat", "anthropic_v4_schema",
                    "system_prompt_v4_safe", "user_prompt_v4", "poplib_v4_schema",
                    "estimate_tokens", "chunk_text", "truncate_tokens", "%||%"),
        packages = c("tibble", "dplyr", "purrr", "tidyr", "httr", "jsonlite",
                     "rtiktoken", "lubridate"),
        seed = TRUE
      ),
      .progress = TRUE
    )
    batch_result <- list(
      raw    = bind_rows(map(results, "raw")),
      spans  = bind_rows(map(results, "spans")),
      errors = unlist(map(results, "errors")),
      timestamp = Sys.time()
    )
    save(batch_result, file = sprintf("%sbatch_%04d.RData", CHECKPOINT_DIR, batch_idx))
    elapsed <- Sys.time() - t0
    message(sprintf("  batch %d: %.1f %s | rows: %d | spans: %d | errors: %d",
                    batch_idx, as.numeric(elapsed), attr(elapsed, "units"),
                    nrow(batch_result$raw), nrow(batch_result$spans), length(batch_result$errors)))
  }
  plan(sequential)
}

# ============================================================
# Combine + validate + aggregate (same logic as R/14)
# ============================================================

message("\n=== Combining Claude checkpoints ===")
all_files <- list.files(CHECKPOINT_DIR, pattern = "^batch_\\d+\\.RData$", full.names = TRUE)
raw_list <- list(); spans_list <- list(); err_list <- character()
for (f in all_files) {
  e <- new.env(); load(f, envir = e)
  raw_list   <- c(raw_list,   list(e$batch_result$raw))
  spans_list <- c(spans_list, list(e$batch_result$spans))
  err_list   <- c(err_list,   e$batch_result$errors %||% character())
}
raw_claude   <- bind_rows(raw_list)
spans_claude <- bind_rows(spans_list)
write_parquet(raw_claude, "data/CMP/poplib_v4_claude_full_raw.parquet")
message("  rows: ", nrow(raw_claude), "  spans: ", nrow(spans_claude),
        "  errors: ", length(err_list))

message("\n=== Validating Claude spans (regex + LLM judge) ===")
chunk_text_lookup <- setNames(spans_claude$chunk_text, spans_claude$doc_id)
sp_for_check <- system_prompt_v4_safe("X", 2020L)
validated_claude <- validate_spans_table_with_judge(
  spans_claude |> select(doc_id, doc_id_root, dim_name, quote, context),
  chunk_text_lookup, sp_for_check, TOPIC_KEYWORDS_V4,
  use_judge = TRUE, verbose = TRUE
)
write_parquet(validated_claude, "data/CMP/poplib_v4_claude_full_validated_spans.parquet")

message("\n=== Aggregating to manifesto level ===")
qv_claude <- validated_claude |>
  select(doc_id, dim_name, quality_verified) |>
  group_by(doc_id, dim_name) |>
  summarise(quality_verified = any(quality_verified, na.rm = TRUE), .groups = "drop")

aggregate_dim_claude <- function(dim_name) {
  score_col  <- paste0(dim_name, "_score")
  status_col <- paste0(dim_name, "_status")
  if (!(score_col %in% names(raw_claude))) return(NULL)
  per_chunk <- raw_claude |>
    select(doc_id, doc_id_root, score = all_of(score_col), status = all_of(status_col)) |>
    left_join(qv_claude |> filter(dim_name == !!dim_name) |> select(doc_id, quality_verified),
              by = "doc_id") |>
    mutate(quality_verified = ifelse(is.na(quality_verified), FALSE, quality_verified)) |>
    mutate(eligible = quality_verified &
                       status %in% c("verified", "mixed") &
                       !is.na(score))
  per_chunk |>
    group_by(doc_id_root) |>
    summarise(
      "{dim_name}_score" := if (any(eligible)) mean(score[eligible], na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
}
agg_claude <- reduce(compact(map(V4_DIMS, aggregate_dim_claude)),
                     full_join, by = "doc_id_root") |>
  rename(doc_id = doc_id_root)
write_parquet(agg_claude, "data/CMP/poplib_v4_claude_full_agg.parquet")
message("  saved data/CMP/poplib_v4_claude_full_agg.parquet")

# ============================================================
# Cross-model agreement vs gpt-4o-mini v4
# ============================================================

message("\n=== Computing cross-model agreement (gpt-4o-mini-v4 vs Claude-v4) ===")
if (!file.exists("data/CMP/poplib_v4_agg_results.parquet")) {
  warning("OpenAI v4 results not yet available. Skipping cross-model comparison.")
} else {
  v4_openai <- read_parquet("data/CMP/poplib_v4_agg_results.parquet")
  comp <- agg_claude |>
    rename_with(~ sub("_score$", "_claude", .x), ends_with("_score")) |>
    inner_join(v4_openai |> rename_with(~ sub("_score$", "_openai", .x), ends_with("_score")),
               by = "doc_id")

  key_vars <- c("populism_overall","liberalism_overall","pop_ideology_overall",
                "lib_political","lib_social","lib_economic","lib_financial_market",
                "pop_anti_elitism","pop_people_centrism","pop_manichean")
  agreement <- map_dfr(key_vars, function(v) {
    a <- comp[[paste0(v, "_openai")]]; b <- comp[[paste0(v, "_claude")]]
    ok <- !is.na(a) & !is.na(b)
    if (sum(ok) < 10) return(tibble(variable = v, n = sum(ok),
                                     r = NA_real_, MAD = NA_real_,
                                     mean_openai = NA_real_, mean_claude = NA_real_))
    tibble(
      variable    = v,
      n           = sum(ok),
      r           = round(cor(a[ok], b[ok]), 3),
      spearman    = round(cor(a[ok], b[ok], method = "spearman"), 3),
      MAD         = round(mean(abs(a[ok] - b[ok])), 3),
      mean_openai = round(mean(a[ok]), 2),
      mean_claude = round(mean(b[ok]), 2)
    )
  })
  cat("\n--- v4 cross-model agreement (n =", nrow(comp), "manifestos in overlap) ---\n")
  print(agreement, n = 20)

  cross <- list(timestamp = Sys.time(),
                agreement = agreement, comp = comp,
                sample = sample_tbl |> select(doc_id, countryname, date, pop_q))
  save(cross, file = "data/CMP/poplib_v4_cross_agreement.RData")
  message("\nSaved: data/CMP/poplib_v4_cross_agreement.RData")
}

message("\nDone.")
