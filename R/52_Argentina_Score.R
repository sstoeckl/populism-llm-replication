# ============================================================
# 52_Argentina_Score.R  (PLLM-0185, 2026-10-28)
#
# Out-of-sample scoring of the Argentine 2021-2025 party platforms (Camara Nacional Electoral
# filings, OCR text in data/external/argentina/text_clean/, register argentina_docs.csv) with
# the FROZEN production pipeline: the same prompt, schema (poplib_v4), chunking, retries,
# temperature and per-chunk scorers as R/15b (Claude), R/16 (GPT-4.1-mini) and R/18 (Gemini),
# loaded exactly as R/32 loads them (function definitions evaluated, drivers not executed), with
# R/32's usage-logging wrappers, provider guards and model-version trace. Nothing is
# reimplemented. The baseline corpus is NOT touched: outputs go to data/CMP/argentina_oos/ and
# are never merged into data/CMP/v2026a/.
#
# Usage:
#   Rscript R/52_Argentina_Score.R score    <model>     # checkpointed, resumable; then finalize
#   Rscript R/52_Argentina_Score.R finalize <model>     # combine checkpoints -> raw + validated spans
#   Rscript R/52_Argentina_Score.R estimate             # token count per document and chunk count
#   <model> in {claude, gpt41mini, gemini}
# Environment: GEMINI_BACKEND (vertex | aistudio; default vertex), GEMINI_MODEL (default
#   gemini-3.5-flash), EXPECT_GEMINI (default = GEMINI_MODEL; abort if another version is served),
#   CLAUDE_WORKERS / GEMINI_WORKERS.
#
# Outputs (data/CMP/argentina_oos/):
#   checkpoints_<model>/batch_NNNN.RData
#   poplib_v4_<model>_raw.parquet, poplib_v4_<model>_validated_spans.parquet
#   usage_logs/usage_v2_<pid>.csv   (requested and resolved model per call)
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(lubridate)
  library(jsonlite); library(httr); library(furrr)
})
source("R/1_Functions.R")
source("R/3c_Populism_v4_safe.R")
source("R/12_Validate_Span_Quality.R")

args  <- commandArgs(trailingOnly = TRUE)
MODE  <- args[1] %||% "estimate"
WHICH <- args[2] %||% NA_character_

AR_DIR    <- "data/external/argentina"
UPD_DIR   <- "data/CMP/argentina_oos"          # R/32 wrappers write STOP/ABORT files here
USAGE_DIR <- file.path(UPD_DIR, "usage_logs")
ensure_dir(USAGE_DIR)
CHECKPOINT_EVERY_UPD <- 10L
WORKERS   <- c(claude = 2L, gpt41mini = 3L, gemini = 2L)
if (nzchar(Sys.getenv("CLAUDE_WORKERS"))) WORKERS[["claude"]] <- as.integer(Sys.getenv("CLAUDE_WORKERS"))
if (nzchar(Sys.getenv("GEMINI_WORKERS"))) WORKERS[["gemini"]] <- as.integer(Sys.getenv("GEMINI_WORKERS"))

# ---- Production definitions, loaded exactly as in R/32 ----------------------------------
# R/32's own helper and wrapper definitions are pulled from its source the same way (top-level
# assignments only; its data/estimate/score/finalize blocks are not executed).
r32 <- parse("R/32_CMP2026a_Score.R", keep.source = FALSE)
eval_defs <- function(exprs, keep) {
  got <- character()
  for (e in exprs) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
                       as.character(e[[2]]) %in% keep) { eval(e, envir = globalenv()); got <- c(got, as.character(e[[2]])) }
  miss <- setdiff(keep, got); if (length(miss)) stop("R/32 definitions not found: ", paste(miss, collapse = ", "))
  invisible(got)
}
eval_defs(r32, c("load_defs", "CHUNK_CONSTS", "chunk_fingerprint"))
load_defs("R/15b_Claude_Full_Corpus.R",
          c(CHUNK_CONSTS, "MODEL_CLAUDE", "anthropic_v4_schema", "anthropic_v4_chat",
            "score_chunk_v4_claude", "decode_dim_string", "flatten_parsed_v4_claude", "score_manifesto_v4_claude"))
fp_claude <- chunk_fingerprint()
load_defs("R/16_Score_Full_Corpus_gpt41mini.R",
          c(CHUNK_CONSTS, "MODEL", "score_chunk_v4_robust", "flatten_parsed_v4", "score_manifesto_v4_robust"))
fp_gpt <- chunk_fingerprint()
load_defs("R/18_Score_Full_Corpus_Gemini_flash_latest.R",
          c(CHUNK_CONSTS, "MODEL_GEMINI", "to_gemini_schema", "gemini_v4_schema",
            "gemini_v4_chat", "score_chunk_v4_gemini", "flatten_parsed_v4_gemini", "score_manifesto_v4_gemini"))
stopifnot("chunking config must be identical across production scripts" =
            fp_claude == fp_gpt && fp_gpt == chunk_fingerprint())

# Gemini: pinned ID on the Vertex AI global endpoint (the alias redirects silently on AI Studio;
# CLAUDE.md / data/CMP/v2026a/ANCHOR.md). The served modelVersion is checked on every call.
GEMINI_BACKEND <- Sys.getenv("GEMINI_BACKEND", "vertex")
MODEL_GEMINI   <- Sys.getenv("GEMINI_MODEL", "gemini-3.5-flash")
EXPECT_GEMINI  <- Sys.getenv("EXPECT_GEMINI", MODEL_GEMINI)
MODEL_ID <- c(claude = MODEL_CLAUDE, gpt41mini = MODEL, gemini = MODEL_GEMINI)
TARGET <- "argentina"; SUFFIX <- ""
ABORT_FILE  <- file.path(UPD_DIR, "ABORT_gemini_model_version")
eval_defs(r32, c("STOP_FILE", "BILLING_PAT", "ensure_model_trace", "traced_call", "doc_of", "log_usage",
                 "guarded_call", "anthropic_v4_chat_prod", "anthropic_v4_chat", "oa_chat_json_prod", "oa_chat_json",
                 "VERTEX_URL", "VERTEX_KEY", "vertex_token", "gemini_v4_chat_vertex", "gemini_v4_chat_prod",
                 "gemini_v4_chat", "CHUNK_FUNS", "score_selected_chunks", "SCORER", "COMMON_GLOBALS",
                 "score_rows", "latest_attempt", "ok_docs_of", "read_ckpts"))

# ---- Data: the 38 platforms -------------------------------------------------------------
docs <- read_csv(file.path(AR_DIR, "argentina_docs.csv"), col_types = cols(.default = "c")) |>
  mutate(date = as.Date(date), milei = as.integer(milei)) |> arrange(doc_id)
docs$text <- map_chr(docs$txt, ~ paste(readLines(file.path(AR_DIR, .x), encoding = "UTF-8", warn = FALSE), collapse = "\n"))
stopifnot(!anyDuplicated(docs$doc_id), all(nzchar(docs$text)))
to_score_all <- docs |> select(doc_id, countryname, date, text)

if (MODE == "estimate") {
  est <- docs |> mutate(tokens = map_dbl(text, ~ tryCatch(estimate_tokens(.x), error = function(e) NA_real_)),
                        n_chunks = if_else(tokens <= FULL_TEXT_TOKEN_CAP, 1L,
                                           map_int(text, ~ length(chunk_text(.x, CHUNK_TOKENS, OVERLAP_TOKENS))))) |>
    select(doc_id, words, tokens, n_chunks)
  print(as.data.frame(est)); message(sprintf("total: %d docs, %.0f tokens, %d chunks (x3 models)",
                                              nrow(est), sum(est$tokens), sum(est$n_chunks)))
  quit(save = "no")
}

# ---- score / finalize: as in R/32 -------------------------------------------------------
stopifnot(WHICH %in% names(SCORER))
ckpt_dir <- file.path(UPD_DIR, paste0("checkpoints_", WHICH))

if (MODE == "score") {
  ensure_dir(ckpt_dir)
  prev <- read_ckpts()
  done <- ok_docs_of(bind_rows(map(prev, "raw")))
  todo <- to_score_all |> filter(!doc_id %in% done)
  message(sprintf("[%s | %s | %s] to score: %d (already ok: %d)", WHICH, MODEL_ID[[WHICH]],
                  if (WHICH == "gemini") GEMINI_BACKEND else "production endpoint", nrow(todo), length(done)))
  b0 <- length(prev)
  n_b <- ceiling(nrow(todo) / CHECKPOINT_EVERY_UPD)
  for (b in seq_len(n_b)) {
    rows <- todo[((b - 1) * CHECKPOINT_EVERY_UPD + 1):min(b * CHECKPOINT_EVERY_UPD, nrow(todo)), ]
    t0 <- Sys.time()
    res <- score_rows(rows, WHICH, WORKERS[[WHICH]])
    batch_result <- list(raw = bind_rows(map(res, "raw")), spans = bind_rows(map(res, "spans")),
                         errors = unlist(map(res, "errors")), timestamp = Sys.time(), model = MODEL_ID[[WHICH]])
    ck_final <- sprintf("%s/batch_%04d.RData", ckpt_dir, b0 + b); ck_tmp <- paste0(ck_final, ".tmp")
    save(batch_result, file = ck_tmp); stopifnot(file.rename(ck_tmp, ck_final))
    message(sprintf("  batch %d/%d: %d docs, %d chunk rows, %d errors, %.1f min", b, n_b, nrow(rows),
                    nrow(batch_result$raw), length(batch_result$errors), as.numeric(Sys.time() - t0, units = "mins")))
    if (length(batch_result$errors)) message("  first error: ", substr(batch_result$errors[1], 1, 300))
    if (file.exists(ABORT_FILE) && WHICH == "gemini") { message("  ABORT: ", readLines(ABORT_FILE)[1]); quit(save = "no", status = 4) }
    prov <- c(claude = "anthropic", gpt41mini = "openai", gemini = "google")[[WHICH]]
    if (file.exists(STOP_FILE(prov)) || any(grepl(BILLING_PAT, batch_result$errors, ignore.case = TRUE))) {
      message("  STOP: persistent quota/billing errors. Re-run later to resume."); quit(save = "no", status = 3)
    }
  }
  MODE <- "finalize"
}

if (MODE == "finalize") {
  ck <- read_ckpts()
  raw_all <- bind_rows(map(ck, "raw")); spans_all <- bind_rows(map(ck, "spans"))
  ok <- ok_docs_of(raw_all)
  last_b <- latest_attempt(raw_all) |> distinct(doc_id_root, .batch)
  raw_all <- raw_all |> semi_join(last_b, by = c("doc_id_root", ".batch")) |> filter(doc_id_root %in% ok) |> select(-.batch)
  spans_all <- spans_all |> semi_join(last_b, by = c("doc_id_root", ".batch")) |> filter(doc_id_root %in% ok) |> select(-.batch)
  raw_all$model_id <- unique(map_chr(ck, ~ .x$model %||% NA_character_)) |> paste(collapse = ";")
  prov <- c(claude = "anthropic", gpt41mini = "openai", gemini = "google")[[WHICH]]
  mv <- list.files(USAGE_DIR, "^usage_v2_", full.names = TRUE) |>
    map_dfr(~ read_csv(.x, col_types = cols(.default = "c"))) |>
    filter(provider %in% c(prov, paste0(prov, "-vertex")), model == MODEL_ID[[WHICH]], !is.na(doc_id)) |>
    arrange(ts) |> group_by(doc_id) |> slice_tail(n = 1) |> ungroup() |>
    select(doc_id, model_resolved = resolved_model, endpoint = provider, scored_at = ts)   # v2026a raw-file layout
  raw_all <- raw_all |> left_join(mv, by = "doc_id") |>
    mutate(source_run = "argentina_oos_2026-10", scoring_target = TARGET, model_requested = MODEL_ID[[WHICH]],
           corpus_version = "CNE_2021-2025_ocr", orig_doc_id = doc_id)
  message("  resolved model versions: ", paste(names(table(raw_all$model_resolved, useNA = "ifany")),
                                             table(raw_all$model_resolved, useNA = "ifany"), sep = "=", collapse = " | "))
  write_parquet(raw_all, file.path(UPD_DIR, paste0("poplib_v4_", WHICH, "_raw.parquet")))
  message(sprintf("[%s] ok documents: %d / %d | chunk rows: %d | spans: %d | not ok: %s", WHICH, length(ok),
                  nrow(to_score_all), nrow(raw_all), nrow(spans_all),
                  paste(setdiff(to_score_all$doc_id, ok), collapse = ", ")))
  # R/12 span validation, called exactly as in R/15b/16/18 and R/32
  chunk_text_lookup <- setNames(spans_all$chunk_text, spans_all$doc_id)
  sp_for_check <- system_prompt_v4_safe("X", 2020L)
  validated <- validate_spans_table_with_judge(
    spans_all |> select(doc_id, doc_id_root, dim_name, quote, context),
    chunk_text_lookup, sp_for_check, TOPIC_KEYWORDS_V4, use_judge = TRUE, verbose = TRUE)
  write_parquet(validated, file.path(UPD_DIR, paste0("poplib_v4_", WHICH, "_validated_spans.parquet")))
  message("  saved raw + validated spans for ", WHICH)
}
