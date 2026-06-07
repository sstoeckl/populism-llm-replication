# ============================================================
# 16_Score_Full_Corpus_gpt41mini.R   (BRANCH: pipeline-v4-safe)
#
# FULL-CORPUS V4 SAFE SCORING DRIVER
#
# Re-scores all 3,327 CMP manifestos under the v4 safe pipeline:
#   - schema poplib_v4 (per-dim {score, evidence_status, span})
#   - system_prompt_v4_safe (anti-hallucination + NA-aware)
#   - deterministic post-hoc validation (R/12) with multilingual regex
#     + LLM-as-judge backstop
#   - manifesto-level aggregation that PROPAGATES NAs (no_evidence
#     does NOT default to 0)
#
# Robustness features:
#   - Per-chunk retry with exponential backoff (handles transient
#     5xx / 429 / parse failures)
#   - Per-batch RData checkpoints every CHECKPOINT_EVERY manifestos
#     (so a crash mid-run loses at most one batch)
#   - Resume capability: on restart, skips manifestos already scored
#     (reads existing checkpoint files)
#   - furrr parallel execution (8 workers default; respect Anthropic
#     /OpenAI rate limits)
#
# Outputs:
#   data/CMP/v4_checkpoints/chunk_*.RData        per-batch raw scores
#   data/CMP/poplib_v4_gpt41mini_raw.parquet               combined raw chunk rows
#   data/CMP/poplib_v4_gpt41mini_validated_spans.parquet   per-span validation table
#   data/CMP/poplib_v4_gpt41mini_agg_results.parquet       manifesto-level aggregates
#                                                (with NA propagation)
#   data/CMP/poplib_v4_gpt41mini_summary.RData              summary stats + comparison vs v1
#
# Cost (estimated):
#   - Scoring: ~$2.50 (gpt-4o-mini × 3,327 × ~1.5 chunks)
#   - LLM judge: ~$5 (~46k judge calls on regex-failed spans)
#   - Total: ~$7.50
#
# Wall time (with 8 parallel workers):
#   - Scoring: ~3-5 hours
#   - Validation + judge: ~30 min
#   - Aggregation: ~5 min
#
# Resume:
#   Rscript R/16_Score_Full_Corpus_gpt41mini.R
#   (If checkpoints exist, only un-scored manifestos are processed.)
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(readxl)
  library(lubridate); library(jsonlite); library(furrr)
})
source("R/1_Functions.R")
source("R/3c_Populism_v4_safe.R")
source("R/12_Validate_Span_Quality.R")

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) stop("OPENAI_API_KEY missing")

message("=== Full-corpus v4 safe scoring driver ===")
message("Timestamp: ", Sys.time())

# ============================================================
# Configuration
# ============================================================

N_WORKERS              <- min(6L, max(1L, parallel::detectCores() - 1L))   # 6 workers to stay under OpenAI rate limits when running alongside R/15
CHECKPOINT_EVERY       <- 100L
CHECKPOINT_DIR         <- "data/CMP/v4_gpt41mini_checkpoints/"
MAX_RETRIES_PER_CHUNK  <- 5L
RETRY_BASE_SLEEP_SEC   <- 2
FULL_TEXT_TOKEN_CAP    <- 18000L
CHUNK_TOKENS           <- 20000L
OVERLAP_TOKENS         <- 2000L
MODEL                  <- "gpt-4.1-mini-2025-04-14"
JUDGE_MODEL            <- "gpt-4o-mini-2024-07-18"  # judge stays on cheaper model

ensure_dir(CHECKPOINT_DIR)

# ============================================================
# Helpers
# ============================================================

V4_DIMS <- c(
  "pop_anti_elitism","pop_people_centrism","pop_manichean","populism_overall",
  "pop_ideology_left","pop_ideology_right","pop_ideology_centrist","pop_ideology_overall",
  "lib_political","lib_social","lib_economic","lib_financial_market","liberalism_overall"
)

# Robust single-chunk scorer with exponential-backoff retry.
score_chunk_v4_robust <- function(chunk_text, doc_id_in, country, year,
                                  model = MODEL, max_retries = MAX_RETRIES_PER_CHUNK,
                                  base_sleep = RETRY_BASE_SLEEP_SEC) {
  sysp <- system_prompt_v4_safe(country, year)
  usp  <- user_prompt_v4(chunk_text, doc_id_in)
  for (attempt in seq_len(max_retries)) {
    res <- tryCatch(
      oa_chat_json(
        system_prompt = sysp, user_prompt = usp,
        model = model, json_schema = poplib_v4_schema,
        temperature = 0, top_p = 1, store = TRUE,
        max_output_tokens = 5000L
      ),
      error = function(e) list(text = NA, error = conditionMessage(e))
    )
    if (!is.null(res$text) && is.character(res$text) && nzchar(res$text)) {
      parsed <- tryCatch(fromJSON(res$text, simplifyVector = FALSE),
                         error = function(e) NULL)
      if (!is.null(parsed)) return(list(parsed = parsed, attempts = attempt))
    }
    # Transient — back off and retry
    Sys.sleep(base_sleep * (2^(attempt - 1)))
  }
  list(parsed = NULL, attempts = max_retries, last_error = res$error %||% "unknown")
}

# Flatten parsed v4 JSON into a 1-row tibble (raw chunk-level) plus the
# spans table for this chunk.
flatten_parsed_v4 <- function(parsed, chunk_doc_id, doc_id_root,
                              chunk_id, chunk_text) {
  row <- tibble(
    doc_id         = chunk_doc_id,
    doc_id_root    = doc_id_root,
    chunk_id       = chunk_id,
    schema_version = parsed$schema_version %||% NA_character_
  )
  for (d in V4_DIMS) {
    obj <- parsed[[d]]
    row[[paste0(d, "_score")]]   <- if (is.null(obj$score)) NA_real_ else as.numeric(obj$score)
    row[[paste0(d, "_status")]]  <- as.character(obj$evidence_status %||% NA)
    row[[paste0(d, "_quote")]]   <- as.character(obj$span$quote   %||% NA)
    row[[paste0(d, "_context")]] <- as.character(obj$span$context %||% NA)
  }
  row$populism_confidence       <- as.numeric(parsed$populism_confidence %||% NA)
  row$pop_ideology_confidence   <- as.numeric(parsed$pop_ideology_confidence %||% NA)
  row$liberalism_confidence     <- as.numeric(parsed$liberalism_confidence %||% NA)
  row$pop_justification         <- as.character(parsed$pop_justification %||% NA)
  row$pop_ideology_justification<- as.character(parsed$pop_ideology_justification %||% NA)
  row$lib_justification         <- as.character(parsed$lib_justification %||% NA)

  spans <- map_dfr(V4_DIMS, function(d) {
    q <- parsed[[d]]$span$quote %||% ""
    if (!nzchar(q)) return(tibble())
    tibble(doc_id = chunk_doc_id, doc_id_root = doc_id_root, chunk_id = chunk_id,
           dim_name = d, quote = q,
           context = parsed[[d]]$span$context %||% "",
           chunk_text = chunk_text)
  })
  list(row = row, spans = spans)
}

# Score one entire manifesto (chunked if long). Returns list(raw, spans, errors).
score_manifesto_v4_robust <- function(doc_id_root, text, country, year) {
  est <- tryCatch(estimate_tokens(text), error = function(e) Inf)
  chunks <- if (is.finite(est) && est <= FULL_TEXT_TOKEN_CAP) list(text)
            else as.list(chunk_text(text, CHUNK_TOKENS, OVERLAP_TOKENS))
  if (length(chunks) == 0L) chunks <- list(text)

  rows  <- vector("list", length(chunks))
  spans <- vector("list", length(chunks))
  errors <- character()
  for (ci in seq_along(chunks)) {
    chunk_doc_id <- paste0(doc_id_root, "_ch", ci)
    sr <- score_chunk_v4_robust(chunks[[ci]], chunk_doc_id, country, year)
    if (is.null(sr$parsed)) {
      # Persistent failure after retries — record an error row
      err <- sr$last_error %||% "no_parsed_result"
      errors <- c(errors, paste0(chunk_doc_id, ": ", err))
      rows[[ci]] <- tibble(doc_id = chunk_doc_id, doc_id_root = doc_id_root,
                           chunk_id = ci, schema_version = NA_character_,
                           scoring_error = err)
      next
    }
    f <- flatten_parsed_v4(sr$parsed, chunk_doc_id, doc_id_root, ci, chunks[[ci]])
    rows[[ci]]  <- f$row
    spans[[ci]] <- f$spans
  }
  list(raw = bind_rows(rows), spans = bind_rows(spans), errors = errors,
       doc_id_root = doc_id_root)
}

# ============================================================
# Load data + identify what still needs scoring
# ============================================================

message("Loading manifesto data...")
dat <- read_parquet("data/CMP/full_data.parquet") |>
  filter(!is.na(manifesto_id), !is.na(text)) |>
  mutate(date = as.Date(date)) |>
  distinct(doc_id, .keep_all = TRUE)
message("  manifestos available: ", nrow(dat))

# Existing checkpoints
existing_files <- list.files(CHECKPOINT_DIR, pattern = "^batch_\\d+\\.RData$", full.names = TRUE)
already_done <- character()
if (length(existing_files) > 0) {
  message("  found ", length(existing_files), " existing checkpoints; loading to find done IDs...")
  for (f in existing_files) {
    e <- new.env()
    load(f, envir = e)
    already_done <- c(already_done, unique(e$batch_result$raw$doc_id_root))
  }
  already_done <- unique(already_done)
  message("  already-scored manifestos: ", length(already_done))
}

to_score <- dat |> filter(!doc_id %in% already_done)
message("  manifestos remaining to score: ", nrow(to_score))

if (nrow(to_score) == 0) {
  message("All manifestos already scored. Skipping to aggregation step.")
} else {
  # ============================================================
  # Score in checkpointed batches
  # ============================================================
  plan(multisession, workers = N_WORKERS)
  message("Parallel plan: ", N_WORKERS, " workers")
  message("Checkpoint cadence: every ", CHECKPOINT_EVERY, " manifestos")

  batch_idx_start <- length(existing_files) + 1L
  n_batches <- ceiling(nrow(to_score) / CHECKPOINT_EVERY)
  for (b in seq_len(n_batches)) {
    batch_idx <- batch_idx_start + b - 1L
    chunk_start <- (b - 1L) * CHECKPOINT_EVERY + 1L
    chunk_end   <- min(b * CHECKPOINT_EVERY, nrow(to_score))
    batch_rows  <- to_score[chunk_start:chunk_end, ]
    message(sprintf("\n--- Batch %d (%d-%d of %d remaining) ---",
                    batch_idx, chunk_start, chunk_end, nrow(to_score)))
    t0 <- Sys.time()

    results <- future_map(
      seq_len(nrow(batch_rows)),
      function(i) {
        r <- batch_rows[i, ]
        score_manifesto_v4_robust(r$doc_id, r$text, r$countryname, year(r$date))
      },
      .options = furrr_options(
        globals = c("batch_rows",                                  # the iteration source
                    "MODEL", "MAX_RETRIES_PER_CHUNK", "RETRY_BASE_SLEEP_SEC",
                    "FULL_TEXT_TOKEN_CAP", "CHUNK_TOKENS", "OVERLAP_TOKENS",
                    "V4_DIMS", "score_manifesto_v4_robust", "score_chunk_v4_robust",
                    "flatten_parsed_v4", "system_prompt_v4_safe", "user_prompt_v4",
                    "poplib_v4_schema", "oa_chat_json", "estimate_tokens",
                    "chunk_text", "truncate_tokens", "%||%"),
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
    message(sprintf("  batch %d done in %.1f %s | rows: %d | spans: %d | errors: %d",
                    batch_idx, as.numeric(elapsed), attr(elapsed, "units"),
                    nrow(batch_result$raw), nrow(batch_result$spans), length(batch_result$errors)))
  }
  plan(sequential)
}

# ============================================================
# Combine all checkpoint batches
# ============================================================

message("\n=== Combining checkpoint batches ===")
all_files <- list.files(CHECKPOINT_DIR, pattern = "^batch_\\d+\\.RData$", full.names = TRUE)
raw_list <- list(); spans_list <- list(); err_list <- character()
for (f in all_files) {
  e <- new.env(); load(f, envir = e)
  raw_list   <- c(raw_list,   list(e$batch_result$raw))
  spans_list <- c(spans_list, list(e$batch_result$spans))
  err_list   <- c(err_list,   e$batch_result$errors %||% character())
}
raw_v4   <- bind_rows(raw_list)
spans_v4 <- bind_rows(spans_list)
message("  combined raw rows: ", nrow(raw_v4))
message("  combined span rows: ", nrow(spans_v4))
message("  total persistent errors: ", length(err_list))

write_parquet(raw_v4, "data/CMP/poplib_v4_gpt41mini_raw.parquet")
message("  saved data/CMP/poplib_v4_gpt41mini_raw.parquet")

# ============================================================
# Validate every span (judge backstop)
# ============================================================

message("\n=== Validating all spans (regex + LLM-judge backstop) ===")
chunk_text_lookup <- setNames(spans_v4$chunk_text, spans_v4$doc_id)
sp_for_check <- system_prompt_v4_safe("X", 2020L)

t0 <- Sys.time()
validated <- validate_spans_table_with_judge(
  spans_v4 |> select(doc_id, doc_id_root, dim_name, quote, context),
  chunk_text_lookup, sp_for_check, TOPIC_KEYWORDS_V4,
  use_judge = TRUE, verbose = TRUE
)
message(sprintf("  validation done in %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
message(sprintf("  judge cache entries: %d", length(ls(envir = .judge_cache))))

write_parquet(validated, "data/CMP/poplib_v4_gpt41mini_validated_spans.parquet")
message("  saved data/CMP/poplib_v4_gpt41mini_validated_spans.parquet")

# ============================================================
# Aggregate to manifesto level with NA propagation
# ============================================================
#
# Rule: a chunk's score for dimension X counts in aggregation IFF
#  (a) status == "verified" or "mixed" (not no_evidence and not NA), AND
#  (b) the corresponding span is quality_verified (no hallucination,
#      verbatim, on-topic).
# If a manifesto has NO qualifying chunks for X, the manifesto's X = NA.

message("\n=== Aggregating to manifesto level (NA-aware) ===")

# Build per-(doc_id, dim_name) quality-verified flag from validated table
qv <- validated |>
  select(doc_id, dim_name, quality_verified) |>
  group_by(doc_id, dim_name) |>
  summarise(quality_verified = any(quality_verified, na.rm = TRUE),
            .groups = "drop")

# For each dim, count chunks where qualifier is met, take mean of score across them
aggregate_dim <- function(dim_name) {
  score_col  <- paste0(dim_name, "_score")
  status_col <- paste0(dim_name, "_status")
  if (!(score_col %in% names(raw_v4))) return(NULL)
  per_chunk <- raw_v4 |>
    select(doc_id, doc_id_root, score = all_of(score_col), status = all_of(status_col)) |>
    left_join(qv |> filter(dim_name == !!dim_name) |> select(doc_id, quality_verified),
              by = "doc_id") |>
    mutate(quality_verified = ifelse(is.na(quality_verified), FALSE, quality_verified)) |>
    mutate(eligible = quality_verified &
                       status %in% c("verified", "mixed") &
                       !is.na(score))
  per_manifesto <- per_chunk |>
    group_by(doc_id_root) |>
    summarise(
      "{dim_name}_score"        := if (any(eligible)) mean(score[eligible], na.rm = TRUE) else NA_real_,
      "{dim_name}_n_eligible"   := sum(eligible),
      "{dim_name}_n_no_evidence":= sum(status == "no_evidence", na.rm = TRUE),
      "{dim_name}_n_chunks"     := n(),
      .groups = "drop"
    )
  per_manifesto
}

agg_list <- map(V4_DIMS, aggregate_dim)
agg_list <- compact(agg_list)
agg_v4 <- reduce(agg_list, full_join, by = "doc_id_root") |>
  rename(doc_id = doc_id_root) |>
  left_join(dat |> select(doc_id, countryname, partyname, date, party), by = "doc_id") |>
  # Rescale ideology to [-1, +1] when both directions have evidence
  mutate(
    pop_ideology_overall2 = case_when(
      is.na(pop_ideology_right_score) & is.na(pop_ideology_left_score) ~ NA_real_,
      TRUE ~ (coalesce(pop_ideology_right_score, 0) - coalesce(pop_ideology_left_score, 0)) /
              (coalesce(pop_ideology_right_score, 0) + coalesce(pop_ideology_left_score, 0) +
               coalesce(pop_ideology_centrist_score, 0) + 1e-9)
    )
  )
write_parquet(agg_v4, "data/CMP/poplib_v4_gpt41mini_agg_results.parquet")
message("  saved data/CMP/poplib_v4_gpt41mini_agg_results.parquet")
message(sprintf("  manifesto rows: %d", nrow(agg_v4)))

# ============================================================
# Summary + comparison vs v1
# ============================================================

message("\n=== Summary ===")
v1 <- read_excel("data/CMP/poplib_agg_results.xlsx") |> mutate(date = as.Date(date))

summary_stats <- list(
  timestamp = Sys.time(),
  n_manifestos_total  = nrow(agg_v4),
  n_persistent_errors = length(err_list),
  span_stats = validated |>
    group_by(dim_name) |>
    summarise(
      n = n(),
      pct_hallucin    = round(100 * mean(hallucination_flag, na.rm = TRUE), 2),
      pct_verbatim    = round(100 * mean(verbatim_found, na.rm = TRUE), 2),
      pct_topic       = round(100 * mean(topic_flag, na.rm = TRUE), 2),
      pct_verified    = round(100 * mean(quality_verified, na.rm = TRUE), 2),
      .groups = "drop"
    ),
  na_rate_per_dim = agg_v4 |>
    summarise(across(ends_with("_score"), ~ round(100 * mean(is.na(.x)), 2))) |>
    pivot_longer(everything(), names_to = "dimension", values_to = "pct_NA")
)

cat("\n--- Span quality ---\n")
print(summary_stats$span_stats, n = 20)
cat("\n--- NA rate per manifesto-level dimension ---\n")
print(summary_stats$na_rate_per_dim, n = 20)

# v1 vs v4 correlations on the overlap
key_vars <- c("populism_overall", "liberalism_overall",
              "lib_political", "lib_social", "lib_economic", "lib_financial_market",
              "pop_anti_elitism", "pop_people_centrism", "pop_manichean")
cmp <- v1 |> rename_with(~ paste0(.x, "_v1"), all_of(key_vars)) |>
  select(doc_id, all_of(paste0(key_vars, "_v1"))) |>
  inner_join(agg_v4 |> select(doc_id, all_of(paste0(key_vars, "_score"))) |>
                       rename_with(~ sub("_score$", "_v4", .x), all_of(paste0(key_vars, "_score"))),
             by = "doc_id")

v1_v4_cor <- map_dfr(key_vars, function(v) {
  a <- cmp[[paste0(v, "_v1")]]; b <- cmp[[paste0(v, "_v4")]]
  ok <- !is.na(a) & !is.na(b)
  tibble(variable = v, n_both_ok = sum(ok),
         r = if (sum(ok) >= 5 && sd(a[ok]) > 0 && sd(b[ok]) > 0) round(cor(a[ok], b[ok]), 3) else NA_real_,
         MAD = if (sum(ok) >= 5) round(mean(abs(a[ok] - b[ok])), 3) else NA_real_,
         mean_v1 = if (sum(ok) >= 5) round(mean(a[ok]), 2) else NA_real_,
         mean_v4 = if (sum(ok) >= 5) round(mean(b[ok]), 2) else NA_real_)
})
cat("\n--- v1 vs v4 (overlap manifestos only) ---\n")
print(v1_v4_cor, n = 20)

summary_stats$v1_v4_comparison <- v1_v4_cor
summary_stats$errors <- err_list
save(summary_stats, file = "data/CMP/poplib_v4_gpt41mini_summary.RData")
message("\nSaved: data/CMP/poplib_v4_gpt41mini_summary.RData")
message("Done.")
