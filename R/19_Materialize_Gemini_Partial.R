# =============================================================
# 19_Materialize_Gemini_Partial.R
#
# After R/18 was paused at 30/33 checkpoint batches (Gemini billing cap):
#   - 27 batches successfully scored ~2,697 of 3,327 manifestos
#   - Last 3 batches all failed with HTTP 429 (cap exceeded)
#
# This script materializes the partial-coverage Gemini results into the
# same parquet shape as the other models, with the missing 630 manifestos
# flagged as "no_gemini_coverage". Then aggregates per-manifesto for the
# website and uploads the raw parquet to Hugging Face.
# =============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(furrr)
})
source("R/1_Functions.R")
source("R/3c_Populism_v4_safe.R")
source("R/12_Validate_Span_Quality.R")

ckpt_dir <- "data/CMP/v4_gemini_flash_latest_checkpoints"
ckpt_files <- list.files(ckpt_dir, pattern = "batch_\\d+\\.RData$", full.names = TRUE)
message("Checkpoint batches found: ", length(ckpt_files))

# Combine all batches
raw_list <- list(); spans_list <- list(); errs <- character()
for (f in ckpt_files) {
  e <- new.env(); load(f, envir = e)
  raw_list   <- c(raw_list,   list(e$batch_result$raw))
  spans_list <- c(spans_list, list(e$batch_result$spans))
  errs       <- c(errs,       e$batch_result$errors %||% character())
}
raw_all <- bind_rows(raw_list)
spans_all <- bind_rows(spans_list)
message("Combined raw rows: ", nrow(raw_all), " | spans: ", nrow(spans_all), " | error msgs: ", length(errs))

# Identify successfully scored vs failed manifestos
if ("scoring_error" %in% names(raw_all)) {
  ok_docs <- raw_all |> filter(is.na(scoring_error), !is.na(schema_version)) |>
    pull(doc_id_root) |> unique()
  bad_docs <- raw_all |> filter(!is.na(scoring_error)) |> pull(doc_id_root) |> unique()
  bad_docs <- setdiff(bad_docs, ok_docs)
} else {
  ok_docs <- raw_all |> filter(!is.na(schema_version)) |> pull(doc_id_root) |> unique()
  bad_docs <- character()
}
all_target <- read_parquet("data/CMP/full_data.parquet") |>
  filter(!is.na(text)) |> distinct(doc_id) |> pull(doc_id)
unscored <- setdiff(all_target, c(ok_docs, bad_docs))
message(sprintf("Status: %d ok | %d failed-with-error | %d unscored (batches not reached)",
                length(ok_docs), length(bad_docs), length(unscored)))
message(sprintf("Coverage: %.1f%% of %d target manifestos",
                100 * length(ok_docs) / length(all_target), length(all_target)))

# Keep only successfully scored rows (drop the error rows from failed batches 28-30)
raw_ok <- raw_all |> filter(doc_id_root %in% ok_docs)
spans_ok <- spans_all |> filter(doc_id_root %in% ok_docs)
write_parquet(raw_ok, "data/CMP/poplib_v4_gemini_flash_latest_raw.parquet")
write_parquet(spans_ok, "data/CMP/poplib_v4_gemini_flash_latest_spans.parquet")
message("Wrote raw + spans parquets")

# Span validation + aggregation (same logic as R/15b / R/16 / R/18 tail)
message("Validating spans (regex + judge)...")
chunk_text_lookup <- setNames(spans_ok$chunk_text, spans_ok$doc_id)
sp_for_check <- system_prompt_v4_safe("X", 2020L)
# Original-pattern call: inline sequential judge, same as R/14/R/15b/R/16/R/18.
# Slow (~5h for 16k calls) but identical to other models — no cache export issues.
validated <- validate_spans_table_with_judge(
  spans_ok |> select(doc_id, doc_id_root, dim_name, quote, context),
  chunk_text_lookup, sp_for_check, TOPIC_KEYWORDS_V4,
  use_judge = TRUE, verbose = TRUE)
write_parquet(validated, "data/CMP/poplib_v4_gemini_flash_latest_validated_spans.parquet")

message("Aggregating per-manifesto...")
V4_DIMS <- c("pop_anti_elitism","pop_people_centrism","pop_manichean","populism_overall",
             "pop_ideology_left","pop_ideology_right","pop_ideology_centrist","pop_ideology_overall",
             "lib_political","lib_social","lib_economic","lib_financial_market","liberalism_overall")
qv <- validated |> group_by(doc_id, dim_name) |>
  summarise(quality_verified = any(quality_verified, na.rm = TRUE), .groups = "drop")
aggregate_dim <- function(d) {
  sc <- paste0(d,"_score"); st <- paste0(d,"_status")
  if (!(sc %in% names(raw_ok))) return(NULL)
  raw_ok |>
    select(doc_id, doc_id_root, score = all_of(sc), status = all_of(st)) |>
    left_join(qv |> filter(dim_name == !!d) |> select(doc_id, quality_verified), by = "doc_id") |>
    mutate(quality_verified = coalesce(quality_verified, FALSE),
           eligible = quality_verified & status %in% c("verified","mixed") & !is.na(score)) |>
    group_by(doc_id_root) |>
    summarise("{d}_score" := if (any(eligible)) mean(score[eligible], na.rm = TRUE) else NA_real_,
              .groups = "drop")
}
agg <- reduce(compact(map(V4_DIMS, aggregate_dim)), full_join, by = "doc_id_root") |>
  rename(doc_id = doc_id_root)
write_parquet(agg, "data/CMP/poplib_v4_gemini_flash_latest_agg.parquet")
message(sprintf("Aggregate written: %d manifestos with scores", nrow(agg)))

# Save a "coverage status" file for the website / paper
coverage <- tibble(
  doc_id = all_target,
  gemini_status = case_when(
    doc_id %in% ok_docs ~ "scored",
    doc_id %in% bad_docs ~ "failed_quota_cap",
    TRUE ~ "not_run"
  )
)
write_parquet(coverage, "data/CMP/gemini_coverage.parquet")
message("Coverage status: ", paste(table(coverage$gemini_status), collapse = " / "))
message("DONE.")
