# ============================================================
# 33_CMP2026a_Merge.R
#
# Phase 1 step 4 (PLAN.md): merged per-model raw chunk files for the full
# MPDS2026a / corpus 2026-1 scoring pool (3,371 manifestos), with provenance.
#
# Sources per model (claude / gpt41mini / gemini):
#   june   : data/CMP/poplib_v4_{claude_full,gpt41mini,gemini_flash_latest}_raw.parquet
#            -> only successful chunks of manifestos whose text is unchanged in 2026-1
#   update : data/CMP/update_2026a/poplib_v4_<m>_raw.parquet        (Oct 7/8; 83 docs)
#   norm   : data/CMP/update_2026a/poplib_v4_<m>_norm_raw.parquet   (131 normalisation-only)
#   retry  : data/CMP/update_2026a/poplib_v4_<m>_retry_raw.parquet  (failed June chunks)
# Party re-codes with identical text keep their June rows, re-keyed to the 2026a doc_id.
#
# Dropped rows (June error rows incl. the Gemini quota-error rows, and June rows
# superseded by 2026-1 re-scores) are archived with a drop_reason.
#
# Outputs (new directory only):
#   data/CMP/v2026a/raw_<m>.parquet                 merged chunk rows + provenance
#   data/CMP/v2026a/validated_spans_<m>.parquet     R/12 span validation, same row set
#   data/CMP/v2026a/archive/dropped_rows_<m>.parquet
#   data/CMP/v2026a/coverage.parquet                doc_id x model coverage + reason
# Baseline aggregation is NOT built here (Phase 2).
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow)
})

UPD   <- "data/CMP/update_2026a"
OUT   <- Sys.getenv("MERGE_OUT", "data/CMP/v2026a")   # override only to re-check the merge into scratch
ARCH  <- file.path(OUT, "archive")
dir.create(ARCH, recursive = TRUE, showWarnings = FALSE)
stopifnot("refusing to overwrite merged files" = !file.exists(file.path(OUT, "raw_claude.parquet")))

# atomic writes: temp file in the same directory, then rename
write_atomic <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  write_parquet(x, tmp)
  stopifnot(file.rename(tmp, path))
}

# Classify the last scoring error of a failed chunk (one line per chunk)
classify_error <- function(err, model) {
  case_when(
    grepl("Timeout was reached", err) ~ "timeout (180 s, all retries)",
    grepl("429|RESOURCE_EXHAUSTED|quota", err, ignore.case = TRUE) ~ "429 / quota (rate)",
    grepl("SAFETY|blocked", err, ignore.case = TRUE) ~ "safety block",
    err == "unknown" & model == "gpt41mini" ~
      "truncated JSON: output hit max_output_tokens=5000 (diagnosed 2026-10-14)",
    err == "unknown" ~ "invalid JSON on all retries (likely truncated output; not diagnosed)",
    TRUE ~ paste("other:", substr(err, 1, 80)))
}

MODELS <- tribble(
  ~m,          ~june_raw,                                              ~june_spans,                                                         ~june_ckpt,                                       ~june_requested,            ~june_resolved,                  ~provider,
  "claude",    "data/CMP/poplib_v4_claude_full_raw.parquet",           "data/CMP/poplib_v4_claude_full_validated_spans.parquet",            "data/CMP/v4_claude_full_checkpoints",            "claude-sonnet-4-6",        NA_character_,                   "anthropic",
  "gpt41mini", "data/CMP/poplib_v4_gpt41mini_raw.parquet",             "data/CMP/poplib_v4_gpt41mini_validated_spans.parquet",              "data/CMP/v4_gpt41mini_checkpoints",              "gpt-4.1-mini-2025-04-14",  NA_character_,                   "openai",
  "gemini",    "data/CMP/poplib_v4_gemini_flash_latest_raw.parquet",   "data/CMP/poplib_v4_gemini_flash_latest_validated_spans.parquet",    "data/CMP/v4_gemini_flash_latest_checkpoints",    "gemini-flash-latest",      "gemini-3.5-flash (inferred)",   "google"
)

# All 2026-10 scoring checkpoints (source of the *_raw parquets) must load cleanly
ck_files <- list.files(UPD, pattern = "^batch_\\d+\\.RData$", recursive = TRUE, full.names = TRUE)
ck_files <- ck_files[!grepl("/corrupt/", ck_files)]
bad <- ck_files[!map_lgl(ck_files, function(f) tryCatch({ load(f, envir = new.env()); TRUE },
                                                       error = function(e) FALSE))]
if (length(bad)) stop("unreadable checkpoints: ", paste(bad, collapse = ", "))
message("checkpoints validated: ", length(ck_files))

status <- read_parquet(file.path(UPD, "update_status.parquet"))
pool   <- read_parquet("data/CMP/full_data_2026a_meta.parquet") |>
  filter(has_text) |> distinct(doc_id) |> pull(doc_id)
stopifnot(length(pool) == 3371L)
# June rows are valid for 2026-1 only if the text is byte-identical
june_valid_docs <- status |> filter(update_status %in% c("unchanged", "changed_meta", "recoded")) |>
  mutate(doc_2026 = doc_id, doc_june = coalesce(old_doc_id, doc_id)) |> select(doc_june, doc_2026)

usage <- list.files(file.path(UPD, "usage_logs"), "^usage_v2_", full.names = TRUE) |>
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c")))

june_timestamps <- function(dir) {
  fs <- list.files(dir, pattern = "^batch_\\d+\\.RData$", full.names = TRUE)
  map_dfr(fs, function(f) {
    e <- new.env()
    # every checkpoint must load cleanly (lesson 2026-10-13: a kill during save()
    # left a truncated checkpoint) -- fail loudly rather than silently drop rows
    tryCatch(load(f, envir = e), error = function(err) stop("unreadable checkpoint: ", f))
    r <- e$batch_result$raw
    if (!nrow(r)) return(tibble())
    ok <- if ("scoring_error" %in% names(r)) is.na(r$scoring_error) & !is.na(r$schema_version) else !is.na(r$schema_version)
    tibble(doc_id = r$doc_id[ok], scored_at = format(e$batch_result$timestamp, "%Y-%m-%d %H:%M:%S"))
  }) |> group_by(doc_id) |> slice_tail(n = 1) |> ungroup()
}

is_ok <- function(r) {
  err <- if ("scoring_error" %in% names(r)) !is.na(r$scoring_error) else rep(FALSE, nrow(r))
  !err & !is.na(r$schema_version)
}

coverage <- list(); full_rescored <- list(); gap_chunks <- list()
# Expected chunks per manifesto (2026-10-26, chain-review #17): distinct chunk ids over ALL three
# June files (one shared chunker; a model's missing rows can then not lower the expectation and
# hide a gap). Previously the row count of the Claude file was used for every model.
june_n_chunks_all <- map_dfr(MODELS$june_raw, ~ read_parquet(.x, col_select = c("doc_id_root", "chunk_id"))) |>
  distinct(doc_id_root, chunk_id) |> count(doc_id_root, name = "n_chunks_june")
for (k in seq_len(nrow(MODELS))) {
  M <- MODELS[k, ]; m <- M$m
  message("== ", m)

  # ---- June ----
  june <- read_parquet(M$june_raw) |> mutate(.ok = is_ok(pick(everything())))
  june_ts <- june_timestamps(M$june_ckpt)
  june_ok <- june |> filter(.ok) |> distinct(doc_id, .keep_all = TRUE)
  keep_june <- june_ok |> inner_join(june_valid_docs, by = c("doc_id_root" = "doc_june"))
  drop_june <- bind_rows(
    june |> filter(!.ok) |> mutate(drop_reason = "june_error_row"),
    june |> filter(.ok) |> anti_join(keep_june |> select(doc_id), by = "doc_id") |>
      mutate(drop_reason = "june_superseded_by_2026-1_text"),
    june |> filter(.ok) |> filter(duplicated(doc_id)) |> mutate(drop_reason = "june_duplicate_row")
  ) |> select(-.ok)
  keep_june <- keep_june |>
    left_join(june_ts, by = "doc_id") |>
    mutate(orig_doc_id = doc_id,
           doc_id = paste0(doc_2026, sub("^.*(_ch\\d+)$", "\\1", doc_id)),   # re-key party re-codes
           doc_id_root = doc_2026,
           source_run = "june2026", scoring_target = "june",
           endpoint = c(anthropic = "anthropic-api", openai = "openai-api", google = "aistudio")[[M$provider]],
           model_requested = M$june_requested,
           model_resolved = M$june_resolved, corpus_version = "2025-1 (text identical in 2026-1)") |>
    select(-.ok, -doc_2026)

  # ---- 2026-1 re-scores ----
  rescored <- map_dfr(c(update = "", norm = "_norm", retry = "_retry"), function(sfx) {
    f <- file.path(UPD, paste0("poplib_v4_", m, sfx, "_raw.parquet"))
    if (!file.exists(f)) return(tibble())
    read_parquet(f) |> mutate(.src = sfx)
  }, .id = "target")
  if (nrow(rescored)) {
    ts <- usage |> filter(str_starts(provider, M$provider), model != "gpt-4o-mini-2024-07-18") |>
      arrange(ts) |> group_by(doc_id) |> slice_tail(n = 1) |> ungroup() |>
      transmute(doc_id, scored_at = ts, backend = provider)
    rescored <- rescored |>
      filter(is_ok(pick(everything()))) |>
      left_join(ts, by = "doc_id") |>
      mutate(orig_doc_id = doc_id,
             scoring_target = target,
             endpoint = case_when(backend == "google-vertex" ~ "vertex-global",
                                  backend == "google" ~ "aistudio",
                                  backend == "anthropic" ~ "anthropic-api",
                                  backend == "openai" ~ "openai-api"),
             source_run = if_else(endpoint == "vertex-global", "vertex_2026-10",
                                  paste0(target, "_2026-10")),
             model_requested = model_id, model_resolved = resolved_model,
             corpus_version = if_else(target == "retry", "2025-1 (text identical in 2026-1)", "2026-1")) |>
      select(-target, -.src, -backend, -model_id, -resolved_model)
    # retries replace only their own chunk ids; full re-scores replace whole manifestos
    full_docs <- rescored |> filter(scoring_target != "retry") |> distinct(doc_id_root) |> pull()
    keep_june <- keep_june |> filter(!doc_id_root %in% full_docs)
  }
  merged <- bind_rows(keep_june |> filter(!doc_id %in% rescored$doc_id), rescored) |>
    arrange(doc_id_root, chunk_id)
  stopifnot(!anyDuplicated(merged$doc_id))
  if (!"scoring_error" %in% names(merged)) merged$scoring_error <- NA_character_
  stopifnot(all(is.na(merged$scoring_error)))

  full_rescored[[m]] <- if (nrow(rescored)) full_docs else character()

  # ---- documented gaps: a manifesto x model leg enters only if COMPLETE ----
  # Failed chunks from the 2026-10 runs (last attempt per chunk, all targets)
  ck <- list.files(UPD, pattern = "^batch_\\d+\\.RData$", recursive = TRUE, full.names = TRUE)
  ck <- ck[grepl(paste0("/checkpoints_", m, "(_norm|_retry)?/"), ck)]
  att <- map_dfr(seq_along(ck), function(i) {
    e <- new.env(); load(ck[i], envir = e); r <- e$batch_result$raw
    if (!"scoring_error" %in% names(r)) r$scoring_error <- NA_character_
    # order attempts by the timestamp stored IN the checkpoint, then the batch number (not file
    # mtime, which a copy or touch can change; chain-review #17)
    r |> transmute(doc_id, doc_id_root, chunk_id, scoring_error,
                   .t = as.numeric(e$batch_result$timestamp %||% NA),
                   .b = as.integer(sub("^batch_(\\d+)\\.RData$", "\\1", basename(ck[i]))), .i = i)
  }) |> arrange(.t, .b, .i) |> group_by(doc_id) |> slice_tail(n = 1) |> ungroup()
  stopifnot("every 2026-10 checkpoint must carry its timestamp" = !anyNA(att$.t))
  failed_chunks <- att |> filter(!is.na(scoring_error)) |>
    mutate(model = m, gap_class = classify_error(scoring_error, m))
  expected <- bind_rows(
    june_n_chunks_all |> inner_join(june_valid_docs, by = c("doc_id_root" = "doc_june")) |>
      filter(!doc_2026 %in% full_docs) |> select(doc_id_root = doc_2026, n_expected = n_chunks_june),
    att |> filter(doc_id_root %in% c(full_docs, failed_chunks$doc_id_root[!failed_chunks$doc_id_root %in% merged$doc_id_root])) |>
      group_by(doc_id_root) |> summarise(n_expected = max(chunk_id), .groups = "drop")
  ) |> distinct(doc_id_root, .keep_all = TRUE)
  incomplete <- merged |> count(doc_id_root, name = "n_ok") |>
    left_join(expected, by = "doc_id_root") |> filter(n_ok < n_expected) |> pull(doc_id_root)
  if (length(incomplete)) {
    drop_june <- bind_rows(drop_june, merged |> filter(doc_id_root %in% incomplete) |>
                             mutate(drop_reason = "incomplete_leg_documented_gap"))
    merged <- merged |> filter(!doc_id_root %in% incomplete)
  }
  gap_chunks[[m]] <- failed_chunks |> filter(!doc_id_root %in% merged$doc_id_root) |> select(-any_of(c(".t", ".b", ".i")))
  message("  incomplete legs set to NA: ", length(incomplete), " ", paste(incomplete, collapse = ", "))
  # June AI-Studio Gemini rows of the 131 normalisation-only docs, kept for the
  # Phase-2 sensitivity check (June-Gemini vs Vertex-Gemini)
  if (m == "gemini") {
    norm_docs <- status |> filter(text_change_type == "normalisation_only") |> pull(doc_id)
    write_atomic(june_ok |> filter(doc_id_root %in% norm_docs) |> select(-.ok) |>
                    mutate(source_run = "june2026", endpoint = "aistudio",
                           model_requested = M$june_requested, model_resolved = M$june_resolved),
                  file.path(ARCH, "gemini_june_superseded.parquet"))
  }
  write_atomic(merged, file.path(OUT, paste0("raw_", m, ".parquet")))
  write_atomic(drop_june, file.path(ARCH, paste0("dropped_rows_", m, ".parquet")))
  message(sprintf("  merged rows %d (%d docs) | archived %d [%s]", nrow(merged), n_distinct(merged$doc_id_root),
                  nrow(drop_june), paste(names(table(drop_june$drop_reason)), table(drop_june$drop_reason),
                                         sep = "=", collapse = ", ")))

  # ---- validated spans for exactly the merged chunk set ----
  sp_june <- read_parquet(M$june_spans) |> semi_join(keep_june |> select(doc_id = orig_doc_id), by = "doc_id") |>
    left_join(keep_june |> select(orig_doc_id, doc_id_new = doc_id, doc_id_root_new = doc_id_root),
              by = c("doc_id" = "orig_doc_id")) |>
    mutate(doc_id = doc_id_new, doc_id_root = doc_id_root_new) |> select(-doc_id_new, -doc_id_root_new) |>
    filter(doc_id %in% merged$doc_id) |> mutate(span_source = "june2026")
  sp_new <- map_dfr(c("", "_norm", "_retry"), function(sfx) {
    f <- file.path(UPD, paste0("poplib_v4_", m, sfx, "_validated_spans.parquet"))
    if (file.exists(f)) read_parquet(f) |> mutate(span_source = paste0(if (sfx == "") "update" else sub("_", "", sfx), "_2026a"))
    else tibble()
  }) |> filter(doc_id %in% merged$doc_id)
  write_atomic(bind_rows(sp_june, sp_new), file.path(OUT, paste0("validated_spans_", m, ".parquet")))

  # ---- coverage ----
  june_n_chunks <- june_n_chunks_all
  cov <- tibble(doc_id = pool) |>
    left_join(merged |> count(doc_id = doc_id_root, name = "n_chunks_ok"), by = "doc_id") |>
    left_join(merged |> group_by(doc_id = doc_id_root) |>
                summarise(sources = paste(sort(unique(source_run)), collapse = "+"),
                          max_chunk = max(chunk_id), .groups = "drop"), by = "doc_id") |>
    left_join(june_n_chunks |> inner_join(june_valid_docs, by = c("doc_id_root" = "doc_june")) |>
                select(doc_id = doc_2026, n_chunks_june), by = "doc_id") |>
    mutate(model = m, n_chunks_ok = coalesce(n_chunks_ok, 0L),
           covered = n_chunks_ok > 0,
           # June-text docs must have all June chunks; full 2026-1 re-scores are
           # complete by construction (R/32 finalize keeps only fully parsed docs)
           n_expected = if_else(doc_id %in% full_rescored[[m]], max_chunk, n_chunks_june),
           chunk_gaps = covered & n_chunks_ok < coalesce(n_expected, max_chunk))
  gr <- gap_chunks[[m]] |> group_by(doc_id = doc_id_root) |>
    summarise(gap_reason = paste0(n(), " chunk(s) failed: ",
                                  paste(sort(unique(gap_class)), collapse = "; ")), .groups = "drop")
  cov <- cov |> left_join(gr, by = "doc_id") |>
    mutate(gap_reason = if_else(!covered & is.na(gap_reason), "no scoring attempt found", gap_reason))
  coverage[[m]] <- cov
}

cov <- bind_rows(coverage)
write_atomic(cov, file.path(OUT, "coverage.parquet"))
write_atomic(bind_rows(gap_chunks), file.path(OUT, "gap_chunks.parquet"))
cat("\nCoverage (doc_id x model, pool = 3,371):\n")
print(cov |> group_by(model) |> summarise(n_covered = sum(covered), n_gap = sum(!covered),
                                          docs_with_chunk_gaps = sum(chunk_gaps, na.rm = TRUE),
                                          .groups = "drop"))
cat("\nSource mix (docs):\n")
print(cov |> count(model, sources) |> pivot_wider(names_from = model, values_from = n, values_fill = 0), n = 30)
missing <- cov |> filter(!covered | chunk_gaps)
if (nrow(missing)) { cat("\nNot fully covered:\n"); print(missing, n = 50) }
