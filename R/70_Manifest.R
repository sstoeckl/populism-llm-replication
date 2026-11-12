# ============================================================
# 70_Manifest.R
#
# Writes PIPE_OUT/MANIFEST.md5: md5 of every input the default chain reads, of the derived
# data (span checks, baseline) and of every table, figure, csv and data file it writes.
# Logs (run times, notes) are not part of the manifest. R/71_Verify.R reruns the chain into
# a scratch directory and diffs against this file.
#
# Also records the software environment: PIPE_OUT/env/sessionInfo.txt and
# packages.csv (version of every loaded/attached package and of every package used by the
# chain scripts). renv.lock: see R/README.md (written once with renv::snapshot, without
# activating renv).
#
# Format: "md5  <path>" lines (md5sum -c compatible), "# section" comment lines.
# ============================================================

source("R/0_Config.R")

inputs <- c(
  list.files(PIPE_IN, pattern = "^(raw|validated_spans)_.*\\.parquet$", full.names = TRUE),
  file.path(PIPE_IN, c("coverage.parquet", "gap_chunks.parquet", "judge_topic_v2026a.parquet")),
  list.files(file.path(PIPE_IN, "archive"), full.names = TRUE),
  PIPE_META, PIPE_MPDS,
  "data/CMP/poplib_v4_gemini_flash_latest_validated_spans.parquet",    # June Gemini spans (fork)
  "data/CMP/update_2026a/corpus_texts_20260904101355.parquet",         # 2026-1 text (Test 2 positions)
  "website/data/manifesto_scores.parquet",                             # June baseline (comparison only)
  PIPE_PAPER_LINES,                                                    # frozen paper lines (R/48), read by R/44 + R/49
  "data/CMP/20251111-poplib_results.RData", "data/CMP/full_data.parquet", # Nov-2025 scores + metadata (R/44 bridge)
  list.files("data/external", full.names = TRUE, recursive = TRUE),    # CHES, ParlGov + SOURCES.csv (R/44a); argentina/ (PLLM-0185: CNE pdf, OCR text, register)
  list.files("data/CMP/argentina_oos", pattern = "^poplib_v4_.*\\.parquet$", full.names = TRUE),  # archived out-of-sample chunk scores (R/52) -> R/53
  "R/_archive/3_Populism.R",                                           # old prompt text quoted by R/44
  list.files("output/independence/cache/external", full.names = TRUE),
  "output/independence/tables/t3_slope_bootstrap.csv",                 # archived Test 3 (separate calls, API) -> R/46 halo macros, R/50_1 table
  "output/independence/tables/t3_slopes.csv",                          # archived Test 3 slopes -> R/50_1 table tab:indep:slopes
  "output/independence/tables/t3_slopes_subdims.csv",                  # archived Test 3 per-dimension slopes -> tab:indep:slopes dimension rows (PLLM-0244)
  list.files("output/reproducibility_v4/checkpoints", pattern = "\\.rds$", full.names = TRUE),
  "output/v2026a/env/packages.csv",                                    # reference environment (R/71 step 0)
  list.files("R", pattern = "\\.R$", full.names = TRUE), "run_all.R")
derived <- list.files(PIPE_DERIVED, pattern = "^(span_checks_.*\\.parquet|manifesto_scores_v2026a\\.(parquet|csv)|argentina_scores_oos\\.(parquet|csv))$",
                      full.names = TRUE)
outputs <- list.files(PIPE_OUT, recursive = TRUE, full.names = TRUE)
# validation/leakage/ is written by R/57 (restricted API module, PLLM-0208, not a chain stage) and is not part of the manifest
outputs <- outputs[!grepl("(^|/)logs/|(^|/)env/|(^|/)validation/leakage/|MANIFEST\\.md5$|\\.tmp$", sub(paste0("^", PIPE_OUT, "/?"), "", outputs))]

stopifnot(all(file.exists(inputs)))
sec <- function(title, files) c(paste("#", title), sprintf("%s  %s", unname(tools::md5sum(files)), files))
man <- c("# MANIFEST.md5 — v2026a canonical chain (written by R/70_Manifest.R)",
         "# Paths: inputs relative to the project root; DERIVED/ = PIPE_DERIVED, OUT/ = PIPE_OUT.",
         sec("INPUTS (read-only)", sort(inputs)),
         c("# DERIVED", { f <- sort(derived); sprintf("%s  DERIVED/%s", unname(tools::md5sum(f)), basename(f)) }),
         c("# OUTPUTS", { f <- sort(outputs); sprintf("%s  OUT/%s", unname(tools::md5sum(f)), sub(paste0("^", PIPE_OUT, "/"), "", f)) }))
write_atomic(man, file.path(PIPE_OUT, "MANIFEST.md5"), writer = function(x, p) writeLines(x, p))

# ---- Software environment ----
env_dir <- file.path(PIPE_OUT, "env"); dir.create(env_dir, showWarnings = FALSE)
used <- unique(unlist(lapply(c(list.files("R", "^[0-9].*\\.R$", full.names = TRUE), "run_all.R"), function(f) {
  x <- readLines(f, warn = FALSE)
  c(regmatches(x, gregexpr("(?<=library\\()[A-Za-z0-9.]+", x, perl = TRUE)) |> unlist(),
    regmatches(x, gregexpr("[A-Za-z][A-Za-z0-9.]+(?=::)", x, perl = TRUE)) |> unlist())
})))
used <- sort(unique(c(used, "fixest", "arrow", "dplyr", "tidyr", "purrr", "stringi", "ggplot2", "ggrepel", "furrr",
                      "future", "knitr", "modelsummary", "rlang", "jsonlite", "httr", "readr", "tidyverse", "scales")))
used <- used[vapply(used, requireNamespace, logical(1), quietly = TRUE)]
pk <- data.frame(package = used, version = vapply(used, function(p) as.character(utils::packageVersion(p)), ""),
                 stringsAsFactors = FALSE)
pk <- rbind(data.frame(package = "R", version = paste(R.version$major, R.version$minor, sep = ".")), pk)
write_csv_det(pk, file.path(env_dir, "packages.csv"))
writeLines(capture.output(sessionInfo()), file.path(env_dir, "sessionInfo.txt"))
message(sprintf("R/70: MANIFEST.md5 with %d inputs, %d derived, %d outputs", length(inputs), length(derived), length(outputs)))
