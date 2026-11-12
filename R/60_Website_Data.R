# ============================================================
# 60_Website_Data.R
#
# Data files for the inspector website (website/ submodule, Quarto), built from the same
# aggregation path as the paper (R/1_Aggregation.R via the R/40 baseline) and the R/42 grid.
# Drop-in replacements, same file names and columns, for the files the site reads in
# website/data/; written to PIPE_OUT/website_data/ (this script never writes into website/).
#
#   manifesto_scores.parquet/.csv  = the v2026a baseline (one row per manifesto)
#   agg_long.parquet               per (manifesto x model x dim) canonical within-model score.
#                                  The country / party pages average it across models
#                                  (plain mean over available models), which reproduces the
#                                  baseline exactly because nothing is rounded.
#   baselines.parquet              GLOBAL and COUNTRY means per model x dim (party pages)
#   manifestos_meta.parquet        all MPDS2026a rows (incl. manifestos without text)
#   specs_grid.parquet             R/42 grid, estimable cells (website regression module),
#                                  plus specs_distinct.parquet
#   version.json                   version stamp shown on every page (Phase 4)
#
# Replaces website/data/_build_baseline.R (June rule) and website/data/_build_agg.R (read the
# *_agg files: verified|mixed & quality_verified, plain mean, empty *_overall dims). Phase 4
# switches the site to these files; until then the site still shows the June numbers.
# ============================================================

suppressPackageStartupMessages({ library(tidyverse); library(arrow); library(jsonlite) })
source("R/0_Config.R")
source("R/1_Aggregation.R")

WEB <- file.path(PIPE_OUT, "website_data"); dir.create(WEB, showWarnings = FALSE)
wp  <- function(x, f) write_atomic(x, file.path(WEB, f), compression = "zstd", compression_level = 9)

base_file <- file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet")
base <- read_parquet(base_file)
RULE <- base$validation_rule[1]
wp(base, "manifesto_scores.parquet")
write_csv_det(base, file.path(WEB, "manifesto_scores.csv"))

# Per-model manifesto scores, exactly the columns <dim>_<model> of the baseline
agg_long <- map_dfr(AGG_MODELS, function(m) {
  base |> select(doc_id, all_of(paste0(AGG_DIMS, "_", m))) |>
    rename_with(~ sub(paste0("_", m, "$"), "", .x)) |>
    pivot_longer(-doc_id, names_to = "dim", values_to = "score") |>
    filter(is.finite(score)) |> mutate(model = m)
}) |> arrange(doc_id, dim, model)
wp(agg_long, "agg_long.parquet")

# Check: page-level cross-model mean == baseline
chk <- agg_long |> group_by(doc_id, dim) |> summarise(score = mean(score), .groups = "drop") |>
  inner_join(base |> select(doc_id, all_of(AGG_DIMS)) |> pivot_longer(-doc_id, names_to = "dim", values_to = "b"),
             by = c("doc_id", "dim"))
stopifnot("website cross-model mean must equal the baseline" = max(abs(chk$score - chk$b)) < 1e-12)

meta <- read_parquet(PIPE_META) |>
  transmute(doc_id, manifesto_id, countryname, partyname, partyabbrev, date = as.Date(date),
            edate = as.Date(edate), token_count, parfam, rile, party, year = as.integer(format(as.Date(date), "%Y"))) |>
  distinct(doc_id, .keep_all = TRUE) |> arrange(doc_id)
wp(meta, "manifestos_meta.parquet")

baselines <- agg_long |> left_join(meta |> select(doc_id, countryname), by = "doc_id")
baselines <- bind_rows(
  baselines |> group_by(model, dim) |> summarise(mean = mean(score), n = n(), .groups = "drop") |>
    mutate(scope = "GLOBAL", country = NA_character_),
  baselines |> group_by(country = countryname, model, dim) |> summarise(mean = mean(score), n = n(), .groups = "drop") |>
    mutate(scope = "COUNTRY")) |>
  select(scope, country, model, dim, mean, n) |> arrange(scope, country, model, dim)
wp(baselines, "baselines.parquet")

grid <- read_parquet(file.path(PIPE_OUT, "spec_curve", "specs_grid_v2026a.parquet")) |> filter(status == "ok")
wp(grid, "specs_grid.parquet")
wp(read_parquet(file.path(PIPE_OUT, "spec_curve", "specs_distinct_v2026a.parquet")), "specs_distinct.parquet")

version <- list(baseline_version = PIPE_VERSION$baseline, corpus = PIPE_VERSION$corpus,
                models = PIPE_VERSION$models, judge = PIPE_VERSION$judge, span_validation_rule = RULE,
                baseline_md5 = unname(tools::md5sum(base_file)), n_manifestos = nrow(base))
write_atomic(toJSON(version, auto_unbox = TRUE, pretty = TRUE), file.path(WEB, "version.json"),
             writer = function(x, p) writeLines(x, p))
message(sprintf("R/60: website data in %s (%d manifestos, %d agg rows, %d grid cells)", WEB, nrow(base),
                nrow(agg_long), nrow(grid)))
