# =============================================================
# 20_Spec_Curve_Compute.R
#
# Pre-compute the full specification specs for the interactive
# regression module. ~9k regressions stored as a small parquet
# that the website can lookup-filter without any compute.
#
# Forks (the analytic choices):
#   1. Model set:          7 options
#   2. Within-manifest agg: 3 options
#   3. Between-model agg:   3 options
#   4. Outcome dimension:   5 options
#   5. Sample restriction:  4 options
#   6. Fixed effects:       3 options
#   7. SE cluster:          3 options
# = 7 × 3 × 3 × 5 × 4 × 3 × 3 = 11,340 specs (with some skipped due to invalid combos)
# =============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(fixest); library(furrr)
})

# ---- Load cross-LLM long-form data ----
load_model_long <- function(model_id, path) {
  if (!file.exists(path)) return(NULL)
  raw <- read_parquet(path)
  V4_DIMS <- c("populism_overall","pop_anti_elitism","pop_people_centrism","pop_manichean",
               "pop_ideology_overall",
               "lib_political","lib_social","lib_economic","lib_financial_market",
               "liberalism_overall")
  rows <- raw |> distinct(doc_id_root) |> rename(doc_id = doc_id_root)
  for (d in V4_DIMS) {
    sc <- paste0(d,"_score"); st <- paste0(d,"_status")
    if (!sc %in% names(raw)) next
    # Confidence-weighted mean per doc (default agg). Also keep plain + median.
    weighted <- raw |>
      filter(.data[[st]] == "verified", !is.na(.data[[sc]])) |>
      group_by(doc_id_root) |>
      summarise(
        !!paste0(d, "_cwm")    := weighted.mean(.data[[sc]], w = pmax(populism_confidence, 0.1), na.rm = TRUE),
        !!paste0(d, "_mean")   := mean(.data[[sc]], na.rm = TRUE),
        !!paste0(d, "_median") := median(.data[[sc]], na.rm = TRUE),
        .groups = "drop") |>
      rename(doc_id = doc_id_root)
    rows <- left_join(rows, weighted, by = "doc_id")
  }
  rows |> mutate(model = model_id, .before = 1)
}

cat("Loading per-model scores...\n")
sonnet  <- load_model_long("sonnet",     "data/CMP/poplib_v4_claude_full_raw.parquet")
gpt41   <- load_model_long("gpt41mini",  "data/CMP/poplib_v4_gpt41mini_raw.parquet")
gemini  <- load_model_long("gemini",     "data/CMP/poplib_v4_gemini_flash_latest_raw.parquet")

# Manifesto metadata for FE / sample restriction
meta <- read_parquet("data/CMP/full_data.parquet",
                     col_select = c("doc_id","countryname","party","date","parfam")) |>
  mutate(date = as.Date(date), year = lubridate::year(date))

# Build a long table: one row per (doc × model × within_agg × dim)
combined <- bind_rows(sonnet, gpt41, gemini) |>
  pivot_longer(-c(model, doc_id), names_to = "key", values_to = "score") |>
  separate(key, c("dim", "within_agg"), sep = "_(?=cwm$|mean$|median$)") |>
  filter(!is.na(score), is.finite(score))
cat("Combined long rows:", nrow(combined), "\n")
cat("Distinct manifests:", length(unique(combined$doc_id)), "\n")

# ---- Define the spec specs ----
MODEL_SETS <- list(
  sonnet_only      = c("sonnet"),
  gpt41_only       = c("gpt41mini"),
  gemini_only      = c("gemini"),
  sonnet_gpt41     = c("sonnet","gpt41mini"),
  sonnet_gemini    = c("sonnet","gemini"),
  gpt41_gemini     = c("gpt41mini","gemini"),
  all_three        = c("sonnet","gpt41mini","gemini")
)
WITHIN_AGG  <- c("cwm","mean","median")
BETWEEN_AGG <- c("mean","median","leave_worst_out")
OUTCOMES    <- c("liberalism_overall","lib_political","lib_social","lib_economic","lib_financial_market")
SAMPLES     <- list(
  full       = quote(year >= 1945),
  post2000   = quote(year >= 2000),
  post2010   = quote(year >= 2010),
  europe_only = quote(year >= 1945 & countryname %in% c(
    "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark",
    "Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy",
    "Latvia","Lithuania","Luxembourg","Malta","Netherlands","Norway","Poland",
    "Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom"))
)
FE_SPECS <- list(
  ctry_year   = "countryname + year",
  ctry_year_p = "countryname + year + party",
  ctry_x_year = "countryname^year"
)
SE_CLUSTERS <- list(
  country     = "countryname",
  ctry_year   = "countryname^year",
  party       = "party"
)

between_combine <- function(scores, method) {
  scores <- scores[is.finite(scores)]
  if (length(scores) == 0) return(NA_real_)
  if (length(scores) == 1) return(scores[1])
  switch(method,
    mean             = mean(scores),
    median           = median(scores),
    leave_worst_out  = {
      drop_idx <- which.max(abs(scores - mean(scores)))
      mean(scores[-drop_idx])
    },
    mean(scores))
}

# Prepare per (within_agg × model_set × between_agg × outcome × populism) data
# This is the cartesian aggregation step. To keep it fast, cache by within_agg.
make_dataset <- function(model_ids, within_agg, between_agg, outcome_dim) {
  # Score for populism + outcome dimension for each manifesto in this model set
  sub <- combined |>
    filter(model %in% model_ids,
           within_agg == !!within_agg,
           dim %in% c("populism_overall", outcome_dim))
  # Aggregate across models per doc × dim
  agg <- sub |>
    group_by(doc_id, dim) |>
    summarise(score = between_combine(score, between_agg), .groups = "drop") |>
    pivot_wider(names_from = dim, values_from = score)
  # Merge metadata
  agg <- agg |> inner_join(meta, by = "doc_id") |>
    filter(is.finite(populism_overall), is.finite(.data[[outcome_dim]]))
  agg
}

# Run one spec
run_spec <- function(model_ids, within_agg, between_agg, outcome_dim,
                     sample_expr, fe_str, se_cluster) {
  dat <- make_dataset(model_ids, within_agg, between_agg, outcome_dim)
  dat <- dat |> filter(eval(sample_expr))
  if (nrow(dat) < 100) return(NULL)
  fml <- as.formula(sprintf("%s ~ populism_overall | %s", outcome_dim, fe_str))
  fit <- tryCatch(
    feols(fml, data = dat, cluster = as.formula(paste0("~", se_cluster))),
    error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  s <- coeftable(fit)
  if (!"populism_overall" %in% rownames(s)) return(NULL)
  list(
    coef = s["populism_overall","Estimate"],
    se   = s["populism_overall","Std. Error"],
    t    = s["populism_overall","t value"],
    p    = s["populism_overall","Pr(>|t|)"],
    n    = nobs(fit),
    r2_w = tryCatch(fitstat(fit, "r2", simplify = TRUE)[["r2"]], error = function(e) NA_real_)
  )
}

# Generate the specs
specs <- expand_grid(
  model_set   = names(MODEL_SETS),
  within_agg  = WITHIN_AGG,
  between_agg = BETWEEN_AGG,
  outcome     = OUTCOMES,
  sample_key  = names(SAMPLES),
  fe_key      = names(FE_SPECS),
  se_key      = names(SE_CLUSTERS)
) |>
  # Skip illogical combos: 1-model sets don't need a "between_agg" choice
  filter(!(map_lgl(model_set, ~ length(MODEL_SETS[[.x]]) == 1) & between_agg != "mean"))
cat("Total specs to run:", nrow(specs), "\n")

# Compute (parallel)
plan(multisession, workers = 4L)
res <- future_map_dfr(seq_len(nrow(specs)), function(i) {
  r <- specs[i, ]
  out <- run_spec(
    MODEL_SETS[[r$model_set]],
    r$within_agg, r$between_agg, r$outcome,
    SAMPLES[[r$sample_key]], FE_SPECS[[r$fe_key]], SE_CLUSTERS[[r$se_key]]
  )
  if (is.null(out)) return(tibble())
  bind_cols(r, as_tibble(out))
}, .options = furrr_options(seed = TRUE,
                            globals = c("specs","MODEL_SETS","SAMPLES","FE_SPECS","SE_CLUSTERS",
                                        "WITHIN_AGG","BETWEEN_AGG","OUTCOMES","combined","meta",
                                        "make_dataset","between_combine","run_spec"),
                            packages = c("dplyr","tidyr","fixest","purrr")),
  .progress = TRUE)
plan(sequential)

cat("Successful specs:", nrow(res), "\n")
write_parquet(res, "website/data/specs_grid.parquet",
              compression = "zstd", compression_level = 9)
cat(sprintf("Saved website/data/specs_grid.parquet (%.1f KB)\n",
            file.size("website/data/specs_grid.parquet") / 1024))

# Headline summary stats
cat("\n=== Bottom-line populism coefficient distribution ===\n")
cat(sprintf("  Mean:    %+.3f\n", mean(res$coef, na.rm=TRUE)))
cat(sprintf("  Median:  %+.3f\n", median(res$coef, na.rm=TRUE)))
cat(sprintf("  5/95%%:   [%+.3f, %+.3f]\n",
            quantile(res$coef, 0.05, na.rm=TRUE), quantile(res$coef, 0.95, na.rm=TRUE)))
cat(sprintf("  %% sig negative (p<.05): %.1f%%\n",
            100 * mean(res$p < 0.05 & res$coef < 0, na.rm = TRUE)))
cat(sprintf("  %% sign-flipped (positive): %.1f%%\n",
            100 * mean(res$coef > 0, na.rm = TRUE)))
