# ============================================================
# 40_Baseline.R
#
# Manifesto-level baseline for MPDS2026a / corpus 2026-1 (3,371 manifestos), built with
# the single aggregation path in R/1_Aggregation.R.
#
# Rule (Phase 0, rule (a') decided 2026-10-24, option B): verified chunks whose span passes the
# deterministic quote check (verbatim in the source chunk and not copied from the prompt) on
# all 13 dims, weighted by the dimension's own confidence block; per-model manifesto means;
# plain mean over the available models; IDEO = (R - L) / (R + L) on the cross-model means (rev3,
# 2026-10-27); the previous construction (R - L) / (R + L + C) is kept as pop_ideology_direction_c.
# Gap legs (coverage.parquet) are NA. The repaired topic check (fixed regex + judge) is NOT a
# filter: it is reported here as a diagnostic (40_topic_check_diagnostic.csv) and is a
# spec-curve fork (R/42: validation none / quote / r12 / full).
#
# Inputs : PIPE_IN raw_<m>.parquet, coverage.parquet; PIPE_DERIVED span_checks_<m>.parquet
#          (R/34); PIPE_META (MPDS2026a metadata); website/data/manifesto_scores.parquet
#          (June 2026 baseline, read-only, for the comparison only)
# Outputs: PIPE_DERIVED/manifesto_scores_v2026a.parquet (+ .csv)
#          PIPE_OUT/csv/40_*.csv (incl. 40_rule_decomposition.csv, 40_topic_check_diagnostic.csv),
#          PIPE_OUT/logs/40_baseline.txt
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow)
})
source("R/0_Config.R")
source("R/1_Aggregation.R")

VALIDATION_RULE <- Sys.getenv("BASELINE_VALIDATION", "quote")
OLD_BASELINE    <- PIPE_JUNE_BASELINE   # June 2026 (3,327 docs); optional input
log_lines <- character()
logf <- function(...) { l <- sprintf(...); message(l); log_lines <<- c(log_lines, l) }

# ---- 1. Chunk x dim table and checks --------------------------------------------
cl <- load_chunk_dims(PIPE_IN, PIPE_DERIVED)
logf("chunk-dim rows: %d (%s)", nrow(cl),
     paste(names(table(cl$model)), table(cl$model), sep = "=", collapse = ", "))

status_tab <- chunk_status_summary(cl)
write_csv_det(status_tab, file.path(PIPE_OUT, "csv", "40_chunk_status_by_model_dim.csv"))
pl <- cl |> filter(is.na(status)) |> count(model)
logf("parse-lost chunk-dims (status NA without scoring error): %s",
     if (nrow(pl)) paste(pl$model, pl$n, sep = "=", collapse = ", ") else "none")

# R/12 quality_verified on the core dims equals the plain quote check (the stored topic regex
# matched every span); verify, so that rule "r12" is exactly R/12 as stored.
ck_all <- map_dfr(AGG_FILE_STEM, ~ read_parquet(file.path(PIPE_DERIVED, paste0("span_checks_", .x, ".parquet"))) |>
                    mutate(model = .x))
r12_eq <- ck_all |> filter(dim_name %in% DIM_GROUPS$core) |>
  summarise(n = n(), agree = mean(r12_quality_verified == quote_ok))
logf("core dims: R/12 quality_verified == quote check in %.4f%% of %d spans", 100 * r12_eq$agree, r12_eq$n)
stopifnot("R/12 quality_verified must equal the quote check on the core dims" = r12_eq$agree == 1)
lrc_eq <- ck_all |> filter(dim_name %in% DIM_GROUPS$ideo_lrc, !(span_source %in% "parseloss_2026a")) |>
  summarise(n = n(), agree = mean(r12_quality_verified == (quote_ok & topic_ok)))
logf("L/R/C dims (spans with an R/12 run): R/12 quality_verified == quote & topic in %.4f%% of %d spans",
     100 * lrc_eq$agree, lrc_eq$n)
stopifnot("R/12 quality_verified must equal quote & topic on the L/R/C dims" = lrc_eq$agree == 1)
miss_judge <- ck_all |> filter(dim_name %in% DIM_GROUPS$overall, quote_ok, !(topic_rx %in% TRUE), is.na(judge)) |> nrow()
logf("*_overall spans still waiting for the judge: %d", miss_judge)
miss_core <- ck_all |> filter(dim_name %in% DIM_GROUPS$core, quote_ok, !(topic_rx %in% TRUE), is.na(judge)) |> nrow()
logf("core-dim spans still waiting for the judge: %d", miss_core)
if (miss_judge + miss_core > 0)
  stop("judge results missing (needed for the topic diagnostic and forks); run R/34 judge, then R/34 checks")

# ---- 2. Aggregate -------------------------------------------------------------------
wl <- aggregate_within(cl, validation = VALIDATION_RULE)
base <- combine_models(wl, stat = "cwm", between = "mean")

per_model <- wl |>
  mutate(col = paste0(dim, "_", model)) |>
  select(doc_id, col, cwm) |>
  pivot_wider(names_from = col, values_from = cwm)
model_cols <- as.vector(outer(AGG_DIMS, AGG_MODELS, paste, sep = "_"))
for (cc in setdiff(model_cols, names(per_model))) per_model[[cc]] <- NA_real_

cov <- read_parquet(file.path(PIPE_IN, "coverage.parquet")) |>
  mutate(model = names(AGG_FILE_STEM)[match(model, AGG_FILE_STEM)])
legs <- cov |> filter(covered) |> arrange(doc_id, match(model, AGG_MODELS)) |>
  group_by(doc_id) |>
  summarise(model_legs = paste(model, collapse = "+"), n_model_legs = n(), .groups = "drop")
src <- cov |> filter(covered) |> select(doc_id, model, sources) |>
  mutate(model = paste0("source_run_", model)) |>
  pivot_wider(names_from = model, values_from = sources)

# Model provenance per manifesto x model (chain-review #7): endpoint(s), resolved version(s),
# share of chunks whose resolved version was logged from the API response
prov_raw <- map_dfr(AGG_MODELS, function(m) read_parquet(v26_file("raw", AGG_FILE_STEM[[m]]),
                    col_select = c("doc_id_root", "endpoint", "model_requested", "model_resolved")) |> mutate(model = m))
prov <- prov_raw |> group_by(doc_id = doc_id_root, model) |>
  summarise(endpoint = paste(sort(unique(endpoint)), collapse = "+"),
            resolved = paste(sort(unique(coalesce(model_resolved, "not logged"))), collapse = "+"),
            share_logged = mean(!is.na(model_resolved) & !grepl("inferred", model_resolved)), .groups = "drop") |>
  pivot_wider(names_from = model, values_from = c(endpoint, resolved, share_logged), names_glue = "{.value}_{model}")
prov_tab <- prov_raw |> mutate(resolved_status = case_when(is.na(model_resolved) ~ "not logged",
                                                         grepl("inferred", model_resolved) ~ "inferred",
                                                         TRUE ~ "logged")) |>
  count(model, model_requested, endpoint, resolved_status, name = "chunks") |>
  group_by(model) |> mutate(share_chunks = chunks / sum(chunks)) |> ungroup()
write_csv_det(prov_tab, file.path(PIPE_OUT, "csv", "40_model_provenance.csv"))

meta <- read_parquet(PIPE_META) |>
  filter(has_text) |> distinct(doc_id, .keep_all = TRUE) |>
  transmute(doc_id, countryname, partyname, partyabbrev, party, parfam, rile,
            date = as.Date(date), year = as.integer(format(as.Date(date), "%Y")), token_count,
            update_status, text_change_type)
stopifnot(nrow(meta) == 3371L)

out <- meta |>
  left_join(base |> select(doc_id, all_of(AGG_DIMS), pop_ideology_direction = IDEO,
                           pop_ideology_direction_c = IDEO_c, pop_ideology_direction_zero = IDEO_zero, pop_ideology_direction_ge2 = IDEO_ge2,
                           pop_ideology_direction_all3 = IDEO_all3, starts_with("n_models_")), by = "doc_id") |>
  left_join(per_model |> select(doc_id, all_of(model_cols)), by = "doc_id") |>
  left_join(legs, by = "doc_id") |>
  left_join(src, by = "doc_id") |>
  left_join(prov, by = "doc_id") |>
  mutate(baseline_version = PIPE_VERSION$baseline,
         corpus_version   = PIPE_VERSION$corpus,
         aggregation      = paste0("verified chunks; span validation '", VALIDATION_RULE,
                                   "'; own-block confidence-weighted mean within model; mean across models"),
         validation_rule  = VALIDATION_RULE) |>
  arrange(countryname, party, date, doc_id)
meta_cols <- c("doc_id", "countryname", "partyname", "partyabbrev", "party", "parfam", "rile",
               "date", "year", "token_count")
out <- out |> select(all_of(meta_cols), all_of(AGG_DIMS), pop_ideology_direction, pop_ideology_direction_c,
                     pop_ideology_direction_zero, pop_ideology_direction_ge2, pop_ideology_direction_all3,
                     all_of(model_cols), starts_with("n_models_"), model_legs, n_model_legs,
                     starts_with("source_run_"), paste0("endpoint_", AGG_MODELS), paste0("resolved_", AGG_MODELS),
                     paste0("share_logged_", AGG_MODELS), update_status, text_change_type,
                     baseline_version, corpus_version, aggregation, validation_rule)
stopifnot(!anyDuplicated(out$doc_id))

f_base <- file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet")
write_atomic(out, f_base, compression = "zstd", compression_level = 9)
write_csv_det(out, file.path(PIPE_DERIVED, "manifesto_scores_v2026a.csv"))
logf("wrote %s: %d manifestos x %d columns", f_base, nrow(out), ncol(out))

cover <- tibble(var = c(AGG_DIMS, "pop_ideology_direction"),
                n_nonmissing = map_int(c(AGG_DIMS, "pop_ideology_direction"), ~ sum(!is.na(out[[.x]]))))
write_csv_det(cover, file.path(PIPE_OUT, "csv", "40_baseline_coverage.csv"))
log_lines <- c(log_lines, capture.output(print(cover, n = 20)))

# ---- 3. Old vs new: decompose into data change and rule change ----------------------
# (i)  June 2026 baseline file (corpus 2025-1 data, June rule: verified, no span check,
#      populism_confidence weights for every dim)
# (ii) v2026a data, June rule
# (iii) v2026a data, new rule (this baseline)
june_rule <- combine_models(aggregate_within(cl, validation = "none"), stat = "cwm_pop")
if (PIPE_PUBLIC || !file.exists(OLD_BASELINE)) {
  logf("June baseline comparison skipped (%s; 40_baseline_old_vs_new.csv not written)",
       if (PIPE_PUBLIC) "public mode" else paste(OLD_BASELINE, "not available"))
} else {
old <- read_parquet(OLD_BASELINE) |> mutate(IDEO = pop_ideology_direction)
cmp_vars <- c("populism_overall", "liberalism_overall", "lib_political", "lib_social",
              "lib_economic", "lib_financial_market", "pop_anti_elitism", "pop_people_centrism",
              "pop_manichean", "pop_ideology_left", "pop_ideology_right", "pop_ideology_centrist", "IDEO")
new <- out |> mutate(IDEO = pop_ideology_direction)
agree <- function(a, b, va, vb, label) {
  map_dfr(cmp_vars, function(v) {
    j <- inner_join(a |> select(doc_id, x = all_of(v)), b |> select(doc_id, y = all_of(v)), by = "doc_id") |>
      filter(!is.na(x), !is.na(y))
    tibble(comparison = label, var = v, n_a = sum(!is.na(a[[v]])), n_b = sum(!is.na(b[[v]])),
           n_both = nrow(j), cor = cor(j$x, j$y), mean_diff = mean(j$y - j$x),
           mad = mean(abs(j$y - j$x)), max_abs = max(abs(j$y - j$x)))
  })
}
diff_tab <- bind_rows(
  agree(old, june_rule, label = "data: June file -> v2026a data (June rule)"),
  agree(june_rule, new, label = "rule: June rule -> new rule (v2026a data)"),
  agree(old, new, label = "total: June file -> v2026a baseline"))
write_csv_det(diff_tab, file.path(PIPE_OUT, "csv", "40_baseline_old_vs_new.csv"))
log_lines <- c(log_lines, "", capture.output(print(diff_tab |> mutate(across(where(is.double), ~ round(.x, 3))),
                                                n = 60, width = 200)))

}

# Rule decomposition on v2026a data: which part of the new rule moves what
steps <- list(
  "June rule (verified, pop-confidence weights, no span check)" = list(v = "none",  s = "cwm_pop"),
  "+ own-block confidence weights"                              = list(v = "none",  s = "cwm"),
  "+ quote check, all dims"                                     = list(v = "quote", s = "cwm"),
  "+ topic check on L/R/C and *_overall (= rule r12, fork)"     = list(v = "r12",   s = "cwm"),
  "+ topic check on all dims (= rule full, fork)"               = list(v = "full",  s = "cwm"))
step_tab <- imap_dfr(steps, function(st, lbl) {
  b <- combine_models(aggregate_within(cl, validation = st$v), stat = st$s)
  tibble(step = lbl, n_pop = sum(!is.na(b$populism_overall)), n_lib = sum(!is.na(b$liberalism_overall)),
         n_ideo = sum(!is.na(b$IDEO)), mean_pop = mean(b$populism_overall, na.rm = TRUE),
         mean_lib = mean(b$liberalism_overall, na.rm = TRUE), mean_ideo = mean(b$IDEO, na.rm = TRUE))
})
write_csv_det(step_tab, file.path(PIPE_OUT, "csv", "40_rule_decomposition.csv"))
log_lines <- c(log_lines, "", capture.output(print(step_tab, width = 200)))

# Repaired topic check as a diagnostic: pass rate among verified chunk-dims with a score and
# a span that passes the quote check, by model x dim x score band. The pass rate rising with
# the score is why the topic check is not used as a filter (it drops low-score evidence).
topic_diag <- cl |> filter(status %in% "verified", !is.na(score), quote_ok) |>
  mutate(score_band = cut(score, c(-Inf, 2, 4, 6, Inf), labels = c("0-2", "3-4", "5-6", "7-10"))) |>
  group_by(model, dim, score_band) |>
  summarise(n = n(), topic_pass = mean(topic_ok), .groups = "drop") |>
  arrange(dim, model, score_band)
topic_all <- cl |> filter(status %in% "verified", !is.na(score), quote_ok) |>
  group_by(model, dim) |> summarise(n = n(), topic_pass = mean(topic_ok), .groups = "drop") |>
  mutate(score_band = "all")
write_csv_det(bind_rows(topic_all, topic_diag) |> arrange(dim, model, score_band),
              file.path(PIPE_OUT, "csv", "40_topic_check_diagnostic.csv"))
writeLines(log_lines, file.path(PIPE_OUT, "logs", "40_baseline.txt"))
