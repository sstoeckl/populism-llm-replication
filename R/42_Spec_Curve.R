# ============================================================
# 42_Spec_Curve.R
#
# Specification curve / multiverse on the v2026a data (paper Appendix C), recomputed with
# the single aggregation path (R/1_Aggregation.R). Replaces R/_archive/20_Spec_Curve_Compute.R.
#
# Estimands
#   pop    beta on POP in   LIB ~ POP | FE                      (paper eq. C.1)
#   inter  beta on POPxIDEO in LIB ~ POP * IDEO | FE            (paper eq. 1, column 5)
#
# Forks
#   measurement  model_set    7 (each single model, each pair, all three)
#                within_agg   cwm (own-block confidence weights) | cwm_pop (populism confidence
#                             for every dim, June rule) | mean | median
#                validation   quote (baseline: verbatim + not from the prompt, all dims) |
#                             none (no span check) | r12 (+ topic check on L/R/C and *_overall) |
#                             full (+ repaired topic check on all dims: fixed regex + judge)
#                gemini_norm  vertex | june  (Gemini rows of the 131 normalisation-only docs)
#                gap_legs     na | june_fallback (fills the 3 documented Gemini gap legs)
#                between_agg  mean | median | leave_worst_out; only defined for all_three:
#                             with one model there is nothing to combine, and with two models
#                             median = mean and leave-worst-out is undefined (the two
#                             deviations tie), so those cells would only duplicate "mean".
#   analytic     outcome      5 liberalism dimensions
#                sample       full | post2000 | post2010 | europe_only
#                fe           country + year | + party | country x year
#                se_cluster   country | country x year | party   (inference only: never changes beta)
#
# Combinations that cannot change the data are still listed (e.g. gemini_norm for
# sonnet_only) so the grid is a full product within each model set, but every cell carries a
# `data_key` and `beta_key`; distinct specifications are counted on the regression actually
# run (data x outcome x sample x fe), never on the cell count. There are no structurally
# removed cells; a cell that cannot be estimated (fewer than 100 complete cases, fewer than 100
# obs left after fixest drops singleton fixed-effect groups, or a fixest error) is kept with
# status "failed" and its reason, and excluded from the distributions.
#
# Outputs: PIPE_OUT/spec_curve/specs_grid_v2026a.parquet (one row per cell x estimand)
#          PIPE_OUT/csv/42_*.csv, PIPE_OUT/tables/tab_speccurve_*.tex,
#          PIPE_OUT/figures/spec_curve*.pdf, PIPE_OUT/logs/42_spec_curve.txt
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(fixest); library(furrr); library(scales)
})
source("R/0_Config.R")
source("R/1_Aggregation.R")
SC <- file.path(PIPE_OUT, "spec_curve"); dir.create(SC, showWarnings = FALSE)
RULE <- read_parquet(file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet"), col_select = "validation_rule")$validation_rule[1]

MODEL_SETS <- list(sonnet_only = "sonnet", gpt41_only = "gpt41mini", gemini_only = "gemini",
                   sonnet_gpt41 = c("sonnet", "gpt41mini"), sonnet_gemini = c("sonnet", "gemini"),
                   gpt41_gemini = c("gpt41mini", "gemini"), all_three = AGG_MODELS)
WITHIN   <- c("cwm", "cwm_pop", "mean", "median")
OUTCOMES <- c("liberalism_overall", "lib_political", "lib_social", "lib_economic", "lib_financial_market")
# Europe sample = EU27 + EFTA (Iceland, Norway, Switzerland; Liechtenstein has no CMP data) +
# United Kingdom. 2026-10-26 (chain-review #11): Iceland was missing; names are now asserted
# against MPDS, and the European CMP countries outside the definition are listed in
# csv/42_europe_definition.csv (Albania, Bosnia-Herzegovina, Moldova, Montenegro, North
# Macedonia, Russia, Serbia, Ukraine, plus Armenia, Georgia, Turkey).
EUROPE <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark",
            "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy",
            "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal",
            "Romania", "Slovakia", "Slovenia", "Spain", "Sweden",                       # EU27
            "Iceland", "Norway", "Switzerland",                                         # EFTA
            "United Kingdom")
EUROPE_OTHER <- c("Albania", "Bosnia-Herzegovina", "Moldova", "Montenegro", "North Macedonia", "Russia",
                  "Serbia", "Ukraine", "Armenia", "Georgia", "Turkey")
SAMPLES <- c("full", "post2000", "post2010", "europe_only")
FES <- c(ctry_year = "countryname + year", ctry_year_p = "countryname + year + party", ctry_x_year = "countryname^year")
SES <- c(country = "countryname", ctry_year = "countryname^year", party = "party")

meta <- read_parquet(PIPE_META) |> filter(has_text) |> distinct(doc_id, .keep_all = TRUE) |>
  transmute(doc_id, countryname, party, year = as.integer(format(as.Date(date), "%Y")))
stopifnot("EUROPE names must exist in MPDS2026a" = all(c(EUROPE, EUROPE_OTHER) %in% meta$countryname))
write_csv_det(meta |> count(countryname, name = "manifestos") |>
                mutate(europe_sample = countryname %in% EUROPE, european_but_excluded = countryname %in% EUROPE_OTHER) |>
                filter(europe_sample | european_but_excluded) |> arrange(desc(europe_sample), countryname),
              file.path(PIPE_OUT, "csv", "42_europe_definition.csv"))

# ---- 1. Manifesto-level datasets, one per measurement path -------------------------
VALIDATIONS <- unique(c(RULE, "none", "r12", "full"))
data_cfg <- expand_grid(validation = VALIDATIONS, gemini_norm = c("vertex", "june"), gap_legs = c("na", "june_fallback"),
                        parse_repaired = "keep")
# parse-repair fork (chain-review #6): the 450 Claude dims decoded by the lenient parser of R/15b
# excluded, evaluated at the baseline measurement path only (one extra data configuration)
data_cfg <- bind_rows(data_cfg, tibble(validation = RULE, gemini_norm = "vertex", gap_legs = "na", parse_repaired = "exclude"))
paths <- list()
for (k in seq_len(nrow(data_cfg))) {
  cfg <- data_cfg[k, ]
  cl <- load_chunk_dims(PIPE_IN, PIPE_DERIVED, gemini_norm = cfg$gemini_norm, gap_legs = cfg$gap_legs)
  wl <- aggregate_within(cl, validation = cfg$validation, exclude_repaired = cfg$parse_repaired == "exclude")
  for (ms in names(MODEL_SETS)) for (w in WITHIN) {
    betw <- if (ms == "all_three") c("mean", "median", "leave_worst_out") else "mean"
    for (b in betw) {
      key <- paste(cfg$validation, cfg$gemini_norm, cfg$gap_legs, cfg$parse_repaired, ms, w, b, sep = "|")
      paths[[key]] <- list(validation = cfg$validation, gemini_norm = cfg$gemini_norm, gap_legs = cfg$gap_legs,
                           parse_repaired = cfg$parse_repaired,
                           model_set = ms, within_agg = w, between_agg = b,
                           data = combine_models(wl, models = MODEL_SETS[[ms]], stat = w, between = b,
                                                 dims = c("populism_overall", OUTCOMES, "pop_ideology_left",
                                                          "pop_ideology_right", "pop_ideology_centrist")) |>
                             select(doc_id, populism_overall, all_of(OUTCOMES), IDEO) |>
                             inner_join(meta, by = "doc_id") |> arrange(doc_id))
    }
  }
  message(sprintf("data config %d/%d done (%d paths)", k, nrow(data_cfg), length(paths)))
}
# identical datasets share a data_key (e.g. gemini_norm is irrelevant for sonnet_only)
data_hash <- map_chr(paths, ~ rlang::hash(.x$data))
data_key  <- match(data_hash, unique(data_hash))
message(sprintf("measurement paths: %d, distinct datasets: %d", length(paths), max(data_key)))
# 2026-11-11 (PLLM-0206): the distinct datasets, stacked, as input of R/56 (joint inference)
write_atomic(map_dfr(which(!duplicated(data_key)), function(i)
               paths[[i]]$data |> mutate(data_key = data_key[i], .before = 1) |>
                 select(data_key, doc_id, countryname, party, year, populism_overall, all_of(OUTCOMES), IDEO)),
             file.path(SC, "datasets_v2026a.parquet"), compression = "zstd", compression_level = 9)

# ---- 2. Regressions: run each distinct dataset once ---------------------------------
run_dataset <- function(dat) {
  out <- list()
  for (oc in OUTCOMES) for (smp in SAMPLES) for (fe in names(FES)) {
    s <- switch(smp, full = dat, post2000 = dat[dat$year >= 2000, ], post2010 = dat[dat$year >= 2010, ],
                europe_only = dat[dat$countryname %in% EUROPE, ])
    for (est in c("pop", "inter")) {
      rhs <- if (est == "pop") "populism_overall" else "populism_overall * IDEO"
      term <- if (est == "pop") "populism_overall" else "populism_overall:IDEO"
      need <- if (est == "pop") c(oc, "populism_overall") else c(oc, "populism_overall", "IDEO")
      ss <- s[stats::complete.cases(s[need]), ]
      base <- tibble(outcome = oc, sample_key = smp, fe_key = fe, estimand = est)
      if (nrow(ss) < 100) { out[[length(out) + 1]] <- base |> mutate(status = "failed", reason = "fewer than 100 obs"); next }
      fit <- tryCatch(feols(as.formula(sprintf("%s ~ %s | %s", oc, rhs, FES[[fe]])), data = ss, nthreads = 1,
                            warn = FALSE, notes = FALSE),
                      error = function(e) e)
      if (inherits(fit, "error") || !term %in% names(coef(fit)) || nobs(fit) < 100) {
        out[[length(out) + 1]] <- base |> mutate(status = "failed",
          reason = if (inherits(fit, "error")) conditionMessage(fit) else if (!term %in% names(coef(fit)))
            "term not identified" else "fewer than 100 obs after fixest drops singleton FE groups")
        next
      }
      ses <- map_dfr(names(SES), function(se) {
        ct <- coeftable(summary(fit, vcov = as.formula(paste0("~", SES[[se]]))))
        tibble(se_key = se, se = ct[term, 2], t = ct[term, 3], p = ct[term, 4])
      })
      out[[length(out) + 1]] <- base |>
        mutate(status = "ok", reason = NA_character_, coef = coef(fit)[[term]], n = nobs(fit),
               r2_w = fitstat(fit, "wr2", simplify = TRUE)[[1]], se_tab = list(ses))
    }
  }
  bind_rows(out)
}
uniq <- which(!duplicated(data_key))
plan(multisession, workers = 8L)
reg <- future_map(uniq, function(i) run_dataset(paths[[i]]$data) |> mutate(data_key = data_key[i]),
                  .options = furrr_options(seed = NULL,
                                           globals = c("paths", "data_key", "run_dataset", "OUTCOMES", "SAMPLES",
                                                       "FES", "SES", "EUROPE"),
                                           packages = c("dplyr", "tibble", "purrr", "fixest", "stats")))
plan(sequential)
reg <- bind_rows(reg)

# ---- 3. The grid: every path x analytic cell x SE cluster -----------------------------
path_tab <- imap_dfr(paths, ~ tibble(path_id = .y, validation = .x$validation, gemini_norm = .x$gemini_norm,
                                     gap_legs = .x$gap_legs, parse_repaired = .x$parse_repaired, model_set = .x$model_set, within_agg = .x$within_agg,
                                     between_agg = .x$between_agg)) |>
  mutate(data_key = data_key)
grid <- path_tab |>
  inner_join(reg, by = "data_key", relationship = "many-to-many") |>
  mutate(se_tab = map2(se_tab, status, ~ if (.y == "ok") .x else tibble(se_key = names(SES), se = NA_real_, t = NA_real_, p = NA_real_))) |>
  unnest(se_tab) |>
  mutate(beta_key = paste(data_key, outcome, sample_key, fe_key, estimand, sep = "|"),
         is_baseline = validation == RULE & gemini_norm == "vertex" & gap_legs == "na" & parse_repaired == "keep" &
           model_set == "all_three" & within_agg == "cwm" & between_agg == "mean" &
           outcome == "liberalism_overall" & sample_key == "full" & fe_key == "ctry_year" & se_key == "party") |>
  arrange(estimand, validation, gemini_norm, gap_legs, parse_repaired, model_set, within_agg, between_agg,
          outcome, sample_key, fe_key, se_key)
write_atomic(grid, file.path(SC, "specs_grid_v2026a.parquet"), compression = "zstd", compression_level = 9)

# ---- 4. Accounting ------------------------------------------------------------------------
ok <- grid |> filter(status == "ok")
acct <- grid |> group_by(estimand) |>
  summarise(cells = n(), cells_ok = sum(status == "ok"), cells_failed = sum(status != "ok"),
            regressions_run = n_distinct(beta_key),
            distinct_specs = n_distinct(paste(outcome, sample_key, fe_key, formatC(coef, digits = 10, format = "e"), n)[status == "ok"]),
            .groups = "drop")
write_csv_det(acct, file.path(PIPE_OUT, "csv", "42_grid_accounting.csv"))
fails <- grid |> filter(status != "ok") |> distinct(beta_key, estimand, model_set, outcome, sample_key, fe_key, reason) |>
  count(estimand, reason, name = "regressions")
write_csv_det(fails, file.path(PIPE_OUT, "csv", "42_failed_cells.csv"))

# Distinct specifications: cells that give the identical regression (same estimand, outcome,
# sample, FE, beta and N) count once; party-clustered SE. Among duplicates the cell closest to
# the baseline measurement path is kept, so e.g. gemini_norm = "june" survives only for model
# sets that contain Gemini.
distinct_specs <- ok |> filter(se_key == "party") |>
  mutate(reg_id = paste(estimand, outcome, sample_key, fe_key, formatC(coef, digits = 10, format = "e"), n, sep = "|")) |>
  arrange(reg_id, validation != RULE, gemini_norm != "vertex", gap_legs != "na", parse_repaired != "keep", within_agg != "cwm",
          between_agg != "mean", model_set) |>
  distinct(reg_id, .keep_all = TRUE) |>
  arrange(estimand, validation, gemini_norm, gap_legs, parse_repaired, model_set, within_agg, between_agg, outcome, sample_key, fe_key)
write_atomic(distinct_specs, file.path(SC, "specs_distinct_v2026a.parquet"), compression = "zstd", compression_level = 9)

# ---- 5. Tables and figures (Appendix C) --------------------------------------------------
summ <- function(x) x |> summarise(n = n(), median = median(coef), mean = mean(coef),
                                   p2.5 = quantile(coef, .025, names = FALSE), p5 = quantile(coef, .05, names = FALSE),
                                   p25 = quantile(coef, .25, names = FALSE), p75 = quantile(coef, .75, names = FALSE),
                                   p95 = quantile(coef, .95, names = FALSE), p97.5 = quantile(coef, .975, names = FALSE),
                                   share_sig_neg = mean(p < .05 & coef < 0, na.rm = TRUE),
                                   share_sig_pos = mean(p < .05 & coef > 0, na.rm = TRUE),
                                   share_pos = mean(coef > 0), n_p_missing = sum(is.na(p)), .groups = "drop")
headline <- bind_rows(
  distinct_specs |> group_by(estimand) |> summ() |> mutate(set = "distinct specs, party SE"),
  ok |> group_by(estimand) |> summ() |> mutate(set = "all cells (incl. SE fork)")) |>
  relocate(set, estimand)
write_csv_det(headline, file.path(PIPE_OUT, "csv", "42_headline_distribution.csv"))

FORKS <- c("model_set", "within_agg", "validation", "gemini_norm", "gap_legs", "parse_repaired", "between_agg",
           "outcome", "sample_key", "fe_key")
per_fork <- map_dfr(FORKS, function(f) distinct_specs |> group_by(estimand, option = .data[[f]]) |> summ() |>
                      mutate(fork = f, .before = 1))
write_csv_det(per_fork, file.path(PIPE_OUT, "csv", "42_per_fork.csv"))

fork_lbl <- c(model_set = "Model set", within_agg = "Within-model aggregation", validation = "Span validation",
              gemini_norm = "Gemini source (131 manifestos)", gap_legs = "Missing model scores",
              parse_repaired = "Leniently decoded dimensions", between_agg = "Between-model aggregation",
              outcome = "Outcome", sample_key = "Sample", fe_key = "Fixed effects")
# display labels of the fork options (tables only; csv files keep the option codes)
# (PLLM-0238, 2026-11-11) labels are fork-aware: the option code "full" means the full sample in the
# sample fork and the full span validation in the validation fork; model sets, outcomes and fixed
# effects are spelled out as in the OA8 table of R/45.
opt_lbl <- c(vertex = "Vertex AI (pinned version)", june = "AI Studio alias", na = "left missing",
             june_fallback = "filled from the initial scoring pass", none = "none", quote = "quote check",
             r12 = "quote + topic (L/R/C, overall)", full = "quote + topic (all dims)",
             cwm = "conf.-weighted (own block)", cwm_pop = "conf.-weighted (populism conf.)",
             mean = "mean", median = "median", leave_worst_out = "leave worst out", exclude = "exclude", keep = "keep",
             all_three = "all three", sonnet_only = "Sonnet only", gpt41_only = "GPT-4.1-mini only", gemini_only = "Gemini only",
             sonnet_gpt41 = "Sonnet + GPT-4.1-mini", sonnet_gemini = "Sonnet + Gemini", gpt41_gemini = "GPT-4.1-mini + Gemini",
             post2000 = "elections from 2000", post2010 = "elections from 2010", europe_only = "European countries",
             ctry_year = "country + year", ctry_year_p = "country + year + party", ctry_x_year = "country $\\times$ year",
             liberalism_overall = "Liberalism (overall)", lib_political = "Political", lib_social = "Social",
             lib_economic = "Economic", lib_financial_market = "Financial markets")
opt_show <- function(x, fork = NA) ifelse(!is.na(fork) & fork == "sample_key" & x == "full", "all manifestos",
                                          ifelse(x %in% names(opt_lbl), opt_lbl[x], str_replace_all(x, "_", "\\\\_")))
f3 <- function(x) sprintf("$%.3f$", x)
hp <- headline |> filter(set == "distinct specs, party SE", estimand == "pop")
write_atomic(c(sprintf("%% v2026a; distinct specifications (party-clustered SE); span validation rule '%s'; generated by R/42_Spec_Curve.R", RULE),
  "\\begin{table}[ht]", "\\centering",
  sprintf("\\caption{Distribution of the populism slope across %s distinct specifications}", comma(hp$n)),
  "\\label{tab:speccurve:headline}", "\\footnotesize", "\\begin{tabular}{lr}", "\\toprule",
  "Statistic & Value \\\\", "\\midrule",
  sprintf("Median $\\beta$ & %s \\\\", f3(hp$median)), sprintf("Mean $\\beta$ & %s \\\\", f3(hp$mean)),
  sprintf("Interquartile range & $[%.3f,\\,%.3f]$ \\\\", hp$p25, hp$p75),
  sprintf("2.5th / 97.5th percentile & $[%.3f,\\,%.3f]$ \\\\", hp$p2.5, hp$p97.5),
  sprintf("Share significantly negative at $p<0.05$ & %.1f\\%% \\\\", 100 * hp$share_sig_neg),
  sprintf("Share significantly positive at $p<0.05$ & %.1f\\%% \\\\", 100 * hp$share_sig_pos),
  sprintf("Share with $\\hat{\\beta}>0$ (any significance) & %.1f\\%% \\\\", 100 * hp$share_pos),
  "\\bottomrule", "\\end{tabular}", "\\end{table}"), file.path(PIPE_OUT, "tables", "tab_speccurve_headline.tex"),
  writer = function(x, p) writeLines(x, p))
pf <- per_fork |> filter(estimand == "pop")
pf_rows <- unlist(map(FORKS, function(f) {
  x <- pf |> filter(fork == f) |> arrange(median)
  c(sprintf("%s & %s & %s & %s & $[%.3f,\\,%.3f]$ & %.1f & %.1f \\\\",
            c(paste0("\\textit{", fork_lbl[[f]], "}"), rep("", nrow(x) - 1)),
            opt_show(x$option, f), comma(x$n), f3(x$median), x$p5, x$p95,
            100 * x$share_sig_neg, 100 * x$share_pos), "\\midrule")
}))
write_atomic(c(sprintf("%% v2026a; distinct specifications (party-clustered SE); generated by R/42_Spec_Curve.R"),
  "\\begin{table}[ht]", "\\centering", "\\caption{Per-fork decomposition of the populism slope}",
  "\\label{tab:speccurve:perfork}", "\\footnotesize", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%", "\\begin{tabular}{llrrcrr}", "\\toprule",
  "Fork & Option & $n$ & Median $\\beta$ & 5/95\\% band & \\% sig.\\ neg. & \\% $\\hat\\beta>0$ \\\\", "\\midrule",
  head(pf_rows, -1), "\\bottomrule", "\\end{tabular}}", "\\end{table}"),
  file.path(PIPE_OUT, "tables", "tab_speccurve_perfork.tex"), writer = function(x, p) writeLines(x, p))

for (est in c("pop", "inter")) {
  sorted <- distinct_specs |> filter(estimand == est) |> arrange(coef, beta_key) |> mutate(rank = row_number(),
            sig = if_else(p < .05, "p < 0.05", "n.s."))
  lab <- if (est == "pop") "Populism coefficient" else "Populism x ideology coefficient"
  p_sc <- ggplot(sorted, aes(rank, coef)) +
    geom_linerange(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se, colour = sig), alpha = 0.25, linewidth = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    geom_hline(yintercept = median(sorted$coef), colour = "darkblue", linewidth = 0.4) +
    geom_point(size = 0.25) +
    scale_colour_manual(values = c("p < 0.05" = "firebrick", "n.s." = "grey60"), name = NULL) +
    scale_y_continuous(name = lab) +
    scale_x_continuous(name = "Distinct specification (sorted by coefficient)", labels = comma) +
    theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  save_pdf(p_sc, file.path(PIPE_OUT, "figures", sprintf("spec_curve%s.pdf", if (est == "pop") "" else "_interaction")), 7.5, 4.2)
  p_h <- ggplot(sorted, aes(coef)) +
    geom_histogram(binwidth = if (est == "pop") 0.025 else 0.05, fill = "steelblue", alpha = 0.7, colour = "white") +
    geom_vline(xintercept = median(sorted$coef), colour = "darkblue", linewidth = 0.6) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    scale_x_continuous(name = lab) + scale_y_continuous(name = "Number of distinct specifications", labels = comma) +
    theme_minimal(base_size = 11)
  save_pdf(p_h, file.path(PIPE_OUT, "figures", sprintf("spec_curve_hist%s.pdf", if (est == "pop") "" else "_interaction")), 7.5, 3.5)
}

log_lines <- c(sprintf("R/42 spec curve | baseline validation rule = %s; forks: %s", RULE, paste(VALIDATIONS, collapse = ", ")),
               sprintf("measurement paths %d, distinct datasets %d", length(paths), max(data_key)),
               capture.output(print(acct, width = 200)), capture.output(print(fails, width = 200)),
               capture.output(print(headline, width = 200)),
               "baseline cell:", capture.output(print(grid |> filter(is_baseline) |>
                                                         select(estimand, coef, se, p, n), width = 200)))
writeLines(log_lines, file.path(PIPE_OUT, "logs", "42_spec_curve.txt"))
cat(log_lines, sep = "\n")
