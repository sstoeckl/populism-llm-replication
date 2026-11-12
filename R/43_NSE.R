# ============================================================
# 43_NSE.R
#
# Non-standard-error inputs for the Phase 5 reporting decision (PLAN.md). Computes both
# candidate rules; decides nothing.
#   (i)  central specification with an NSE interval: beta +/- 1.96 NSE, and the 2.5-97.5%
#        percentile band of the specification distribution;
#   (ii) combined uncertainty SE_total = sqrt(SE^2 + NSE^2), with t and p.
# NSE = SD of beta across DISTINCT specifications (R/42 specs_distinct), in three pools. The
# primary pools are `measure` and `outcome` (one estimand); `all` pools different outcomes and is
# reported for comparability with the paper's Appendix C only:
#   all       every distinct specification, outcomes pooled (the paper's Appendix C grid)
#   outcome   every distinct specification with the outcome fixed at the estimand's outcome
#             (liberalism_overall), i.e. all measurement and analytic forks
#   measure   measurement forks only (model set, within/between aggregation, span validation,
#             Gemini source, gap legs); outcome, sample, FE fixed at the baseline
# Both estimands: pop (beta on POP) and inter (beta on POP x IDEO, model 5).
#
# PLLM-0253 (2026-11-11): the interaction estimand also for the four liberalism dimensions (political,
# social, economic, financial market), pools `measure` and `outcome` with the outcome fixed at that
# dimension; baseline = the same specification as the headline (baseline data path, full sample,
# country + year FE, party-clustered SE) with that outcome, i.e. column (5) of Tables 5-8. Written to
# 43_nse_rules_by_outcome.csv (43_nse_rules.csv keeps its rows, which R/46 reads) and as a second
# panel of tab_nse.tex.
#
# Outputs: PIPE_OUT/csv/43_nse_distribution.csv, 43_nse_rules.csv, 43_nse_rules_by_outcome.csv,
#          tables/tab_nse.tex
# ============================================================

suppressPackageStartupMessages({ library(tidyverse); library(arrow) })
source("R/0_Config.R")

SC <- file.path(PIPE_OUT, "spec_curve")
ds   <- read_parquet(file.path(SC, "specs_distinct_v2026a.parquet"))
grid <- read_parquet(file.path(SC, "specs_grid_v2026a.parquet"))
base <- grid |> filter(is_baseline, status == "ok") |> select(estimand, coef, se, t, p, n)
stopifnot(nrow(base) == 2L)

BASE_ANALYTIC <- list(outcome = "liberalism_overall", sample_key = "full", fe_key = "ctry_year")
pools <- list(
  all     = function(x) x,
  outcome = function(x) x |> filter(outcome == BASE_ANALYTIC$outcome),
  measure = function(x) x |> filter(outcome == BASE_ANALYTIC$outcome, sample_key == BASE_ANALYTIC$sample_key,
                                    fe_key == BASE_ANALYTIC$fe_key))

dist <- imap_dfr(pools, function(f, pool) {
  f(ds) |> group_by(estimand) |>
    summarise(n_specs = n(), median = median(coef), mean = mean(coef), nse_sd = sd(coef),
              iqr_lo = quantile(coef, .25, names = FALSE), iqr_hi = quantile(coef, .75, names = FALSE),
              p2.5 = quantile(coef, .025, names = FALSE), p97.5 = quantile(coef, .975, names = FALSE),
              min = min(coef), max = max(coef),
              share_sig5 = mean(p < .05, na.rm = TRUE), share_sig5_neg = mean(p < .05 & coef < 0, na.rm = TRUE),
              share_sig5_pos = mean(p < .05 & coef > 0, na.rm = TRUE), n_p_missing = sum(is.na(p)),
              share_sign_flip_vs_median = mean(sign(coef) != sign(median(coef))),
              median_se = median(se), .groups = "drop") |>
    mutate(pool = pool, .before = 1)
})
write_csv_det(dist, file.path(PIPE_OUT, "csv", "43_nse_distribution.csv"))

# Central spec of each pool: the distinct specification whose beta is closest to the pool median
central <- imap_dfr(pools, function(f, pool) {
  f(ds) |> group_by(estimand) |> mutate(dev = abs(coef - median(coef))) |>
    arrange(dev, beta_key) |> slice(1) |> ungroup() |>
    transmute(pool, estimand, central_coef = coef, central_se = se, central_p = p, central_n = n,
              central_spec = paste(model_set, within_agg, validation, gemini_norm, gap_legs, parse_repaired, between_agg,
                                   outcome, sample_key, fe_key, sep = " / "))
})

rules <- dist |> select(pool, estimand, n_specs, nse_sd, median, p2.5, p97.5) |>
  left_join(central, by = c("pool", "estimand")) |>
  left_join(base |> rename(base_coef = coef, base_se = se, base_t = t, base_p = p, base_n = n), by = "estimand") |>
  mutate(
    # (i) central / baseline spec with NSE interval
    rule_i_central_lo = central_coef - 1.96 * nse_sd, rule_i_central_hi = central_coef + 1.96 * nse_sd,
    rule_i_base_lo = base_coef - 1.96 * nse_sd, rule_i_base_hi = base_coef + 1.96 * nse_sd,
    # (ii) combined uncertainty on the baseline spec
    se_total = sqrt(base_se^2 + nse_sd^2), t_total = base_coef / se_total,
    p_total = 2 * pnorm(-abs(t_total)), nse_over_se = nse_sd / base_se)
write_csv_det(rules, file.path(PIPE_OUT, "csv", "43_nse_rules.csv"))

# ---- Interaction estimand by liberalism dimension (PLLM-0253) ----
DIM_OUT <- c(lib_political = "political", lib_social = "social", lib_economic = "economic",
             lib_financial_market = "financial market")
base_key <- grid |> filter(is_baseline, status == "ok") |> distinct(data_key, sample_key, fe_key, se_key)
stopifnot(nrow(base_key) == 1L)
rules_dim <- map_dfr(names(DIM_OUT), function(o) {
  b <- grid |> filter(data_key == base_key$data_key, outcome == o, sample_key == base_key$sample_key,
                      fe_key == base_key$fe_key, se_key == base_key$se_key, estimand == "inter", status == "ok")
  stopifnot(nrow(b) == 1L)
  pools_o <- list(outcome = function(x) x |> filter(outcome == o),
                  measure = function(x) x |> filter(outcome == o, sample_key == base_key$sample_key, fe_key == base_key$fe_key))
  imap_dfr(pools_o, function(f, pool) f(ds) |> filter(estimand == "inter") |>
    summarise(n_specs = n(), nse_sd = sd(coef), median = median(coef),
              p2.5 = quantile(coef, .025, names = FALSE), p97.5 = quantile(coef, .975, names = FALSE),
              share_sig5_neg = mean(p < .05 & coef < 0, na.rm = TRUE), share_sig5_pos = mean(p < .05 & coef > 0, na.rm = TRUE)) |>
    mutate(pool = pool, .before = 1)) |>
    mutate(outcome = o, estimand = "inter", base_coef = b$coef, base_se = b$se, base_t = b$t, base_p = b$p, base_n = b$n,
           .before = 1)
}) |>
  mutate(se_total = sqrt(base_se^2 + nse_sd^2), t_total = base_coef / se_total,
         p_total = 2 * pnorm(-abs(t_total)), nse_over_se = nse_sd / base_se)
write_csv_det(rules_dim, file.path(PIPE_OUT, "csv", "43_nse_rules_by_outcome.csv"))

f3 <- function(x) sub("^-", "$-$", formatC(x, format = "f", digits = 3))   # true minus sign (PLLM-0240)
fp <- function(p) ifelse(p < 0.001, "$<$0.001", formatC(p, format = "f", digits = 3))
# primary pools first (chain-review #11): measurement-only, then outcome-fixed, then all pooled
tex_rows <- rules |> arrange(estimand, match(pool, c("measure", "outcome", "all"))) |>
  mutate(est = if_else(estimand == "pop", "POP", "POP$\\times$IDEO"),
         row = sprintf("%s & %s & %d & %s & %s & [%s, %s] & %s & %s & %s & %s \\\\", est, pool, n_specs, f3(base_coef),
                       f3(base_se), f3(p2.5), f3(p97.5), f3(nse_sd), f3(se_total), f3(t_total), fp(p_total)))
tex_rows_dim <- rules_dim |> arrange(match(outcome, names(DIM_OUT)), match(pool, c("measure", "outcome"))) |>
  mutate(row = sprintf("POP$\\times$IDEO, %s & %s & %d & %s & %s & [%s, %s] & %s & %s & %s & %s \\\\", DIM_OUT[outcome], pool, n_specs,
                       f3(base_coef), f3(base_se), f3(p2.5), f3(p97.5), f3(nse_sd), f3(se_total), f3(t_total), fp(p_total)))
write_atomic(c("% v2026a; NSE = SD of beta across distinct specifications; baseline spec: all three models, own-block cwm, quote-check span validation, Vertex Gemini, gap legs NA, mean, liberalism_overall, full sample, country + year FE, party-clustered SE; generated by R/43_NSE.R",
  "\\begin{table}[ht]", "\\centering", "\\caption{Sampling and non-standard errors of the headline estimates}",
  "\\label{tab:nse}", "\\footnotesize", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%", "\\begin{tabular}{llrrrcrrrr}", "\\toprule",
  "Estimand & NSE pool & Specs & $\\hat\\beta$ & SE & 2.5--97.5\\% of specs & NSE & $\\sqrt{SE^2+NSE^2}$ & $t$ & $p$ \\\\",
  "\\midrule", tex_rows$row,
  "\\midrule", "\\multicolumn{10}{l}{\\emph{Interaction by liberalism dimension (outcome fixed at the dimension)}} \\\\",
  tex_rows_dim$row, "\\bottomrule", "\\end{tabular}}", "\\end{table}"),
  file.path(PIPE_OUT, "tables", "tab_nse.tex"), writer = function(x, p) writeLines(x, p))

out <- c("R/43 NSE", capture.output(print(dist, width = 250)), capture.output(print(rules, width = 250)),
         capture.output(print(rules_dim, width = 250)))
writeLines(out, file.path(PIPE_OUT, "logs", "43_nse.txt"))
cat(out, sep = "\n")
