# ============================================================
# 41_Paper_Tables.R
#
# Every data-driven table and figure of paper/R0/main_v2.tex (main text and Appendix A),
# recomputed on the v2026a baseline. Appendix C (spec curve) is R/42, A.13 and the
# independence tests are R/51 / R/50. Started from R/_archive/22_Paper_Tables_v4.R.
#
#   label in main_v2.tex      output (PIPE_OUT/tables | figures | csv)
#   tab:sumstats              tab_sumstats.tex, 41_tab1_sumstats.csv
#   tab:cormatrix             tab_cormatrix.tex, 41_tab2_cormatrix.csv
#   tab:cormatrix2            tab_cormatrix2_partial.tex (LLM + RILE cells only; the external
#                             ML / POPPA / CHES / GPS indicators are not in the repo)
#   f:1-f:3, f:11-f:16        <Country>_<y>_vs_<x>.pdf (2010+), 41_fig_country_points.csv
#   f:4, f:5                  Populism_Over_Time*.pdf, Liberalism_Over_Time*.pdf, 41_fig4_5_*.csv
#   tab:4-tab:8               tab_reg_<n>_<dv>_clparty.tex (main), _clcountry.tex (robustness),
#                             41_tab4_8_regressions_long.csv
#   f:6-f:10                  Marginal_Effect_Populism_on_<dv>_by_Ideology(_clcountry).pdf,
#                             41_fig6_10_interaction_tests.csv
#   tab:stat1 (A.1)           tab_stat1.tex, 41_tabA1_stat1.csv
#   tab:evidence_shares (A.10) tab_evidence_shares.tex, 41_tabA10_evidence_shares.csv,
#                             41_A10_computational_summary.csv
#   l.170 / l.195 corpus counts 41_corpus_counts.csv
#   (new, appendix) robustness tab_baseline_variants.tex (label tab:robust_measurement),
#                             41_baseline_variants.csv: span-validation forks none / r12 / full,
#                             June rule, Gemini source, gap legs, single models, post-2010
#   (2026-10-27) June axes    figures/june_axes/<same file name>.pdf for f:1-f:3 (Populism vs
#                             Ideology), Germany Economic Liberalism vs Populism, f:4
#                             (Populism_Over_Time) and f:6-f:10 (party SE), on the axis limits of
#                             the June 2026 figures (widened where needed; section 12),
#                             41_fig_june_axes_limits.csv
#   (2026-10-27) combined     figures/paper_v2026a/Trends_by_Ideology.pdf, Marginal_Effects_Panels.pdf,
#                             Marginal_Effects_Dots.pdf (section 13; PLLM-0119..0122),
#                             41_fig_trends_by_ideology.csv, 41_fig_me_points.csv
#   (2026-11-11) figure notes figures/<figure>_note.tex next to each main-text figure (f:1-f:3,
#                             f:trends, f:me): Title Case caption (comment) + self-contained note
#                             to paste below the figure (PLLM-0256; section 14)
#   (2026-11-11, batch 2)     figures/paper_v2026a/Predicted_Liberalism_by_Ideology.pdf (+ _note.tex),
#                             41_fig_predicted_liberalism.csv: predicted liberalism over populism for
#                             a left (IDEO p10) and a right (p90) manifesto from column (5) (PLLM-0242;
#                             section 13b); Trends_by_Ideology.pdf with the populism panel on the left
#                             (PLLM-0242). tab:robust_measurement: three aggregation forks, no-evidence
#                             populism = 0, chunk-length weights, mixed chunks kept (PLLM-0247), with
#                             41_chunk_subset_share.csv. Tables 4-8 notes: column (4) on the sample of
#                             column (6) (PLLM-0248). 41_fig4_5_trend_slopes.csv: country-FE trend
#                             columns; 41_fin_missingness.csv + Table 8 note line (PLLM-0249).
#                             tab_pop_subdims.tex (label tab:pop_subdims), 41_pop_subdims.csv: each
#                             liberalism dimension on each populism sub-dimension (PLLM-0250).
#
# Review batch 2026-11-11: Tables 4-8 print SEs in parentheses (PLLM-0237); table labels, true minus
# signs, thousands separators, Table 1 header, Table D.1 sample note, tab:robust_measurement note and
# outcome labels (PLLM-0240 / PLLM-0258); captions of the main-text tables in Title Case (PLLM-0256).
#
# Specification (paper eq. 1): LIB = b1 POP + b2 IDEO + b3 POP x IDEO + country FE + year FE
# (+ party FE), columns (1)-(7) as in the paper; (7) = the interaction model (5) with party FE (2026-11-06). POP = populism_overall, IDEO =
# pop_ideology_direction = (R - L) / (R + L) (rev3, 2026-10-27). Party FE = CMP `party` code. Inference: SEs clustered by party
# (main), by country (robustness). Each column uses its own complete cases (as in the
# paper); N per column is in every table.
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(fixest); library(ggrepel); library(scales)
})
source("R/0_Config.R")
source("R/1_Aggregation.R")

TAB <- file.path(PIPE_OUT, "tables"); FIG <- file.path(PIPE_OUT, "figures"); CSV <- file.path(PIPE_OUT, "csv")
f2 <- function(x, d = 2) formatC(x, format = "f", digits = d)
# true minus sign for printed numbers (PLLM-0240): "-0.459" -> "$-$0.459" (as the Appendix B tables)
mf <- function(x, d = 3) sub("^-", "$-$", f2(x, d))
stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
write_tex <- function(lines, file) write_atomic(lines, file.path(TAB, file), writer = function(x, p) writeLines(x, p))
setFixest_nthreads(1)   # deterministic summation order

# ---- 1. Data ------------------------------------------------------------------------
d <- read_parquet(file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet")) |>
  mutate(IDEO = pop_ideology_direction)
stopifnot(nrow(d) == 3371L, !anyDuplicated(d$doc_id))
RULE <- unique(d$validation_rule)

LIB_DVS <- c(liberalism_overall   = "Liberalism (Overall)",
             lib_political        = "Liberalism (Political)",
             lib_social           = "Liberalism (Social)",
             lib_economic         = "Liberalism (Economic)",
             lib_financial_market = "Liberalism (Financial Markets)")
TAB_NO  <- c(liberalism_overall = 4, lib_political = 5, lib_social = 6, lib_economic = 7, lib_financial_market = 8)
FILE_LBL <- c(liberalism_overall = "Liberalism", lib_political = "Political_Liberalism",
              lib_social = "Social_Liberalism", lib_economic = "Economic_Liberalism",
              lib_financial_market = "Financial_Market_Liberalism")
SRC_NOTE <- sprintf("v2026a baseline (MPDS2026a / corpus 2026-1; span validation '%s'); generated by R/41_Paper_Tables.R", RULE)

# Corpus counts quoted in the text (l.170, l.195): analysis sample = POP and overall LIB
an <- d |> filter(!is.na(populism_overall), !is.na(liberalism_overall))
corpus_counts <- tibble(
  sample = c("corpus (scoring pool)", "analysis (POP and LIB overall non-missing)", "with IDEO as well"),
  manifestos = c(nrow(d), nrow(an), sum(!is.na(an$IDEO))),
  parties    = c(n_distinct(d$party), n_distinct(an$party), n_distinct(an$party[!is.na(an$IDEO)])),
  countries  = c(n_distinct(d$countryname), n_distinct(an$countryname), n_distinct(an$countryname[!is.na(an$IDEO)])),
  elections  = c(n_distinct(paste(d$countryname, d$date)), n_distinct(paste(an$countryname, an$date)),
                 n_distinct(paste(an$countryname, an$date)[!is.na(an$IDEO)])),
  first_year = c(min(d$year), min(an$year), min(an$year[!is.na(an$IDEO)])),
  last_year  = c(max(d$year), max(an$year), max(an$year[!is.na(an$IDEO)])))
write_csv_det(corpus_counts, file.path(CSV, "41_corpus_counts.csv"))

# ---- 2. Table 1 (tab:sumstats) ----------------------------------------------------------
desc_vars <- c("Populism (overall)" = "populism_overall", "Liberalism (overall)" = "liberalism_overall",
               "Liberalism (political)" = "lib_political", "Liberalism (social)" = "lib_social",
               "Liberalism (economic)" = "lib_economic", "Liberalism (financial markets)" = "lib_financial_market",
               "Ideology" = "IDEO")
sumstats <- imap_dfr(desc_vars, function(v, lbl) {
  x <- d[[v]]
  tibble(variable = lbl, var = v, n = sum(!is.na(x)), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
         min = min(x, na.rm = TRUE), q25 = quantile(x, .25, na.rm = TRUE, names = FALSE),
         median = median(x, na.rm = TRUE), q75 = quantile(x, .75, na.rm = TRUE, names = FALSE),
         max = max(x, na.rm = TRUE))
})
write_csv_det(sumstats, file.path(CSV, "41_tab1_sumstats.csv"))
write_tex(c(paste("%", SRC_NOTE),
  "\\begin{table}[ht]", "\\caption{Summary Statistics of Populism, Liberalism, and Ideology Scores}",
  "\\label{tab:sumstats}", "\\centering", "\\footnotesize", "\\begin{tabular}{lrrrrrrrr}", "  \\toprule",
  "Variable & N & Mean & SD & Min & Q25 & Median & Q75 & Max \\\\", "  \\midrule",
  with(sumstats, sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\", variable, comma(n), mf(mean, 2), f2(sd),
                         mf(min, 2), mf(q25, 2), mf(median, 2), mf(q75, 2), mf(max, 2))),
  "   \\bottomrule", "\\end{tabular}", "\\end{table}"), "tab_sumstats.tex")

# ---- 3. Table 2 (tab:cormatrix) ---------------------------------------------------------
cm <- cor(d[, desc_vars], use = "pairwise.complete.obs")
cn <- crossprod(!is.na(as.matrix(d[, desc_vars])))
dimnames(cm) <- dimnames(cn) <- list(names(desc_vars), names(desc_vars))
cm_long <- expand_grid(i = seq_along(desc_vars), j = seq_along(desc_vars)) |> filter(j < i) |>
  mutate(row = names(desc_vars)[i], col = names(desc_vars)[j], r = cm[cbind(i, j)], n = cn[cbind(i, j)],
         p = map2_dbl(i, j, ~ cor.test(d[[desc_vars[.x]]], d[[desc_vars[.y]]])$p.value)) |>
  select(row, col, r, n, p)
write_csv_det(cm_long, file.path(CSV, "41_tab2_cormatrix.csv"))
cm_rows <- map_chr(seq_along(desc_vars), function(i) {
  cells <- map_chr(1:6, function(j) if (j < i) mf(cm[i, j], 2) else "")   # true minus sign (PLLM-0240)
  paste0(names(desc_vars)[i], " & ", paste(cells, collapse = " & "), " \\\\")
})
write_tex(c(paste("%", SRC_NOTE, "; pairwise-complete correlations"),
  "\\begin{table}[ht]", "\\caption{Correlation Matrix of Populism, Liberalism, and Ideology Scores}",
  "\\label{tab:cormatrix}", "\\footnotesize", "\\centering", "\\begin{tabular}{lrrrrrr}", "  \\toprule",
  " & Pop & Lib & Lib (pol.) & Lib (soc.) & Lib (econ.) & Lib (fin.)\\\\", "  \\midrule",
  cm_rows, "   \\bottomrule",
  sprintf("\\multicolumn{7}{p{0.9\\linewidth}}{\\scriptsize Pairwise N between %s and %s.} \\\\",
          comma(min(cm_long$n)), comma(max(cm_long$n))),
  "\\end{tabular}", "\\end{table}"), "tab_cormatrix.tex")

# ---- 4. Table 3 (tab:cormatrix2): LLM and RILE cells -------------------------------------
# Sample per the caption: each party's most recent election in 2010-2015.
ext <- d |> filter(year >= 2010, year <= 2015) |>
  arrange(party, desc(date), doc_id) |> group_by(party) |> slice(1) |> ungroup()
ext_cor <- tibble(
  pair = c("IDEOL (LLM) ~ POP (LLM)", "RILE (CMP) ~ POP (LLM)", "RILE (CMP) ~ IDEOL (LLM)"),
  x = c("IDEO", "rile", "rile"), y = c("populism_overall", "populism_overall", "IDEO")) |>
  mutate(r = map2_dbl(x, y, ~ cor(ext[[.x]], ext[[.y]], use = "complete.obs")),
         n = map2_int(x, y, ~ sum(complete.cases(ext[[.x]], ext[[.y]]))))
write_csv_det(ext_cor, file.path(CSV, "41_tab3_cormatrix2_partial.csv"))
write_tex(c(paste("%", SRC_NOTE),
  "% PARTIAL tab:cormatrix2: only the LLM and RILE (CMP) cells can be recomputed from data in the repo.",
  "% POP RHET/IDEA (ML), POPPA, CHES 2019 and GPS: see tab_external_validity.tex (R/44).",
  "\\begin{tabular}{lrr}", "\\toprule", " & r & N \\\\", "\\midrule",
  sprintf("%s & %s & %d \\\\", ext_cor$pair, f2(ext_cor$r), ext_cor$n),
  "\\bottomrule", "\\end{tabular}"), "tab_cormatrix2_partial.tex")

# ---- 5. Figures 1-3, 11-16: country scatterplots (2010+) ---------------------------------
spain_parties_regex <- "We can|Basque Nat|Socialist|People's Party|Voice|Catal|Citi"
plot_dat <- d |>
  mutate(partyname   = if_else(countryname == "Spain" & str_detect(partyname, regex("we can", ignore_case = TRUE)),
                               "We can", partyname),
         partyabbrev = if_else(countryname == "Spain" & str_detect(partyname, fixed("We can")),
                               "We can", partyabbrev)) |>
  group_by(countryname, partyname, partyabbrev, date) |>
  mutate(across(c(populism_overall, liberalism_overall, IDEO, lib_economic), ~ mean(.x, na.rm = TRUE))) |>
  ungroup() |>
  filter(date >= as.Date("2010-01-01")) |>
  mutate(across(c(populism_overall, liberalism_overall, IDEO, lib_economic), ~ ifelse(is.nan(.x), NA_real_, .x)),
         date = factor(date)) |>
  rename("Election Date" = date, "Ideology" = IDEO, "Populism Overall" = populism_overall,
         "Liberalism Overall" = liberalism_overall, "Economic Liberalism" = lib_economic, "Party Name" = partyname)
axis_limits <- function(var) if (var == "Ideology") c(-1, 1) else c(0, 10)
plot_country_scatter <- function(data, country, xvar, yvar, xlab, ylab, title) {
  df <- data |> filter(countryname == country, !is.na(.data[[xvar]]), !is.na(.data[[yvar]]))
  if (country == "Spain") df <- df |> filter(grepl(spain_parties_regex, `Party Name`))
  ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]], color = `Party Name`, size = `Election Date`)) +
    geom_point() +
    # ggrepel stops after max.time (default 0.5 s) of wall-clock time, so label positions depended on
    # machine load. Bound it by iterations only (2026-10-26): deterministic for a given seed.
    geom_label_repel(aes(label = partyabbrev), nudge_y = 0.3, size = 3, show.legend = FALSE,
                     max.time = 1e6, max.iter = 10000,
                     max.overlaps = 30, seed = 1) +
    theme_minimal() +
    coord_cartesian(xlim = axis_limits(xvar), ylim = axis_limits(yvar)) +
    labs(title = title, x = xlab, y = ylab, color = "Party Name")
}
ideo_lab <- "Ideology (-1 = left, +1 = right)"
plot_specs <- tribble(
  ~xvar,               ~yvar,                 ~xlab,                    ~ylab,                       ~title,                                                        ~suffix,
  "Ideology",          "Populism Overall",    ideo_lab,                 "Populism Overall Score",    "Populism vs. Ideology in CMP Manifestos",            "Populism_vs_Populist_Ideology",
  "Ideology",          "Liberalism Overall",  ideo_lab,                 "Liberalism Overall Score",  "Liberalism vs. Ideology in CMP Manifestos",          "Liberalism_vs_Populist_Ideology",
  "Ideology",          "Economic Liberalism", ideo_lab,                 "Economic Liberalism Score", "Economic Liberalism vs. Ideology in CMP Manifestos", "Economic_Liberalism_vs_Populist_Ideology",
  "Populism Overall",  "Liberalism Overall",  "Populism Overall Score", "Liberalism Overall Score",  "Liberalism vs. Populism in CMP Manifestos",                   "Liberalism_vs_Populism",
  "Populism Overall",  "Economic Liberalism", "Populism Overall Score", "Economic Liberalism Score", "Economic Liberalism vs. Populism in CMP Manifestos",          "Economic_Liberalism_vs_Populism")
for (ctry in c("Austria", "Germany", "Spain", "Poland")) {
  pwalk(plot_specs, function(xvar, yvar, xlab, ylab, title, suffix) {
    save_pdf(plot_country_scatter(plot_dat, ctry, xvar, yvar, xlab, ylab, title),
             file.path(FIG, sprintf("%s_%s.pdf", ctry, suffix)), width = 15, height = 8)
  })
}
write_csv_det(plot_dat |> filter(countryname %in% c("Austria", "Germany", "Spain", "Poland")) |>
                select(countryname, `Party Name`, partyabbrev, party, `Election Date`, `Populism Overall`,
                       `Populist Ideology` = `Ideology`, `Liberalism Overall`, `Economic Liberalism`) |>   # csv column name kept
                arrange(countryname, `Party Name`, `Election Date`),
              file.path(CSV, "41_fig_country_points.csv"))

# ---- 6. Figures 4-5: time trends --------------------------------------------------------
year_agg <- d |> group_by(year) |>
  summarise(across(c(populism_overall, liberalism_overall, IDEO), ~ mean(.x, na.rm = TRUE)), n = n(), .groups = "drop")
year_agg_ideo <- d |> filter(!is.na(IDEO)) |>
  mutate(Ideology = if_else(IDEO <= 0, "Left", "Right")) |>
  group_by(year, Ideology) |>
  summarise(across(c(populism_overall, liberalism_overall, IDEO), ~ mean(.x, na.rm = TRUE)), n = n(), .groups = "drop")
write_csv_det(year_agg, file.path(CSV, "41_fig4_year_means.csv"))
write_csv_det(year_agg_ideo, file.path(CSV, "41_fig5_year_means_by_ideology.csv"))
plot_trend <- function(data, yvar, title, ylab, color_var = NULL) {
  p <- ggplot(data |> filter(year >= 1960), aes(x = year, y = .data[[yvar]]))
  if (!is.null(color_var)) p <- p + aes(color = .data[[color_var]]) +
      scale_color_manual(name = "Ideology", values = c(Left = "blue", Right = "red"))
  p + geom_point() + geom_smooth(method = "lm", formula = y ~ x, alpha = 0.4) +
    theme_minimal() + labs(title = title, x = "Year", y = ylab)
}
save_pdf(plot_trend(year_agg, "populism_overall", "Average Populism Score in CMP Manifestos Over Time",
                    "Populism Overall Score"), file.path(FIG, "Populism_Over_Time.pdf"), 10, 6)
save_pdf(plot_trend(year_agg_ideo, "populism_overall",
                    "Average Populism Score in CMP Manifestos Over Time, split according to Ideology",
                    "Populism Overall Score", "Ideology"), file.path(FIG, "Populism_Over_Time_by_Ideology.pdf"), 10, 6)
save_pdf(plot_trend(year_agg, "liberalism_overall", "Average Liberalism Score in CMP Manifestos Over Time",
                    "Liberalism Overall Score"), file.path(FIG, "Liberalism_Over_Time.pdf"), 10, 6)
save_pdf(plot_trend(year_agg_ideo, "liberalism_overall",
                    "Average Liberalism Score in CMP Manifestos Over Time, split according to Ideology",
                    "Liberalism Overall Score", "Ideology"), file.path(FIG, "Liberalism_Over_Time_by_Ideology.pdf"), 10, 6)
# Slopes behind the drawn lines (year-mean regressions, 1960+)
tidy_trend <- function(df, y, grp) {
  s <- summary(lm(as.formula(paste(y, "~ year")), data = df |> filter(year >= 1960)))$coefficients
  tibble(series = y, group = grp, slope_per_decade = 10 * s["year", 1], t = s["year", 3], p = s["year", 4])
}
# Inference (chain-review #9): the year-mean OLS p-values ignore serial correlation and the
# number of manifestos per year. The manifesto-level regression with SEs clustered by election
# year is reported next to it (slope_manifesto, t_cl_year, p_cl_year) and is the one to cite.
trend_mf <- function(dd, y) {
  m <- feols(as.formula(paste(y, "~ year")), data = dd |> filter(year >= 1960), vcov = ~year)
  ct <- coeftable(m); G <- n_distinct(dd$year[dd$year >= 1960 & !is.na(dd[[y]])])
  tibble(slope_manifesto_per_decade = 10 * ct["year", 1], t_cl_year = ct["year", 3], p_cl_year = ct["year", 4],
         n_manifestos = nobs(m), n_years = G)
}
# PLLM-0249 (EMP-016, 2026-11-11): the same manifesto-level trend with country fixed effects, SE
# clustered by party (CMP code), so the trend is not driven by the changing country composition of
# the corpus. Columns *_cfe next to the year-mean and year-clustered slopes (same rows).
trend_cfe <- function(dd, y) {
  m <- feols(as.formula(paste(y, "~ year | countryname")), data = dd |> filter(year >= 1960), vcov = ~party)
  ct <- coeftable(m)
  tibble(slope_cfe_per_decade = 10 * ct["year", 1], se_cfe_per_decade = 10 * ct["year", 2],
         t_cfe = ct["year", 3], p_cfe = ct["year", 4], n_cfe = nobs(m),
         n_parties_cfe = n_distinct(dd$party[dd$year >= 1960][obs(m)]))
}
d_lr <- d |> filter(!is.na(IDEO)) |> mutate(Ideology = if_else(IDEO <= 0, "Left", "Right"))
trend_tab <- bind_rows(
  bind_cols(tidy_trend(year_agg, "populism_overall", "all"), trend_mf(d, "populism_overall"), trend_cfe(d, "populism_overall")),
  bind_cols(tidy_trend(year_agg |> filter(year >= 1980), "populism_overall", "all, 1980+"), trend_mf(d |> filter(year >= 1980), "populism_overall"),
            trend_cfe(d |> filter(year >= 1980), "populism_overall")),
  bind_cols(tidy_trend(year_agg, "liberalism_overall", "all"), trend_mf(d, "liberalism_overall"), trend_cfe(d, "liberalism_overall")),
  bind_cols(tidy_trend(year_agg_ideo |> filter(Ideology == "Left"), "liberalism_overall", "Left"), trend_mf(d_lr |> filter(Ideology == "Left"), "liberalism_overall"),
            trend_cfe(d_lr |> filter(Ideology == "Left"), "liberalism_overall")),
  bind_cols(tidy_trend(year_agg_ideo |> filter(Ideology == "Right"), "liberalism_overall", "Right"), trend_mf(d_lr |> filter(Ideology == "Right"), "liberalism_overall"),
            trend_cfe(d_lr |> filter(Ideology == "Right"), "liberalism_overall")))
write_csv_det(trend_tab, file.path(CSV, "41_fig4_5_trend_slopes.csv"))

# ---- 7. Tables 4-8: panel regressions -----------------------------------------------------
VCOVS <- list(clparty = ~party, clcountry = ~countryname)
# PLLM-0237: standard errors (not t-statistics) in parentheses
VC_NOTE <- c(clparty = "Standard errors in parentheses, clustered by party (CMP party code)",
             clcountry = "Standard errors in parentheses, clustered by country")
fit_six <- function(dv, data = d) {
  f <- function(rhs, fe) as.formula(paste(dv, "~", rhs, "|", fe))
  m6 <- feols(f("populism_overall + IDEO", "countryname + year + party"), data = data)
  in6 <- seq_len(nrow(data)) %in% obs(m6)
  list(m1 = feols(f("populism_overall", "countryname + year"), data = data),
       m2 = feols(f("IDEO", "countryname + year"), data = data),
       m3 = feols(f("populism_overall + IDEO", "countryname"), data = data),
       m4 = feols(f("populism_overall + IDEO", "countryname + year"), data = data),
       m5 = feols(f("populism_overall * IDEO", "countryname + year"), data = data),
       m6 = m6,
       # column (7): the interaction model of column (5) with party FE (PLLM-0201, 2026-11-06)
       m7 = feols(f("populism_overall * IDEO", "countryname + year + party"), data = data),
       # column (1) on the common sample of column (4) (chain-review #10). Fitted on `data` with
       # `subset` (2026-11-11), so that obs() indexes rows of `data` and n_clusters in the long csv is
       # right; before, obs() indexed the subsetted data frame. Estimates unchanged.
       m1c = feols(f("populism_overall", "countryname + year"), data = data,
                   subset = stats::complete.cases(data[, c(dv, "populism_overall", "IDEO")])),
       # column (4) on the sample of column (6) (PLLM-0248, 2026-11-11): separates the sample change
       # (fixest drops observations fully explained by the party FE, e.g. one-manifesto parties)
       # from the change of identifying variation between columns (4) and (6)
       m4c = feols(f("populism_overall + IDEO", "countryname + year"), data = data, subset = in6))
}
COEFS <- c(populism_overall = "Populism (overall)", IDEO = "Ideology", "populism_overall:IDEO" = "Populism $\\times$ Ideology")   # PLLM-0240
models <- map(names(LIB_DVS), fit_six) |> set_names(names(LIB_DVS))

reg_long <- imap_dfr(models, function(ms, dv) {
  imap_dfr(VCOVS, function(vc, vcn) {
    imap_dfr(ms, function(m, mn) {
      ct <- coeftable(summary(m, vcov = vc))
      tibble(dv = dv, vcov = vcn, model = mn, term = rownames(ct),
             estimate = ct[, 1], se = ct[, 2], t = ct[, 3], p = ct[, 4],
             nobs = nobs(m), r2 = fitstat(m, "r2", simplify = TRUE)[[1]],
             ar2 = fitstat(m, "ar2", simplify = TRUE)[[1]], wr2 = fitstat(m, "wr2", simplify = TRUE)[[1]],
             rmse = fitstat(m, "rmse", simplify = TRUE)[[1]],
             n_clusters = n_distinct(d[[all.vars(vc)]][obs(m)]))
    })
  })
})
write_csv_det(reg_long, file.path(CSV, "41_tab4_8_regressions_long.csv"))

# PLLM-0249 (EMP-017, 2026-11-11): is the financial-market score missing at random with respect to
# populism? Linear probability model of missingness on populism with country and year FE (SE by
# party), on all manifestos with a populism score; plus the mean populism with and without the score.
# One line of the Table 8 note.
fin_d <- d |> filter(!is.na(populism_overall)) |> mutate(fin_missing = as.numeric(is.na(lib_financial_market)))
fin_lpm <- feols(fin_missing ~ populism_overall | countryname + year, data = fin_d, vcov = ~party)
fin_ct <- coeftable(fin_lpm)
fin_miss <- tibble(n = nrow(fin_d), n_missing = sum(fin_d$fin_missing), share_missing = mean(fin_d$fin_missing),
                   mean_pop_with_fin = mean(fin_d$populism_overall[fin_d$fin_missing == 0]),
                   mean_pop_without_fin = mean(fin_d$populism_overall[fin_d$fin_missing == 1]),
                   b_lpm = fin_ct["populism_overall", 1], se_lpm = fin_ct["populism_overall", 2],
                   t_lpm = fin_ct["populism_overall", 3], p_lpm = fin_ct["populism_overall", 4],
                   n_lpm = nobs(fin_lpm), n_clusters_lpm = n_distinct(fin_d$party[obs(fin_lpm)]))
write_csv_det(fin_miss, file.path(CSV, "41_fin_missingness.csv"))
fin_note <- with(fin_miss, sprintf(" The financial-market score is missing for %s percent of the %s manifestos with a populism score (mean populism %s with and %s without the score); in a linear probability model of missingness on populism with country and year fixed effects the coefficient is %s%s (SE %s).",
                                   f2(100 * share_missing, 1), comma(n), f2(mean_pop_with_fin), f2(mean_pop_without_fin),
                                   mf(b_lpm, 4), stars(p_lpm), f2(se_lpm, 4)))

reg_table_tex <- function(dv, vcn) {
  r <- reg_long |> filter(dv == !!dv, vcov == vcn)
  cell <- function(mn, term, what) {
    x <- r |> filter(model == mn, term == !!term)
    if (nrow(x) == 0) return("")
    # PLLM-0237: SE (three decimals) in parentheses; PLLM-0240: true minus sign
    if (what == "b") sprintf("%s%s", mf(x$estimate, 3), stars(x$p)) else sprintf("(%s)", f2(x$se, 3))
  }
  # columns (1)-(7); (7) = (5) with party FE (PLLM-0201). Seven columns need \scriptsize and 4pt column separation to fit the text width.
  MODS <- paste0("m", 1:7)
  rows <- unlist(imap(COEFS, function(lbl, term) c(
    paste0(lbl, " & ", paste(map_chr(MODS, cell, term = term, what = "b"), collapse = " & "), " \\\\"),
    paste0("& ", paste(map_chr(MODS, cell, term = term, what = "se"), collapse = " & "), " \\\\"))))
  m1c <- r |> filter(model == "m1c", term == "populism_overall")
  m4c <- r |> filter(model == "m4c", term == "populism_overall")
  r <- r |> filter(!model %in% c("m1c", "m4c"))
  gof <- r |> distinct(model, nobs, r2, ar2, rmse, n_clusters) |> arrange(model)
  stopifnot(identical(gof$model, MODS))
  c(sprintf("%% %s; %s", SRC_NOTE, VC_NOTE[[vcn]]),
    "\\begin{table}",
    sprintf("\\caption{Regression Results: %s on Populism and Ideology}", LIB_DVS[[dv]]),
    sprintf("\\label{tab:%d}", TAB_NO[[dv]]), "\\scriptsize\\setlength{\\tabcolsep}{4pt}", "\\centering",
    "\\begin{tabular}{lrrrrrrr}", "\\toprule",
    sprintf(" & \\multicolumn{7}{c}{%s} \\\\", LIB_DVS[[dv]]),
    " & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\\\", " \\midrule",
    rows, "\\midrule",
    # PLLM-0240: readable labels, thousands separators
    "Country FE & YES & YES & YES & YES & YES & YES & YES\\\\",
    "Year FE & YES & YES & NO & YES & YES & YES & YES\\\\",
    "Party FE & NO & NO & NO & NO & NO & YES & YES\\\\",
    paste0("Observations & ", paste(comma(gof$nobs), collapse = " & "), " \\\\"),
    paste0("Clusters & ", paste(comma(gof$n_clusters), collapse = " & "), " \\\\"),
    paste0("Adjusted $R^2$ & ", paste(mf(gof$ar2, 3), collapse = " & "), " \\\\"),
    paste0("RMSE & ", paste(f2(gof$rmse, 2), collapse = " & "), " \\\\"),
    "\\bottomrule",
    # PLLM-0248: column (4) on the sample of column (6), a same-sample comparison for the party-FE step;
    # PLLM-0249: Table 8 carries the financial-market missingness line
    sprintf("\\multicolumn{8}{p{0.9\\linewidth}}{\\scriptsize %s. Party fixed effects use the CMP party code. Column (7) is the interaction model of column (5) with party fixed effects. Column (1) on the sample of column (4): %s%s (t = %s, N = %s). Column (4) on the sample of column (6): %s%s (SE %s, N = %s).%s *** p$<$0.01, ** p$<$0.05, * p$<$0.1.} \\\\",
            VC_NOTE[[vcn]], mf(m1c$estimate, 3), stars(m1c$p), mf(m1c$t, 2), comma(m1c$nobs),
            mf(m4c$estimate, 3), stars(m4c$p), f2(m4c$se, 3), comma(m4c$nobs),
            if (dv == "lib_financial_market") fin_note else ""),
    "\\end{tabular}", "\\end{table}")
}
for (dv in names(LIB_DVS)) for (vcn in names(VCOVS))
  write_tex(reg_table_tex(dv, vcn), sprintf("tab_reg_%d_%s_%s.tex", TAB_NO[[dv]], dv, vcn))

# ---- 7b. Liberalism dimensions on the populism sub-dimensions (tab:pop_subdims; PLLM-0250) -----------
# CONTR-007: the definitions of anti-elitism and political liberalism overlap (both talk about elites
# and institutions); people-centrism has no liberalism counterpart and is the cleanest test. Column (1)
# specification of Tables 4-8 with each populism sub-dimension in place of overall populism: country +
# year FE, SE clustered by party (CMP code); each cell on its own complete cases.
POP_SUB <- c(pop_anti_elitism = "Anti-elitism", pop_people_centrism = "People-centrism",
             pop_manichean = "Manichaean", populism_overall = "Populism (overall)")
subdims <- map_dfr(names(LIB_DVS), function(dv) map_dfr(names(POP_SUB), function(x) {
  m <- feols(as.formula(paste(dv, "~", x, "| countryname + year")), data = d, vcov = ~party)
  ct <- coeftable(m)
  tibble(dv = dv, x = x, estimate = ct[x, 1], se = ct[x, 2], t = ct[x, 3], p = ct[x, 4], nobs = nobs(m),
         n_clusters = n_distinct(d$party[obs(m)]), wr2 = fitstat(m, "wr2", simplify = TRUE)[[1]],
         # standardised slope (SD of the outcome per SD of the regressor, estimation sample)
         beta_std = ct[x, 1] * sd(d[[x]][obs(m)]) / sd(d[[dv]][obs(m)]))
}))
write_csv_det(subdims, file.path(CSV, "41_pop_subdims.csv"))
sd_cell <- function(dv, x, what) { z <- subdims[subdims$dv == dv & subdims$x == x, ]
  if (what == "b") sprintf("%s%s", mf(z$estimate, 3), stars(z$p)) else if (what == "se") sprintf("(%s)", f2(z$se, 3)) else
    sprintf("[%s]", mf(z$beta_std, 2)) }
write_tex(c(paste("%", SRC_NOTE, "; SE clustered by party (CMP party code)"),
  "\\begin{table}[ht]", "\\caption{Liberalism Dimensions on the Populism Sub-Dimensions}", "\\label{tab:pop_subdims}",
  "\\scriptsize", "\\centering", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%",
  "\\begin{tabular}{lrrrrr}", "\\toprule",
  paste0("Regressor & ", paste(sub("^Liberalism \\((.*)\\)$", "\\1", LIB_DVS), collapse = " & "), " \\\\"), "\\midrule",
  unlist(map(names(POP_SUB), function(x) c(
    paste0(POP_SUB[[x]], " & ", paste(map_chr(names(LIB_DVS), sd_cell, x = x, what = "b"), collapse = " & "), " \\\\"),
    paste0(" & ", paste(map_chr(names(LIB_DVS), sd_cell, x = x, what = "se"), collapse = " & "), " \\\\"),
    paste0(" & ", paste(map_chr(names(LIB_DVS), sd_cell, x = x, what = "std"), collapse = " & "), " \\\\")))),
  "\\midrule",
  paste0("Observations & ", paste(map_chr(names(LIB_DVS), function(dv) {
    n <- range(subdims$nobs[subdims$dv == dv]); if (n[1] == n[2]) comma(n[1]) else paste0(comma(n[1]), "--", comma(n[2])) }), collapse = " & "), " \\\\"),
  "\\bottomrule", "\\end{tabular}}", "\\par\\smallskip",
  "\\begin{minipage}{\\linewidth}\\scriptsize Each cell is a separate regression of the liberalism measure in the column on the populism measure in the row, the specification of column (1) of Tables 4--8: country and year fixed effects, standard errors clustered by party (CMP party code) in parentheses, standardised coefficient (standard deviations of the outcome per standard deviation of the regressor, estimation sample) in brackets. All scores 0--10, means over the three LLMs. Each regression on its own complete cases. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\\end{minipage}",
  "\\end{table}"), "tab_pop_subdims.tex")

# ---- 8. Figures 6-10: marginal effect of POP by IDEO, interaction tests ----------------------
me_curve <- function(m, vc, grid) {
  b <- coef(m); V <- vcov(m, vcov = vc)
  i1 <- "populism_overall"; i3 <- "populism_overall:IDEO"
  tibble(IDEO = grid, me = b[i1] + b[i3] * grid,
         se = sqrt(V[i1, i1] + grid^2 * V[i3, i3] + 2 * grid * V[i1, i3]))
}
ideo_q <- quantile(d$IDEO, c(0, .05, .10, .5, .90, .95, 1), na.rm = TRUE)
grid <- seq(ideo_q[["0%"]], ideo_q[["100%"]], length.out = 201)
inter_tab <- imap_dfr(models, function(ms, dv) {
  imap_dfr(VCOVS, function(vc, vcn) {
    m5 <- ms$m5; ct <- coeftable(summary(m5, vcov = vc))
    pts <- me_curve(m5, vc, unname(ideo_q[c("0%", "5%", "10%", "90%", "95%", "100%")])) |>
      mutate(at = c("min", "p5", "p10", "p90", "p95", "max"))
    G <- n_distinct(d[[all.vars(vc)]][obs(m5)])
    tibble(dv = dv, vcov = vcn, n = nobs(m5), n_clusters = G,
           b_pop = ct["populism_overall", 1], b_inter = ct["populism_overall:IDEO", 1],
           se_inter = ct["populism_overall:IDEO", 2], t_inter = ct["populism_overall:IDEO", 3],
           p_inter = ct["populism_overall:IDEO", 4], b_pop_m4 = coef(ms$m4)[["populism_overall"]],
           ideo_p10 = ideo_q[["10%"]], ideo_p90 = ideo_q[["90%"]]) |>
      bind_cols(pts |> select(at, me, se) |> pivot_wider(names_from = at, values_from = c(me, se)))
  })
}) |>
  # same reference distribution as fixest's clustered p-values in the tables: t(G - 1) (chain-review #8)
  mutate(p_me_p10 = 2 * pt(-abs(me_p10 / se_p10), df = n_clusters - 1),
         p_me_p90 = 2 * pt(-abs(me_p90 / se_p90), df = n_clusters - 1))
write_csv_det(inter_tab, file.path(CSV, "41_fig6_10_interaction_tests.csv"))
for (dv in names(LIB_DVS)) for (vcn in names(VCOVS)) {
  m5 <- models[[dv]]$m5; ref <- coef(models[[dv]]$m4)[["populism_overall"]]
  cur <- me_curve(m5, VCOVS[[vcn]], grid)
  crit <- qt(0.975, df = n_distinct(d[[all.vars(VCOVS[[vcn]])]][obs(m5)]) - 1)
  p <- ggplot(cur, aes(IDEO, me)) +
    geom_ribbon(aes(ymin = me - crit * se, ymax = me + crit * se), alpha = 0.2) +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2, col = "gray") +
    geom_hline(yintercept = ref, lty = 2, col = "red") +
    geom_rug(data = d |> filter(!is.na(IDEO)), aes(x = IDEO), inherit.aes = FALSE, alpha = 0.15, sides = "b") +
    theme_minimal() +
    labs(title = paste("Marginal Effect of Populism on", str_replace_all(FILE_LBL[[dv]], "_", " "), "by Ideology"),
         subtitle = sprintf("Model (5), country + year FE; 95%% CI, %s. Red: coefficient of model (4) = %s",
                            ifelse(vcn == "clparty", "SE clustered by party", "SE clustered by country"), f2(ref, 3)),
         x = ideo_lab, y = paste("Marginal Effect of Populism on", str_replace_all(FILE_LBL[[dv]], "_", " ")))
  suffix <- if (vcn == "clparty") "" else "_clcountry"
  save_pdf(p, file.path(FIG, sprintf("Marginal_Effect_Populism_on_%s_by_Ideology%s.pdf", FILE_LBL[[dv]], suffix)), 8, 5)
}

# ---- 9. Table A.1 (tab:stat1): elections per country, analysis sample -------------------------
stat_1 <- an |>
  group_by(countryname, date) |> summarise(n_parties_el = n(), .groups = "drop") |>
  group_by(countryname) |>
  summarise(n_elections = n(), first_el = as.character(min(date)), last_el = as.character(max(date)),
            min_p = min(n_parties_el), max_p = max(n_parties_el),
            mean_p = mean(n_parties_el), median_p = median(n_parties_el), .groups = "drop") |>
  left_join(an |> distinct(countryname, party) |> count(countryname, name = "n_parties"), by = "countryname") |>
  relocate(n_parties, .after = n_elections) |> arrange(countryname)
write_csv_det(stat_1, file.path(CSV, "41_tabA1_stat1.csv"))
write_tex(c(paste("%", SRC_NOTE, "; analysis sample (POP and LIB overall non-missing)"),
  "{\\footnotesize\\setlength{\\tabcolsep}{4pt}", "\\begin{longtable}{lrrrrrrrr}", "  \\toprule",
  "  & & & \\multicolumn{2}{c}{Election dates} & \\multicolumn{4}{c}{No. of Parties}\\\\",
  "Country & \\# Elections & \\# Parties & First & Last & Min & Max & Mean & Median \\\\", "  \\midrule",
  with(stat_1, sprintf("%s & %d & %d & %s & %s & %d & %d & %s & %s \\\\", countryname, n_elections, n_parties,
                       first_el, last_el, min_p, max_p, f2(mean_p), f2(median_p))),
  "   \\bottomrule",
  # PLLM-0240: visible sample note
  sprintf("\\multicolumn{9}{p{0.9\\linewidth}}{\\scriptsize Analysis sample: %s manifestos of %s parties with populism and overall liberalism scores (corpus: %s manifestos, %s parties).} \\\\",
          comma(nrow(an)), comma(n_distinct(an$party)), comma(nrow(d)), comma(n_distinct(d$party))),
  "\\caption{Descriptive Statistics of Elections per Country}",
  "\\label{tab:stat1}", "\\end{longtable}", "}"), "tab_stat1.tex")

# ---- 10. Table A.10 (tab:evidence_shares) and the A.10 computational summary -----------------
cl <- load_chunk_dims(PIPE_IN, PIPE_DERIVED)
ev_dims <- c("Populism (overall)" = "populism_overall", "Anti-elitism" = "pop_anti_elitism",
             "People-centrism" = "pop_people_centrism", "Manichaean" = "pop_manichean",
             "Ideology (overall)" = "pop_ideology_overall", "Political liberalism" = "lib_political",
             "Social liberalism" = "lib_social", "Economic liberalism" = "lib_economic",
             "Financial-market liberalism" = "lib_financial_market", "Liberalism (overall)" = "liberalism_overall")
lv <- validation_levels(RULE)
cl_ev <- cl |> mutate(lv = lv[dim],
                      enters = status %in% "verified" & !is.na(score) &
                        (lv == "none" | (lv == "quote" & quote_ok) | (lv == "quote_topic" & quote_ok & topic_ok)))
ev <- imap_dfr(ev_dims, function(v, lbl) {
  x <- cl_ev |> filter(dim == v)
  tibble(dimension = lbl, dim = v, n_chunk_dims = nrow(x),
         verified = 100 * mean(x$status %in% "verified"), mixed = 100 * mean(x$status %in% "mixed"),
         no_evidence = 100 * mean(x$status %in% "no_evidence"), parse_lost = 100 * mean(is.na(x$status)),
         verified_span_validated = 100 * mean(x$enters))
})
write_csv_det(ev, file.path(CSV, "41_tabA10_evidence_shares.csv"))
x10 <- cl_ev |> filter(dim %in% ev_dims)
comp <- tibble(
  item = c("manifestos in the scoring pool", "chunk records, all three models", "chunk records per model (mean)",
           "share of dimension judgements with status verified (10 dims, %)",
           "share verified and passing span validation (10 dims, %)",
           "share no_evidence (%)", "share mixed (%)", "share parse-lost (Claude, status missing, %)"),
  value = c(nrow(d), n_distinct(paste(cl$model, cl$doc_id)), n_distinct(paste(cl$model, cl$doc_id)) / 3,
            100 * mean(x10$status %in% "verified"), 100 * mean(x10$enters),
            100 * mean(x10$status %in% "no_evidence"), 100 * mean(x10$status %in% "mixed"),
            100 * mean(is.na(x10$status))))
write_csv_det(comp, file.path(CSV, "41_A10_computational_summary.csv"))
write_tex(c(paste("%", SRC_NOTE, "; pooled over the 3 models, all scored chunks"),
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Per-dimension evidence shares (percent), pooled across the three model families}",
  "\\label{tab:evidence_shares}", "\\footnotesize", "\\begin{tabular}{lrrrrr}", "\\toprule",
  "Dimension & Verified & Mixed & No evidence & Not parsed & Verified, span valid \\\\", "\\midrule",
  with(ev, sprintf("%-29s & %s & %s & %s & %s & %s \\\\", dimension, f2(verified, 1), f2(mixed, 1),
                   f2(no_evidence, 1), f2(parse_lost, 1), f2(verified_span_validated, 1))),
  "\\bottomrule", "\\end{tabular}", "\\end{table}"), "tab_evidence_shares.tex")

# ---- 11. Headline coefficients under baseline variants (all from R/1_Aggregation.R) --------------
meta <- d |> select(doc_id, countryname, party, year)
variant_data <- function(validation = RULE, stat = "cwm", models = AGG_MODELS, cl_in = cl, statuses = "verified") {
  combine_models(aggregate_within(cl_in, validation = validation, statuses = statuses), models = models, stat = stat) |>
    inner_join(meta, by = "doc_id")
}
cl_gem_june <- load_chunk_dims(PIPE_IN, PIPE_DERIVED, gemini_norm = "june")
cl_gap_fb   <- load_chunk_dims(PIPE_IN, PIPE_DERIVED, gap_legs = "june_fallback")

# PLLM-0247 (EMP-004 / EMP-009, 2026-11-11): three aggregation forks, as local wrappers around the
# chunk-dim table of R/1_Aggregation.R (R/1 itself is unchanged; aggregate_within() and
# combine_models() do the aggregation in all three).
# (a) Populism chunks without evidence count as 0. For the four populism dimensions (anti-elitism,
#     people-centrism, Manichaean, overall; NOT the pop_ideology_* dims) a chunk with status
#     no_evidence gets score 0, enters as verified and passes the span check (there is no span to
#     check); its weight is the chunk's populism confidence, as for every populism chunk. Liberalism
#     and ideology are unchanged. Mixed and parse-lost chunks stay out, as in the baseline.
POP_DIMS0 <- c("pop_anti_elitism", "pop_people_centrism", "pop_manichean", "populism_overall")
cl_pop0 <- cl |>
  mutate(z0 = dim %in% POP_DIMS0 & status %in% "no_evidence",
         score = if_else(z0, 0, score), status = if_else(z0, "verified", status),
         quote_ok = quote_ok | z0, topic_ok = topic_ok | z0) |> select(-z0)
# (b) Chunk weights by chunk length in tokens instead of confidence (all 13 dims). The chunk texts are
#     not part of the public inputs, so the lengths are reconstructed from the chunking rule of the
#     scorers (R/15b, R/16, R/18: whole text if <= 18,000 tokens, else chunks of 20,000 tokens with
#     2,000 overlap; every chunk but the last has exactly 20,000 tokens) and the number of chunks K of
#     the manifesto (max chunk_id per model). Only the length of the last chunk is unknown: it is
#     T - (K - 1) x 18,000 for a manifesto of T tokens, with T in (max(20,000, (K-1) x 18,000),
#     K x 18,000]. T is estimated as r_c x W, W = the word-based token proxy of the metadata
#     (token_count = 0.75 x words), r_c = the country median of the interval midpoints over W across
#     multi-chunk manifestos (all-corpus median where a country has none), clamped into the interval.
#     Check against the tokenizer on 250 random multi-chunk manifestos (2026-11-11, scratch, full
#     mode): chunk count implied by the true T agrees for 90%, r(estimated, true last-chunk length) =
#     0.80, mean absolute error 2,200 tokens (11% of a full chunk). Single-chunk manifestos are
#     unaffected (one chunk, any weight). Deterministic and identical in public mode.
tok_meta <- read_parquet(PIPE_META, col_select = c("doc_id", "countryname", "token_count")) |>
  distinct(doc_id, .keep_all = TRUE)
chunk_len <- cl |> distinct(model, doc_id_root, chunk_id) |>
  group_by(model, doc_id_root) |> mutate(K = max(chunk_id)) |> ungroup() |>
  left_join(tok_meta, by = c("doc_id_root" = "doc_id")) |>
  mutate(lo = if_else(K == 1L, 0, pmax(20000, (K - 1) * 18000)), hi = if_else(K == 1L, 20000, K * 18000))
r_len <- chunk_len |> filter(K >= 2) |> distinct(doc_id_root, countryname, K, lo, hi, token_count) |>
  mutate(rm = (lo + hi) / 2 / token_count)
r_ctry <- r_len |> group_by(countryname) |> summarise(r_c = median(rm), .groups = "drop")
chunk_len <- chunk_len |> left_join(r_ctry, by = "countryname") |>
  mutate(r_c = coalesce(r_c, median(r_len$rm)),
         T_hat = pmin(pmax(r_c * token_count, lo + 1), hi),
         tokens = if_else(K == 1L, T_hat, if_else(chunk_id < K, 20000, T_hat - (K - 1) * 18000)))
stopifnot("chunk length missing" = !anyNA(chunk_len$tokens), all(chunk_len$tokens > 0))
cl_len <- cl |> left_join(chunk_len |> select(model, doc_id_root, chunk_id, tokens), by = c("model", "doc_id_root", "chunk_id")) |>
  mutate(w_block = tokens) |> select(-tokens)
# (c) Chunks with evidence status "mixed" enter as well (statuses verified + mixed; all dims).

# Share of manifestos whose score rests on a strict subset of their chunks (EMP-004): per model and
# dimension, chunks that enter under the baseline rule vs all scored chunks of the manifesto.
agg_base <- aggregate_within(cl, validation = RULE)
chunk_subset <- cl |> filter(dim %in% c("populism_overall", "liberalism_overall")) |>
  count(model, doc_id = doc_id_root, dim, name = "n_chunks_total") |>
  left_join(agg_base |> select(model, doc_id, dim, n_enter = n_chunks), by = c("model", "doc_id", "dim")) |>
  mutate(n_enter = coalesce(n_enter, 0L))
subset_share <- bind_rows(chunk_subset, chunk_subset |> mutate(model = "all models (manifesto x model)")) |>
  group_by(model, dim) |>
  summarise(manifestos = n(), with_score = sum(n_enter > 0), multi_chunk = sum(n_chunks_total > 1),
            share_strict_subset_of_scored = mean((n_enter < n_chunks_total)[n_enter > 0]),
            share_strict_subset_of_scored_multichunk = mean((n_enter < n_chunks_total)[n_enter > 0 & n_chunks_total > 1]),
            share_no_chunk_enters = mean(n_enter == 0), mean_share_chunks_entering = mean(n_enter / n_chunks_total),
            .groups = "drop") |>
  arrange(dim, model)
# cross-model manifesto score: does at least one contributing model use a strict subset?
subset_any <- chunk_subset |> filter(n_enter > 0) |> group_by(doc_id, dim) |>
  summarise(any_strict = any(n_enter < n_chunks_total), .groups = "drop") |>
  group_by(dim) |> summarise(model = "cross-model score (any contributing model)", with_score = n(),
                             share_strict_subset_of_scored = mean(any_strict), .groups = "drop")
write_csv_det(bind_rows(subset_share, subset_any) |> arrange(dim, model), file.path(CSV, "41_chunk_subset_share.csv"))
variants <- list(
  "Baseline (quote check, own-block weights)"    = d,
  "Span validation: none"                        = variant_data("none"),
  "Span validation: quote and topic check on the left/right/centrist and overall scores" = variant_data("r12"),
  "Span validation: quote and topic check on all dimensions" = variant_data("full"),
  "No span check, populism-confidence weights"   = variant_data("none", "cwm_pop"),
  "Gemini: AI Studio alias rows for 131 docs"    = variant_data(cl_in = cl_gem_june),
  "Gap legs filled from the initial scoring pass" = variant_data(cl_in = cl_gap_fb),
  "Sonnet only"                                  = variant_data(models = "sonnet"),
  "GPT-4.1-mini only"                            = variant_data(models = "gpt41mini"),
  "Gemini only"                                  = variant_data(models = "gemini"),
  "Post-2010 elections"                          = d |> filter(year >= 2010),
  # PLLM-0247 forks (see above)
  "Populism: chunks without evidence count as 0"  = variant_data(cl_in = cl_pop0),
  "Chunk weights: chunk length (tokens) instead of confidence" = variant_data(cl_in = cl_len),
  "Evidence status: verified and mixed chunks"    = variant_data(statuses = c("verified", "mixed")))
# A variant can leave too few observations for a column (e.g. Gemini-only IDEO under the topic
# check); such cells are NA rather than an error.
fit_six_safe <- function(dv, data) {
  ms <- map(c(m1 = 1, m2 = 2, m3 = 3, m4 = 4, m5 = 5, m6 = 6), function(k)
    tryCatch(fit_six_one(dv, data, k), error = function(e) NULL))
  ms
}
fit_six_one <- function(dv, data, k) {
  f <- function(rhs, fe) as.formula(paste(dv, "~", rhs, "|", fe))
  spec <- list(c("populism_overall", "countryname + year"), c("IDEO", "countryname + year"),
               c("populism_overall + IDEO", "countryname"), c("populism_overall + IDEO", "countryname + year"),
               c("populism_overall * IDEO", "countryname + year"), c("populism_overall + IDEO", "countryname + year + party"))[[k]]
  feols(f(spec[1], spec[2]), data = data)
}
sens <- imap_dfr(variants, function(dat, vname) {
  map_dfr(names(LIB_DVS), function(dv) {
    ms <- fit_six_safe(dv, dat |> mutate(IDEO = if ("IDEO" %in% names(dat)) IDEO else pop_ideology_direction))
    imap_dfr(VCOVS, function(vc, vcn) {
      g <- function(m, term) {
        if (is.null(m)) return(c(b = NA_real_, se = NA_real_, p = NA_real_))
        ct <- coeftable(summary(m, vcov = vc)); c(b = ct[term, 1], se = ct[term, 2], p = ct[term, 4]) }
      nobs0 <- function(m) if (is.null(m)) 0L else nobs(m)
      tibble(variant = vname, dv = dv, vcov = vcn,
             b1_pop = g(ms$m1, "populism_overall")[["b"]], se1_pop = g(ms$m1, "populism_overall")[["se"]],
             p1_pop = g(ms$m1, "populism_overall")[["p"]], n1 = nobs0(ms$m1),
             b4_pop = g(ms$m4, "populism_overall")[["b"]], p4_pop = g(ms$m4, "populism_overall")[["p"]],
             b4_ideo = g(ms$m4, "IDEO")[["b"]], p4_ideo = g(ms$m4, "IDEO")[["p"]],
             b5_inter = g(ms$m5, "populism_overall:IDEO")[["b"]], p5_inter = g(ms$m5, "populism_overall:IDEO")[["p"]],
             b6_pop = g(ms$m6, "populism_overall")[["b"]], p6_pop = g(ms$m6, "populism_overall")[["p"]],
             n4 = nobs0(ms$m4), n6 = nobs0(ms$m6))
    })
  })
})
write_csv_det(sens, file.path(CSV, "41_baseline_variants.csv"))
sens_tex <- sens |> filter(vcov == "clparty") |>
  mutate(cell1 = sprintf("%s%s", mf(b1_pop, 3), stars(p1_pop)), cell4 = sprintf("%s%s", mf(b4_pop, 3), stars(p4_pop)),
         cell5 = sprintf("%s%s", mf(b5_inter, 3), stars(p5_inter)), cell6 = sprintf("%s%s", mf(b6_pop, 3), stars(p6_pop)),
         across(c(cell1, cell4, cell5, cell6), ~ if_else(str_detect(.x, "NA"), "--", .x)))
write_tex(c(paste("%", SRC_NOTE, "; SE clustered by party"),
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Populism coefficients of tables 4 to 8 under alternative measurement choices}",
  "\\label{tab:robust_measurement}", "\\scriptsize", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%", "\\begin{tabular}{llrrrrrr}", "\\toprule",
  "Variant & Outcome & $\\beta_{POP}$ (1) & N (1) & $\\beta_{POP}$ (4) & $\\beta_{POP\\times IDEO}$ (5) & $\\beta_{POP}$ (6) & N (4) \\\\",
  "\\midrule",
  with(sens_tex, sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\", str_replace_all(variant, "_", "\\\\_"), LIB_DVS[dv],
                         cell1, comma(n1), cell4, cell5, cell6, comma(n4))),
  "\\bottomrule", "\\end{tabular}}",
  "\\par\\smallskip",
  "\\begin{minipage}{\\linewidth}\\scriptsize Columns of Tables 4--8: (1) populism only, (4) populism and ideology, (5) with their interaction, (6) column (4) with party fixed effects (CMP party code). Country and year fixed effects in all columns; standard errors clustered by party (CMP party code). N (1), N (4): observations of columns (1) and (4); --: too few observations for the column. Last three rows (aggregation within model): chunks without populist evidence scored 0 for the four populism dimensions instead of left out; chunks weighted by their length in tokens instead of the model's confidence (length of the last chunk of a manifesto estimated from its word count); chunks with mixed evidence included. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\\end{minipage}",
  "\\end{table}"), "tab_baseline_variants.tex")

# ---- 12. Paper figures on the June panel axes (figures/june_axes/, PLLM-0065) ---------------------
# Second set of the figures of sections 5, 6 and 8 with the axis limits of the June 2026 paper
# figures (paper/R0/graphics), which ggplot had auto-scaled to the Nov-2025 data; the figures above
# keep their fixed axes (IDEO -1..1, scores 0..10) and are unchanged. Same data and plot code;
# only the axes and a caption differ. Logic of hqg/share/2026-10-27_briefing/presMAD2_v2026a/
# make_fig_oldscale.R, but the June limits are hard-coded here (JUNE_AXES; the old_* columns of its
# fig_oldscale/axis_limits.csv, rounded to 2 decimals there) instead of being recomputed from the
# Nov-2025 data. Where v2026a data fall outside a June limit, the axis is widened to the data
# range and the caption says so. Marginal effects: June used ylim(lo, 0) and the full IDEO range;
# lo is widened downward where the lower 95% band goes below it. No June limits exist for the
# other country panels, Poland, the *_by_Ideology trends or Liberalism_Over_Time, so those are
# not redrawn.
FIG_J <- file.path(FIG, "june_axes"); dir.create(FIG_J, showWarnings = FALSE)
JUNE_AXES <- tribble(
  ~figure,                                    ~x_lo, ~x_hi, ~y_lo, ~y_hi,
  "Austria_Populism_vs_Populist_Ideology",    -0.44,  0.50,  2,     8,
  "Germany_Populism_vs_Populist_Ideology",    -0.49,  0.46,  2,     8,
  "Spain_Populism_vs_Populist_Ideology",      -0.58,  0.54,  2,     9,
  "Germany_Economic_Liberalism_vs_Populism",   2,     8,     5,     8,
  "Populism_Over_Time",                        NA,    NA,    3.86,  6.34,
  "Marginal_Effect_Populism_on_Liberalism_by_Ideology",                  -1, 1, -0.30, 0,
  "Marginal_Effect_Populism_on_Political_Liberalism_by_Ideology",        -1, 1, -0.30, 0,
  "Marginal_Effect_Populism_on_Social_Liberalism_by_Ideology",           -1, 1, -0.30, 0,
  "Marginal_Effect_Populism_on_Economic_Liberalism_by_Ideology",         -1, 1, -0.35, 0,
  "Marginal_Effect_Populism_on_Financial_Market_Liberalism_by_Ideology", -1, 1, -0.50, 0)
widen <- function(lim, x) { r <- range(x, na.rm = TRUE); c(min(lim[1], r[1]), max(lim[2], r[2])) }
axis_note <- function(axis, old, used) if (isTRUE(all.equal(old, used))) NULL else
  sprintf("June %s-axis %s..%s widened to %s..%s", axis, f2(old[1]), f2(old[2]), f2(used[1]), f2(used[2]))
june_caption <- function(notes) if (length(notes)) paste0("v2026a scores; ", paste(notes, collapse = "; "), " so the data fit") else
  "v2026a scores; axes as in the June figure"
june_log <- list()
for (k in which(str_detect(JUNE_AXES$figure, "_vs_"))) {
  lim <- JUNE_AXES[k, ]; ctry <- str_extract(lim$figure, "^[A-Za-z]+")
  spec <- plot_specs |> filter(suffix == str_remove(lim$figure, "^[A-Za-z]+_"))
  df <- plot_dat |> filter(countryname == ctry, !is.na(.data[[spec$xvar]]), !is.na(.data[[spec$yvar]]))
  if (ctry == "Spain") df <- df |> filter(grepl(spain_parties_regex, `Party Name`))
  xl <- widen(c(lim$x_lo, lim$x_hi), df[[spec$xvar]]); yl <- widen(c(lim$y_lo, lim$y_hi), df[[spec$yvar]])
  notes <- c(axis_note("x", c(lim$x_lo, lim$x_hi), xl), axis_note("y", c(lim$y_lo, lim$y_hi), yl))
  p <- plot_country_scatter(plot_dat, ctry, spec$xvar, spec$yvar, spec$xlab, spec$ylab, spec$title) +
    coord_cartesian(xlim = xl, ylim = yl) + labs(caption = june_caption(notes))
  save_pdf(p, file.path(FIG_J, paste0(lim$figure, ".pdf")), width = 15, height = 8)
  june_log[[length(june_log) + 1]] <- tibble(figure = lim$figure, x_june_lo = lim$x_lo, x_june_hi = lim$x_hi,
    y_june_lo = lim$y_lo, y_june_hi = lim$y_hi, x_used_lo = xl[1], x_used_hi = xl[2], y_used_lo = yl[1], y_used_hi = yl[2],
    widened = length(notes) > 0)
}
# Populism over time: the June y-range must hold the year means and the 95% band of the fitted line
lim <- JUNE_AXES |> filter(figure == "Populism_Over_Time")
ya <- year_agg |> filter(year >= 1960)
pr <- predict(lm(populism_overall ~ year, ya), ya, se.fit = TRUE)
yl <- widen(c(lim$y_lo, lim$y_hi), c(ya$populism_overall, pr$fit - 1.96 * pr$se.fit, pr$fit + 1.96 * pr$se.fit))
notes <- axis_note("y", c(lim$y_lo, lim$y_hi), yl)
save_pdf(plot_trend(year_agg, "populism_overall", "Average Populism Score in CMP Manifestos Over Time", "Populism Overall Score") +
           coord_cartesian(ylim = yl) + labs(caption = june_caption(notes)),
         file.path(FIG_J, "Populism_Over_Time.pdf"), 10, 6)
june_log[[length(june_log) + 1]] <- tibble(figure = lim$figure, x_june_lo = NA_real_, x_june_hi = NA_real_,
  y_june_lo = lim$y_lo, y_june_hi = lim$y_hi, x_used_lo = NA_real_, x_used_hi = NA_real_, y_used_lo = yl[1], y_used_hi = yl[2],
  widened = length(notes) > 0)
# Marginal effects (party-clustered version, as in the paper)
for (dv in names(LIB_DVS)) {
  fig <- sprintf("Marginal_Effect_Populism_on_%s_by_Ideology", FILE_LBL[[dv]]); lim <- JUNE_AXES |> filter(figure == fig)
  m5 <- models[[dv]]$m5; ref <- coef(models[[dv]]$m4)[["populism_overall"]]
  cur <- me_curve(m5, VCOVS$clparty, grid)
  crit <- qt(0.975, df = n_distinct(d$party[obs(m5)]) - 1)
  xl <- widen(c(lim$x_lo, lim$x_hi), grid); yl <- c(min(lim$y_lo, min(cur$me - crit * cur$se)), lim$y_hi)
  notes <- c(axis_note("x", c(lim$x_lo, lim$x_hi), xl), axis_note("y", c(lim$y_lo, lim$y_hi), yl))
  p <- ggplot(cur, aes(IDEO, me)) +
    geom_ribbon(aes(ymin = me - crit * se, ymax = me + crit * se), alpha = 0.2) +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2, col = "gray") +
    geom_hline(yintercept = ref, lty = 2, col = "red") +
    geom_rug(data = d |> filter(!is.na(IDEO)), aes(x = IDEO), inherit.aes = FALSE, alpha = 0.15, sides = "b") +
    theme_minimal() + coord_cartesian(xlim = xl, ylim = yl) +
    labs(title = paste("Marginal Effect of Populism on", str_replace_all(FILE_LBL[[dv]], "_", " "), "by Ideology"),
         subtitle = sprintf("Model (5), country + year FE; 95%% CI, SE clustered by party. Red: coefficient of model (4) = %s", f2(ref, 3)),
         x = ideo_lab, y = paste("Marginal Effect of Populism on", str_replace_all(FILE_LBL[[dv]], "_", " ")),
         caption = june_caption(notes))
  save_pdf(p, file.path(FIG_J, paste0(fig, ".pdf")), 8, 5)
  june_log[[length(june_log) + 1]] <- tibble(figure = fig, x_june_lo = lim$x_lo, x_june_hi = lim$x_hi,
    y_june_lo = lim$y_lo, y_june_hi = lim$y_hi, x_used_lo = xl[1], x_used_hi = xl[2], y_used_lo = yl[1], y_used_hi = yl[2],
    widened = length(notes) > 0)
}
write_csv_det(bind_rows(june_log), file.path(CSV, "41_fig_june_axes_limits.csv"))

# ---- 13. Combined paper figures for v2026a (figures/paper_v2026a/; PLLM-0119..0122) -------------------
# Approved 2026-10-27 from the drafts in hqg/share/2026-10-27_briefing/fig_proposals/fig_proposals.R
# (same samples, models and formulas; patchwork not needed, facets only):
#   Trends_by_Ideology.pdf       five-year manifesto-level means of POP and LIB (overall) with 95% CI,
#                                Left / Right = IDEO <= 0 / > 0 (as section 6), 1960+, 2020-2025 one period
#   Marginal_Effects_Panels.pdf  model (5) ME curve of POP by IDEO with 95% CI, five outcomes, common
#                                y-range (-1.0, 0.1); dashed red line = model (1) on the same sample
#   Marginal_Effects_Dots.pdf    ME at the p10 / median / p90 of IDEO for the five outcomes
# Models (decision 2026-10-27): the marginal-effect figures use exactly the Tables 4-8 models of
# section 7, models[[dv]]$m5 (and $m1 for the dashed reference line), each outcome on its own
# complete-case sample, SE clustered by party (CMP code); so every number agrees with the tables.
# Only the evaluation points of IDEO (p10 / median / p90) come from the analysis sample `an`
# (POP and LIB overall non-missing). CI = estimate +- t(G-1, 0.975) SE with G = party clusters, the
# convention of the section-8 figures (changed from the draft's 1.96 on 2026-10-27). No repelled
# labels: the layout does not depend on machine load.
FIG_P <- file.path(FIG, "paper_v2026a"); dir.create(FIG_P, showWarnings = FALSE)
th13 <- theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), legend.position = "bottom")
cols13 <- c(Left = "#2b5fb3", Right = "#c0392b", All = "grey30")
tr13 <- an |> filter(year >= 1960, !is.na(IDEO)) |>
  mutate(Ideology = if_else(IDEO <= 0, "Left", "Right"), period = pmin(floor(year / 5) * 5, 2020)) |>
  pivot_longer(c(populism_overall, liberalism_overall), names_to = "var", values_to = "y") |>
  mutate(var = recode(var, populism_overall = "Populism (overall)", liberalism_overall = "Liberalism (overall)")) |>
  group_by(var, Ideology, period) |>
  summarise(m = mean(y), se = sd(y) / sqrt(n()), n = n(), .groups = "drop") |>
  arrange(var, Ideology, period)
write_csv_det(tr13 |> transmute(var, ideology = Ideology, period_start = period,
                                period_end = if_else(period == 2020, 2025, period + 4), mean = m, se, n),
              file.path(CSV, "41_fig_trends_by_ideology.csv"))
# PLLM-0242 (2026-11-11): populism panel on the LEFT, as Section 4 of the paper describes it (facets
# were alphabetical, which put liberalism on the left). The csv above keeps its row order.
pA <- ggplot(tr13 |> mutate(var = factor(var, levels = c("Populism (overall)", "Liberalism (overall)"))),
             aes(period + 2.5, m, colour = Ideology, fill = Ideology)) +
  geom_ribbon(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.3) +
  facet_wrap(~ var, scales = "free_y") + scale_colour_manual(values = cols13) + scale_fill_manual(values = cols13) +
  th13 + labs(x = NULL, y = "Mean score (0-10), five-year periods", colour = NULL, fill = NULL,
              caption = "Manifesto-level means per five-year period (last: 2020-2025) with 95% CI; Left/Right by the sign of the ideology score.")
save_pdf(pA, file.path(FIG_P, "Trends_by_Ideology.pdf"), 8, 3.6)

DV13 <- c(liberalism_overall = "Overall", lib_political = "Political", lib_social = "Social",
          lib_economic = "Economic", lib_financial_market = "Financial markets")
grid13 <- seq(-1, 1, by = 0.02)
q13 <- quantile(an$IDEO, c(.10, .50, .90), na.rm = TRUE, names = FALSE)
AT13 <- c("Left (p10)", "Centre (median)", "Right (p90)")
me13 <- map_dfr(names(DV13), function(dv) {
  m5 <- models[[dv]]$m5; m1 <- models[[dv]]$m1
  ct5 <- coeftable(summary(m5, vcov = VCOVS$clparty))
  bind_rows(me_curve(m5, VCOVS$clparty, grid13) |> mutate(kind = "curve", at = NA_character_),
            me_curve(m5, VCOVS$clparty, q13) |> mutate(kind = "point", at = AT13)) |>
    mutate(var = dv, dv = DV13[[dv]], ref = coef(m1)[["populism_overall"]],
           p_inter = ct5["populism_overall:IDEO", 4], n = nobs(m5),
           crit = qt(0.975, df = n_distinct(d$party[obs(m5)]) - 1))
})
me13$dv <- factor(me13$dv, levels = DV13)
pts13 <- me13 |> filter(kind == "point") |> mutate(at = factor(at, levels = AT13))
write_csv_det(pts13 |> transmute(dv = as.character(dv), at = as.character(at), me, se, p_inter, n, var, ideo_at = IDEO, ref_m1 = ref, crit),
              file.path(CSV, "41_fig_me_points.csv"))
lab13 <- me13 |> distinct(dv, p_inter, n) |>
  mutate(txt = if_else(p_inter < 0.001, "interaction p < 0.001", sprintf("interaction p = %.3f", p_inter)))
pB <- ggplot(me13 |> filter(kind == "curve"), aes(IDEO, me)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_ribbon(aes(ymin = me - crit * se, ymax = me + crit * se), fill = "grey75", alpha = 0.5) +
  geom_line(linewidth = 0.8) +
  geom_hline(aes(yintercept = ref), colour = "#c0392b", linetype = 2) +
  geom_rug(data = an |> filter(!is.na(IDEO)) |> distinct(IDEO), aes(x = IDEO), inherit.aes = FALSE, alpha = 0.08, sides = "b") +
  geom_text(data = lab13, aes(x = -0.95, y = 0.05, label = txt), hjust = 0, size = 3, inherit.aes = FALSE) +
  facet_wrap(~ dv, nrow = 1) + coord_cartesian(ylim = c(-1.0, 0.1)) + th13 +
  labs(x = "Ideology (-1 = left, +1 = right)", y = "Marginal effect of populism",
       caption = "Model (5) of Tables 4-8 (country and year FE, each outcome on its own sample); 95% CI clustered by party; dashed red line: coefficient of model (1).")
save_pdf(pB, file.path(FIG_P, "Marginal_Effects_Panels.pdf"), 10, 3.2)
pC <- ggplot(pts13, aes(me, fct_rev(dv), colour = at)) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_pointrange(aes(xmin = me - crit * se, xmax = me + crit * se), position = position_dodge(width = 0.55), size = 0.4) +
  scale_colour_manual(values = c("#2b5fb3", "grey40", "#c0392b")) + th13 +
  labs(x = "Marginal effect of populism on liberalism (95% CI)", y = NULL, colour = "Evaluated at ideology =",
       caption = "Model (5) of Tables 4-8, country and year FE, SE clustered by party; ideology at its p10 / median / p90 in the analysis sample.")
save_pdf(pC, file.path(FIG_P, "Marginal_Effects_Dots.pdf"), 7, 3.4)

# ---- 13b. Predicted liberalism by ideology (PLLM-0242, CONTR-001; 2026-11-11) ---------------------------
# Level-plus-slope answer to the title question: predicted liberalism over populism for a left-wing
# manifesto (IDEO at its 10th percentile in the analysis sample) and a right-wing one (90th
# percentile), from column (5) of Tables 4-8 (models[[dv]]$m5: LIB ~ POP * IDEO | country + year,
# each outcome on its own complete cases). The fixed effects are held at their sample average: with
# OLS and fixed effects the mean residual is zero, so mean(fitted) = mean(LIB) on the estimation
# sample, and
#   pred(P, I) = mean(LIB) + b1 (P - mean P) + b2 (I - mean I) + b3 (P I - mean(P I)),
# means over the estimation sample. mean(LIB) is a sample quantity, so the variance of pred is that of
# the linear combination of (b1, b2, b3), from the party-clustered vcov. 95% band = pred +- t(G-1)
# SE, G = party clusters (the convention of the section-8 and section-13 figures). Populism runs over
# its 1st to 99th percentile in the analysis sample.
pop_rng13 <- quantile(an$populism_overall, c(.01, .99), names = FALSE)
pop_grid13 <- seq(pop_rng13[1], pop_rng13[2], length.out = 101)
IDEO_AT13 <- c(Left = q13[1], Right = q13[3])
pl13 <- map_dfr(names(DV13), function(dv) {
  m5 <- models[[dv]]$m5; b <- coef(m5); V <- vcov(m5, vcov = VCOVS$clparty)
  S <- obs(m5); P <- d$populism_overall[S]; I <- d$IDEO[S]; y <- d[[dv]][S]
  trm <- c("populism_overall", "IDEO", "populism_overall:IDEO")
  stopifnot(identical(names(b), trm), isTRUE(all.equal(mean(fitted(m5)), mean(y))))
  G <- n_distinct(d$party[S])
  imap_dfr(IDEO_AT13, function(ia, side) {
    X <- cbind(pop_grid13 - mean(P), ia - mean(I), pop_grid13 * ia - mean(P * I))
    tibble(var = dv, dv = DV13[[dv]], ideology = side, ideo_at = ia, populism = pop_grid13,
           pred = mean(y) + drop(X %*% b[trm]), se = sqrt(rowSums((X %*% V[trm, trm]) * X)),
           crit = qt(0.975, df = G - 1), n = nobs(m5), n_clusters = G)
  })
}) |> mutate(lo = pred - crit * se, hi = pred + crit * se)
write_csv_det(pl13, file.path(CSV, "41_fig_predicted_liberalism.csv"))
pD <- ggplot(pl13 |> mutate(dv = factor(dv, levels = DV13), ideology = factor(ideology, levels = c("Left", "Right"))),
             aes(populism, pred, colour = ideology, fill = ideology)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_rug(data = an |> distinct(populism_overall), aes(x = populism_overall), inherit.aes = FALSE, alpha = 0.08, sides = "b") +
  facet_wrap(~ dv, nrow = 1) +
  scale_colour_manual(values = cols13, labels = c(Left = sprintf("Left (ideology %s, p10)", f2(q13[1])),
                                                  Right = sprintf("Right (ideology %s, p90)", f2(q13[3])))) +
  scale_fill_manual(values = cols13, guide = "none") +
  coord_cartesian(xlim = pop_rng13) + th13 +
  labs(x = "Populism (overall)", y = "Predicted liberalism (0-10)", colour = NULL,
       caption = "Model (5) of Tables 4-8 (country and year FE at their sample average, each outcome on its own sample); 95% CI clustered by party.")
save_pdf(pD, file.path(FIG_P, "Predicted_Liberalism_by_Ideology.pdf"), 10, 3.4)

# ---- 14. Figure notes for the main-text figures (PLLM-0256, 2026-11-11) ---------------------------------
# The figures themselves are unchanged. For each main-text figure a small tex file next to the pdf
# holds the Title Case caption (as a comment) and a self-contained note, to be pasted below the
# \includegraphics of the paper's figure environment. Numbers (elections, N, ideology percentiles)
# come from the same objects as the figures.
write_fig_note <- function(pdf, caption, note) {
  write_atomic(c(sprintf("%% note for %s (generated by R/41_Paper_Tables.R, PLLM-0256)", basename(pdf)),
                 sprintf("%% caption (Title Case): \\caption{%s}", caption),
                 sprintf("\\par\\smallskip\\begin{minipage}{\\linewidth}\\scriptsize\\textit{Notes:} %s\\end{minipage}", note)),
               sub("\\.pdf$", "_note.tex", pdf), writer = function(x, p) writeLines(x, p, useBytes = TRUE))
}
el_list <- function(dates) {            # "2013, 2017, April 2019, November 2019, 2024"
  dates <- sort(unique(as.Date(as.character(dates)))); y <- format(dates, "%Y")
  lab <- ifelse(y %in% y[duplicated(y)], paste(month.name[as.integer(format(dates, "%m"))], y), y)
  paste(lab, collapse = ", ")
}
fmt_minus <- function(x, d = 2) sub("^-", "$-$", f2(x, d))
CTRY_ADJ <- c(Austria = "Austrian", Germany = "German", Spain = "Spanish")
for (ctry in names(CTRY_ADJ)) {
  df <- plot_dat |> filter(countryname == ctry, !is.na(Ideology), !is.na(`Populism Overall`))
  if (ctry == "Spain") df <- df |> filter(grepl(spain_parties_regex, `Party Name`))
  sp <- if (ctry == "Spain") sprintf(" Only the %d parties in the legend are shown.", n_distinct(df$`Party Name`)) else ""
  write_fig_note(file.path(FIG, sprintf("%s_Populism_vs_Populist_Ideology.pdf", ctry)),
    sprintf("Populism and Ideology in the %s Party System", CTRY_ADJ[[ctry]]),
    sprintf("Each point is the manifesto of one party in one national election since 2010 (%s: %s); larger points are later elections. Horizontal axis: ideology, $(R-L)/(R+L)$ of the right-wing ($R$) and left-wing ($L$) ideology scores, from $-$1 (left) to +1 (right). Vertical axis: overall populism score (0--10). Scores are means over the three LLMs; labels are the party abbreviations of the Manifesto Project.%s",
            ctry, el_list(df$`Election Date`), sp))
}
n_tr <- sum(tr13$n[tr13$var == "Populism (overall)"])
write_fig_note(file.path(FIG_P, "Trends_by_Ideology.pdf"),
  "Populism and Liberalism in Party Manifestos over Time, by Ideology",
  sprintf("Mean overall populism (left panel) and mean overall liberalism (right panel), 0--10, of the manifestos in each five-year period from 1960 on (the last period is 2020--2025), plotted at the middle of the period, with 95 percent confidence intervals (mean $\\pm$ 1.96 standard errors). Left: manifestos with an ideology score $(R-L)/(R+L) \\le 0$; right: ideology score $> 0$. Lines connect the period means; no smoothing. Sample: %s manifestos with populism, overall liberalism and ideology scores, elections from 1960 on.",
          comma(n_tr)))
me_note <- sprintf("Marginal effect of a one-point increase in populism on each liberalism measure (0--10), from column (5) of Tables 4--8 (populism, ideology and their interaction; country and year fixed effects; each outcome on its own sample, N between %s and %s). %s 95 percent confidence intervals from standard errors clustered by party (CMP party code), critical value of the $t$ distribution with the number of party clusters minus one degrees of freedom.",
                   comma(min(me13$n)), comma(max(me13$n)), "%s")
write_fig_note(file.path(FIG_P, "Marginal_Effects_Dots.pdf"),
  "Marginal Effect of Populism on Liberalism by Ideology",
  sprintf(me_note, sprintf("The effect is evaluated at the 10th percentile (%s, left), the median (%s, centre) and the 90th percentile (%s, right) of the ideology score $(R-L)/(R+L)$ in the analysis sample.",
                           fmt_minus(q13[1]), fmt_minus(q13[2]), fmt_minus(q13[3]))))
write_fig_note(file.path(FIG_P, "Marginal_Effects_Panels.pdf"),
  "Marginal Effect of Populism on Liberalism across the Ideology Scale, by Liberalism Dimension",
  sprintf(me_note, "The line traces the effect over the ideology score $(R-L)/(R+L)$ from $-$1 (left) to +1 (right), the shaded band is its confidence interval, the rug marks the observed ideology scores, the dashed red line is the populism coefficient of column (1) of the same table, and the text gives the $p$-value of the interaction."))

pl_n <- pl13 |> distinct(var, n)
write_fig_note(file.path(FIG_P, "Predicted_Liberalism_by_Ideology.pdf"),
  "Predicted Liberalism of Left-Wing and Right-Wing Manifestos by Populism",
  sprintf("Predicted liberalism (0--10) as a function of the overall populism score for a left-wing manifesto (ideology score $(R-L)/(R+L)$ at its 10th percentile, %s) and a right-wing manifesto (90th percentile, %s), from column (5) of Tables 4--8 (populism, ideology and their interaction; country and year fixed effects held at their sample average; each outcome on its own sample, N between %s and %s). Populism runs from its 1st to its 99th percentile (%s to %s); the rug marks the observed populism scores. Shaded bands: 95 percent confidence intervals from standard errors clustered by party (CMP party code), critical value of the $t$ distribution with the number of party clusters minus one degrees of freedom.",
          fmt_minus(q13[1]), fmt_minus(q13[3]), comma(min(pl_n$n)), comma(max(pl_n$n)), f2(pop_rng13[1]), f2(pop_rng13[2])))

message("R/41 done: tables -> ", TAB, " | figures -> ", FIG, " | csv -> ", CSV)
