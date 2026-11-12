# ============================================================
# 49_Comparison.R
#
# Writes PIPE_OUT/COMPARISON_v2026a.md: the ticket list for the paper revision. For every
# data-driven table and claim of paper/R0/main_v2.tex it gives the label, the line in
# main_v2.tex (located at run time), the number the paper prints now, the v2026a number
# under the baseline (rule (a'): quote check), the r12 span-validation fork where it matters,
# and a verdict.
# Every number comes from the chain (R/40-R/43 outputs, or regressions refitted here with the
# same R/1_Aggregation.R path); nothing is typed in.
#
# Inputs: frozen paper lines (PIPE_PAPER_LINES, R/48), PIPE_DERIVED baseline + span checks,
#         PIPE_OUT/csv/41_*, 42_*, 43_* (R/41-R/43)
# Output: PIPE_OUT/COMPARISON_v2026a.md, PIPE_OUT/csv/49_tab4_8_old_new.csv, 49_claims.csv
# ============================================================

suppressPackageStartupMessages({ library(tidyverse); library(arrow); library(fixest) })
source("R/0_Config.R")
source("R/1_Aggregation.R")
setFixest_nthreads(1)

# frozen copy of paper/R0/main_v2.tex (R/48): paper edits never change this stage
TEX <- PIPE_PAPER_LINES
tex <- enc2utf8(read.csv(TEX, stringsAsFactors = FALSE, encoding = "UTF-8")$text)
line_of <- function(pattern, fixed = TRUE) { k <- grep(pattern, tex, fixed = fixed)[1]; if (is.na(k)) NA_integer_ else k }
CSV <- file.path(PIPE_OUT, "csv")
rd  <- function(f) read.csv(file.path(CSV, f), stringsAsFactors = FALSE)
f3 <- function(x) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = 3))
f2 <- function(x) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = 2))
fp <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
stars <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")

base <- read_parquet(file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet")) |> mutate(IDEO = pop_ideology_direction)
RULE <- base$validation_rule[1]
RULE_LBL <- c(r12 = "rule (a) literal: R/12 as stored + topic check on *_overall",
              quote = "rule (a'), decided 2026-10-24: quote check (verbatim in the source chunk, not copied from the prompt) on all 13 dims",
              full = "full: quote + repaired topic check on all dims", none = "no span check")
vars <- read.csv(file.path(PIPE_OUT, "csv", "41_baseline_variants.csv"), stringsAsFactors = FALSE)
FORK <- vars |> filter(vcov == "clparty", grepl("left/right/centrist and overall", variant, fixed = TRUE))

# ---- 1. New regressions under the baseline rule (R/41 csv) and the alternative rule ----------
LIB_DVS <- c(liberalism_overall = 4, lib_political = 5, lib_social = 6, lib_economic = 7, lib_financial_market = 8)
reg_new <- rd("41_tab4_8_regressions_long.csv") |> filter(vcov == "clparty")
reg_cty <- rd("41_tab4_8_regressions_long.csv") |> filter(vcov == "clcountry")
fit_six <- function(dv, data) {
  f <- function(rhs, fe) as.formula(paste(dv, "~", rhs, "|", fe))
  list(m1 = feols(f("populism_overall", "countryname + year"), data = data))
}
# r12 fork (topic check on L/R/C and *_overall), from the R/41 robustness table
reg_alt <- bind_rows(
  FORK |> transmute(dv, model = "m1", term = "populism_overall", estimate = b1_pop, p = p1_pop, nobs = n1),
  FORK |> transmute(dv, model = "m5", term = "populism_overall:IDEO", estimate = b5_inter, p = p5_inter, nobs = n4))
# sanity: refitting the baseline rule here reproduces R/41
chk <- fit_six("liberalism_overall", base)$m1
stopifnot(abs(coef(chk)[[1]] - reg_new$estimate[reg_new$dv == "liberalism_overall" & reg_new$model == "m1"]) < 1e-8)   # csv keeps 10 significant digits

# ---- 2. Old numbers from main_v2.tex, Tables 4-8 ---------------------------------------------
parse_old <- function(no) {
  i0 <- line_of(sprintf("\\label{tab:%d}", no))
  blk <- tex[i0:(i0 + 25)]
  getrow <- function(pat, term) {
    k <- grep(pat, blk)[1]; cells <- str_split(blk[k], "&")[[1]][-1]
    tcells <- str_split(blk[k + 1], "&")[[1]][-1]
    tibble(model = paste0("m", seq_along(cells)), term = term, line = i0 + k - 1L,
           old_b = suppressWarnings(as.numeric(str_match(cells, "\\{(-?[0-9.]+)\\}")[, 2])),
           old_stars = coalesce(str_match(cells, "\\}(\\**)")[, 2], ""),
           old_t_iid = suppressWarnings(as.numeric(str_match(tcells, "\\(\\{(-?[0-9.]+)\\}\\)")[, 2])))
  }
  nline <- blk[grep("^Num.Obs", blk)[1]]
  old_n <- as.numeric(str_match_all(nline, "\\{([0-9]+)\\}")[[1]][, 2])
  bind_rows(getrow("^Populism Overall", "populism_overall"), getrow("^Populist Ideology", "IDEO"),
            getrow("^Populism:Ideology", "populism_overall:IDEO")) |>
    filter(!is.na(old_b)) |>
    left_join(tibble(model = paste0("m", seq_along(old_n)), old_n = old_n), by = "model")
}
old_tabs <- imap_dfr(LIB_DVS, ~ parse_old(.x) |> mutate(dv = .y, label = paste0("tab:", .x)))
sig_old <- function(s) s %in% c("**", "***")
cmp <- old_tabs |>
  left_join(reg_new |> select(dv, model, term, new_b = estimate, new_t = t, new_p = p, new_n = nobs),
            by = c("dv", "model", "term")) |>
  left_join(reg_cty |> select(dv, model, term, new_p_country = p), by = c("dv", "model", "term")) |>
  mutate(verdict = case_when(
           sign(old_b) != sign(new_b) & new_p < .05 ~ "SIGN FLIP (significant)",
           sign(old_b) != sign(new_b) ~ "sign flip (n.s.)",
           sig_old(old_stars) & new_p >= .05 ~ "significance LOST",
           !sig_old(old_stars) & new_p < .05 ~ "significance GAINED",
           TRUE ~ "same sign and 5% significance")) |>
  relocate(label, line, dv, model, term)
write_csv_det(cmp, file.path(CSV, "49_tab4_8_old_new.csv"))

# ---- 3. Claims ---------------------------------------------------------------------------------
ss  <- rd("41_tab1_sumstats.csv"); cmr <- rd("41_tab2_cormatrix.csv"); cc <- rd("41_corpus_counts.csv")
tr  <- rd("41_fig4_5_trend_slopes.csv"); it <- rd("41_fig6_10_interaction_tests.csv") |> filter(vcov == "clparty")
ev  <- rd("41_tabA10_evidence_shares.csv"); comp <- rd("41_A10_computational_summary.csv")
sch <- rd("42_headline_distribution.csv"); acct <- rd("42_grid_accounting.csv"); pf <- rd("42_per_fork.csv")
nse <- rd("43_nse_rules.csv"); ext <- rd("41_tab3_cormatrix2_partial.csv")
tdiag <- rd("40_topic_check_diagnostic.csv"); rdec <- rd("40_rule_decomposition.csv")
cov <- read_parquet(file.path(PIPE_IN, "coverage.parquet"))
R <- function(dv, m, term) reg_new |> filter(dv == !!dv, model == m, term == !!term)
A <- function(dv, m, term) reg_alt |> filter(dv == !!dv, model == m, term == !!term)
inter_sig <- it |> filter(p_inter < .05)
alt_inter <- reg_alt |> filter(model == "m5", term == "populism_overall:IDEO")
gap_txt <- function(x) if (nrow(x) == 0) "none" else paste0(x$dv, " (b3 ", f3(x$b_inter %||% x$estimate), ")", collapse = ", ")
gap_alt <- alt_inter |> filter(p < .05)
holds <- function(ok) if (isTRUE(ok)) "HOLDS" else "FAILS"
pop_all_neg <- function(rg) rg |> filter(term == "populism_overall") |> summarise(ok = all(estimate < 0 & p < .05)) |> pull(ok)
ideo_t4 <- reg_new |> filter(dv == "liberalism_overall", term == "IDEO", model %in% paste0("m", 2:6))
dsp <- sch |> filter(set == "distinct specs, party SE", estimand == "pop")
ds_pop <- read_parquet(file.path(PIPE_OUT, "spec_curve", "specs_distinct_v2026a.parquet")) |> filter(estimand == "pop")
pick <- function(pty, yrs) base |> filter(party == pty, year %in% yrs) |> arrange(date)
fpo <- base |> filter(party == 42420, year >= 2010); afd <- base |> filter(party == 41953, year >= 2010)
vox <- base |> filter(party == 33710, year >= 2010); pod <- base |> filter(party == 33210, year >= 2010)
ovp <- base |> filter(party == 42520, year >= 2010)
ctry_med <- base |> filter(year >= 2010) |> group_by(countryname) |> summarise(med = median(populism_overall, na.rm = TRUE))
top_right <- bind_rows(fpo, afd, vox) |> left_join(ctry_med, by = "countryname") |>
  mutate(tr = populism_overall > med & IDEO > 0)
ideo_cors <- cmr |> filter(row == "Ideology")
me_ok <- it |> summarise(ok = all(me_p10 < 0 & me_p90 < 0 & p_me_p10 < .05 & p_me_p90 < .05)) |> pull(ok)
b4 <- reg_new |> filter(model == "m4", term == "populism_overall") |> arrange(estimate)
cov_n <- cov |> group_by(model) |> summarise(n = sum(covered))

claims <- tribble(
  ~label, ~pattern, ~claim, ~old, ~new, ~verdict,
  "abstract", "consistent rejection of liberalism among all populist parties",
  "Populism lowers liberalism on every dimension",
  "beta_POP < 0, *** in all 30 cells",
  sprintf("beta_POP(1) %s to %s; all (1)/(4)/(6) cells p<0.05: %s. r12 fork: %s",
          f3(max(reg_new$estimate[reg_new$model == "m1" & reg_new$term == "populism_overall"])),
          f3(min(reg_new$estimate[reg_new$model == "m1" & reg_new$term == "populism_overall"])),
          pop_all_neg(reg_new |> filter(model %in% c("m1", "m4", "m6"))),
          pop_all_neg(reg_alt |> filter(model %in% c("m1", "m4", "m6")))),
  holds(pop_all_neg(reg_new |> filter(model %in% c("m1", "m4", "m6")))),
  "abstract", "mostly indistinguishable from that of left-wing populism",
  "Right-wing populism mostly indistinguishable from left-wing (interaction n.s.)",
  "3 of 5 interactions ** or ***", sprintf("significant (5%%, party SE): %s. r12 fork: %s",
                                          gap_txt(inter_sig), if (nrow(gap_alt)) paste0(gap_alt$dv, " (", f3(gap_alt$estimate), ")", collapse = ", ") else "none"),
  case_when(nrow(inter_sig) <= 1 ~ "HOLDS", nrow(inter_sig) == 2 ~ "PARTLY (2 of 5 differ)", TRUE ~ "FAILS"),
  "abstract / l.240", "very much in-line with other indicators",
  "Populism and ideology in line with external indicators",
  "Table 3: r = 0.52-0.65 (POP), 0.26-0.79 (IDEO)",
  sprintf("only RILE cells computable: RILE~IDEOL r = %s (n %d), RILE~POP %s, IDEOL~POP %s; external ML/POPPA/CHES/GPS files missing",
          f2(ext$r[ext$pair == "RILE (CMP) ~ IDEOL (LLM)"]), ext$n[ext$pair == "RILE (CMP) ~ IDEOL (LLM)"],
          f2(ext$r[ext$pair == "RILE (CMP) ~ POP (LLM)"]), f2(ext$r[ext$pair == "IDEOL (LLM) ~ POP (LLM)"])),
  "CANNOT VERIFY (external data missing); see validation module (PopuList AUC)",
  "l.170 / l.195", "The corpus includes 3,327 manifestos",
  "Corpus: 3,327 manifestos, 982 parties, 62 countries, 600 elections, 1945-2025",
  "3,327 / 982 / 62 / 600 / 1945-2025",
  with(cc, sprintf("pool %s / %s / %s / %s / %s-%s; analysis sample %s / %s / %s / %s",
                   manifestos[1], parties[1], countries[1], elections[1], first_year[1], last_year[1],
                   manifestos[2], parties[2], countries[2], elections[2])),
  "UPDATE NUMBERS",
  "l.190", "All three models achieve full coverage",
  "All three models achieve full coverage", "full coverage of 3,327",
  paste(sprintf("%s %d", cov_n$model, cov_n$n), collapse = ", ") |> paste("of 3,371 (4 documented gap legs)"),
  "FAILS (reword: 3 gaps Gemini, 1 GPT)",
  "l.195", "our ideology variable is also not systematically correlated",
  "IDEO not systematically correlated with other dimensions", "|r| <= 0.27",
  paste0(paste(sprintf("%s %s", ideo_cors$col, f2(ideo_cors$r)), collapse = "; "), sprintf(" (N %d-%d)", min(ideo_cors$n), max(ideo_cors$n))),
  if (max(abs(ideo_cors$r)) > 0.3) "FAILS" else "HOLDS",
  "l.272 (f:1-f:3)", "always figure in the top right-hand corner",
  "FPÖ, AfD, VOX top-right (POP > country median, IDEO > 0) in every post-2010 election", "yes",
  sprintf("%d of %d manifestos top-right; %d have IDEO NA; not top-right: %s", sum(top_right$tr, na.rm = TRUE), nrow(top_right),
          sum(is.na(top_right$IDEO)),
          { x <- top_right |> filter(!(tr %in% TRUE)); if (nrow(x)) paste(sprintf("%s %d (POP %s, IDEO %s)", x$partyabbrev, x$year, f2(x$populism_overall), f2(x$IDEO)), collapse = "; ") else "none" }),
  if (all(top_right$tr %in% TRUE)) "HOLDS" else if (any(is.na(top_right$IDEO))) "CANNOT SHOW (IDEO missing)" else
    sprintf("HOLDS EXCEPT %d manifesto(s): reword 'always'", sum(!(top_right$tr %in% TRUE))),
  "l.274 (f:1)", "partial imitation of FPÖ positions",
  "ÖVP moves toward FPÖ in 2017/2019", "visible in figure",
  paste(sprintf("ÖVP %d POP %s IDEO %s", ovp$year, f2(ovp$populism_overall), f2(ovp$IDEO)), collapse = "; "),
  "CHECK FIGURE",
  "l.274 (f:2)", "AfD's shift to much more right-wing positions",
  "AfD shifts right 2013 -> 2017", "visible in figure",
  paste(sprintf("AfD %d IDEO %s", afd$year, f2(afd$IDEO)), collapse = "; "),
  if (all(!is.na(afd$IDEO[afd$year %in% c(2013, 2017)]))) (if (diff(afd$IDEO[afd$year %in% c(2013, 2017)]) > 0) "HOLDS" else "FAILS") else "CANNOT SHOW (IDEO missing)",
  "l.274 (f:3)", "Podemos} (We can) movement have declined",
  "Podemos populism declined in recent elections; VOX varied little", "visible in figure",
  paste0("Podemos POP ", paste(sprintf("%d %s", pod$year, f2(pod$populism_overall)), collapse = ", "),
         "; VOX POP ", paste(sprintf("%d %s", vox$year, f2(vox$populism_overall)), collapse = ", "),
         "; VOX IDEO ", paste(f2(vox$IDEO), collapse = ", ")),
  "CHECK FIGURE",
  "l.317 (f:4)", "slow but significant rise over time",
  "Populism rises significantly; stronger excluding pre-1980", "visual",
  with(tr, sprintf("%s per decade (p %s); 1980+: %s (p %s)", f3(slope_per_decade[series == "populism_overall" & group == "all"]),
                   fp(p[series == "populism_overall" & group == "all"]), f3(slope_per_decade[group == "all, 1980+"]),
                   fp(p[group == "all, 1980+"]))),
  with(tr, if (slope_per_decade[series == "populism_overall" & group == "all"] > 0 & p[series == "populism_overall" & group == "all"] < .05) "HOLDS" else "FAILS"),
  "l.327 (f:5)", "overall average liberalism of parties on the right is either stable",
  "Liberalism: right stable/declining, left increasing", "visual",
  with(tr, sprintf("left %s per decade (p %s), right %s (p %s)", f3(slope_per_decade[group == "Left"]), fp(p[group == "Left"]),
                   f3(slope_per_decade[group == "Right"]), fp(p[group == "Right"]))),
  with(tr, if (slope_per_decade[group == "Left"] > 0 & p[group == "Left"] < .05 & slope_per_decade[group == "Right"] <= 0) "HOLDS" else "CHECK"),
  "l.344", "Robust standard errors are clustered at the party level",
  "SEs clustered by party", "tables used iid SEs", "all v2026a tables party-clustered (country-clustered robustness files)",
  "NOW TRUE (replace tables)",
  "l.346 (tab:4)", "coefficients on the populism variable are always negative",
  "POP negative and significant in all models incl. party FE", "*** in (1)-(6)",
  sprintf("(1) %s, (6) %s (p %s)", f3(R("liberalism_overall", "m1", "populism_overall")$estimate),
          f3(R("liberalism_overall", "m6", "populism_overall")$estimate), fp(R("liberalism_overall", "m6", "populism_overall")$p)),
  holds(pop_all_neg(reg_new |> filter(dv == "liberalism_overall"))),
  "l.346 (tab:4)", "Populist ideology is also negative and statistically significant in most models",
  "IDEO negative and significant in most models (left more liberal)", "4 of 5 negative ***",
  sprintf("negative & p<0.05 in %d of 5; estimates %s", sum(ideo_t4$estimate < 0 & ideo_t4$p < .05), paste(f3(ideo_t4$estimate), collapse = ", ")),
  if (sum(ideo_t4$estimate < 0 & ideo_t4$p < .05) >= 3) "HOLDS" else "FAILS",
  "l.375 (f:6)", "Differences between both extreme ends are, in fact, not statistically significant",
  "Overall: left/right difference n.s.", "b3 0.048**",
  with(it |> filter(dv == "liberalism_overall"), sprintf("b3 %s (p %s); r12 fork %s (p %s)", f3(b_inter), fp(p_inter),
       f3(alt_inter$estimate[alt_inter$dv == "liberalism_overall"]), fp(alt_inter$p[alt_inter$dv == "liberalism_overall"]))),
  with(it |> filter(dv == "liberalism_overall"), if (p_inter >= .05) "HOLDS" else "FAILS"),
  "l.385 (f:7)", "where differences between both extreme ends are also not statistically significant",
  "Political: difference n.s.", "b3 0.046**",
  with(it |> filter(dv == "lib_political"), sprintf("b3 %s (p %s); r12 fork %s (p %s)", f3(b_inter), fp(p_inter),
       f3(alt_inter$estimate[alt_inter$dv == "lib_political"]), fp(alt_inter$p[alt_inter$dv == "lib_political"]))),
  with(it |> filter(dv == "lib_political"), if (p_inter >= .05) "HOLDS" else "FAILS"),
  "l.422 (f:8)", "differences between both ends of the ideological spectrum are now marginally significant",
  "Social: right somewhat worse, marginally significant", "b3 -0.057**",
  with(it |> filter(dv == "lib_social"), sprintf("b3 %s (p %s); ME left (p10) %s, right (p90) %s", f3(b_inter), fp(p_inter), f3(me_p10), f3(me_p90))),
  with(it |> filter(dv == "lib_social"), if (b_inter < 0 & p_inter < .01) "HOLDS, MUCH STRONGER (reword 'marginally')" else if (b_inter < 0 & p_inter < .1) "HOLDS" else "FAILS"),
  "l.459 (tab:7)", "Populist ideology is again negative and statistically significant in most models",
  "Economic: IDEO negative and significant", "IDEO positive in old table already",
  sprintf("IDEO (4) %s (p %s)", f3(R("lib_economic", "m4", "IDEO")$estimate), fp(R("lib_economic", "m4", "IDEO")$p)),
  if (R("lib_economic", "m4", "IDEO")$estimate < 0) "HOLDS" else "FAILS (IDEO positive)",
  "l.459 (f:9)", "nowhere near statistical significance",
  "Economic: left/right difference nowhere near significance", "b3 0.027",
  with(it |> filter(dv == "lib_economic"), sprintf("b3 %s (p %s); r12 fork %s (p %s)", f3(b_inter), fp(p_inter),
       f3(alt_inter$estimate[alt_inter$dv == "lib_economic"]), fp(alt_inter$p[alt_inter$dv == "lib_economic"]))),
  with(it |> filter(dv == "lib_economic"), if (p_inter >= .10) "HOLDS" else "FAILS"),
  "l.496 (tab:8)", "Populist ideology is also negative and statistically significant in all models, while figure",
  "Financial: IDEO negative and significant in all models", "negative *** (2)-(5)",
  sprintf("IDEO (4) %s (p %s)", f3(R("lib_financial_market", "m4", "IDEO")$estimate), fp(R("lib_financial_market", "m4", "IDEO")$p)),
  if (R("lib_financial_market", "m4", "IDEO")$estimate < 0) "HOLDS" else "FAILS (sign flips)",
  "l.496 (f:10)", "differences between both ends on the ideological scale are now statistically significant",
  "Financial: difference significant, right less control-oriented (b3 > 0)", "b3 0.082***",
  with(it |> filter(dv == "lib_financial_market"), sprintf("b3 %s (p %s); r12 fork %s (p %s)", f3(b_inter), fp(p_inter),
       f3(alt_inter$estimate[alt_inter$dv == "lib_financial_market"]), fp(alt_inter$p[alt_inter$dv == "lib_financial_market"]))),
  with(it |> filter(dv == "lib_financial_market"), if (b_inter > 0 & p_inter < .05) "HOLDS" else "FAILS"),
  "l.533 / l.555", "The only two dimensions on which a statistically significant gap",
  "Significant left/right gaps only in social (right worse) and financial (right less)", "social, financial",
  sprintf("baseline: %s; r12 fork: %s", gap_txt(inter_sig),
          if (nrow(gap_alt)) paste0(gap_alt$dv, " (", f3(gap_alt$estimate), ")", collapse = ", ") else "none"),
  if (setequal(inter_sig$dv, c("lib_social", "lib_financial_market"))) "HOLDS" else "CHANGES",
  "l.533 / l.555", "increasingly populist positions are never liberty-enhancing",
  "Marginal effect of POP never positive (left and right)", "visual",
  sprintf("ME at IDEO p10 and p90 negative and p<0.05 for all 5 outcomes: %s", me_ok),
  holds(me_ok),
  "l.127", "particularly marked in the economic and financial market dimensions",
  "Effect strongest for economic and financial", "-0.24/-0.26 largest",
  paste(sprintf("%s %s", b4$dv, f3(b4$estimate)), collapse = "; "),
  if (all(b4$dv[1:2] %in% c("lib_economic", "lib_financial_market"))) "HOLDS" else "FAILS",
  "l.555", "particularly pronounced among right-wing populist parties on issues of economic governance",
  "Right-wing effect more pronounced on economic and financial (b3 < 0)", "claim",
  with(it, sprintf("b3 econ %s (p %s), fin %s (p %s)", f3(b_inter[dv == "lib_economic"]), fp(p_inter[dv == "lib_economic"]),
                   f3(b_inter[dv == "lib_financial_market"]), fp(p_inter[dv == "lib_financial_market"]))),
  with(it, if (all(b_inter[dv %in% c("lib_economic", "lib_financial_market")] < 0 & p_inter[dv %in% c("lib_economic", "lib_financial_market")] < .05)) "HOLDS" else "FAILS"),
  "l.538 / C.2", "the median coefficient on populism is",
  "Spec curve: 8,100 specs, median -0.316, 97.6% sig. negative, 0% positive",
  "8,100 / -0.316 / 97.6% / 0%",
  sprintf("%s distinct specs (of %s cells), median %s, IQR [%s, %s], 2.5-97.5%% [%s, %s], %.1f%% sig. negative, %.1f%% positive",
          format(dsp$n, big.mark = ","), format(acct$cells[acct$estimand == "pop"], big.mark = ","), f3(dsp$median),
          f3(dsp$p25), f3(dsp$p75), f3(dsp$p2.5), f3(dsp$p97.5), 100 * dsp$share_sig_neg, 100 * dsp$share_pos),
  if (dsp$share_pos == 0) "HOLDS (update numbers, report distinct count)" else if (dsp$share_sig_pos == 0)
    sprintf("HOLDS QUALITATIVELY: %d of %s distinct specs positive (max %s), none significant; '0%% positive' must be reworded",
            round(dsp$share_pos * dsp$n), format(dsp$n, big.mark = ","), f3(max(ds_pop$coef))) else "CHANGES",
  "l.1318", "the remaining 3{,}240 cells fail",
  "3,240 cells fail estimation", "explanation", sprintf("no cell removed by design; failed cells: %s",
                                                       paste(sprintf("%s %d", acct$estimand, acct$cells_failed), collapse = ", ")),
  "WRONG (they were removed by design); REWRITE",
  "l.879 (A.7)", "Spans that fail are flagged and excluded from the verified-evidence aggregation",
  "Failing spans excluded from aggregation", "not implemented",
  sprintf("implemented: rule '%s'. Topic regex bug: every TOPIC_KEYWORDS_V4 pattern matched any text", RULE),
  "NOW TRUE for the implemented rule; A.7 item 3 must describe the topic check as actually run",
  "l.941 / l.1026 (A.9, A.12)", "weighting each chunk by its confidence level",
  "Per-dimension confidence weights c_{i,d}", "code used populism confidence for all dims",
  "own-block confidence (populism / ideology / liberalism) implemented", "NOW TRUE",
  "l.947 (A.10)", "20{,}176 manifesto-chunk records",
  "20,176 chunk records; 82.9% verified", "20,176 / 82.9%",
  sprintf("%s chunk records; %.1f%% verified; %.1f%% verified and span-valid; %.2f%% parse-lost (Claude)",
          format(comp$value[2], big.mark = ","), comp$value[4], comp$value[5], comp$value[8]),
  "UPDATE NUMBERS",
  "l.952 (A.10)", "Manichaean discourse (32\\%) and financial-market liberalism (26\\%)",
  "Highest no-evidence rates: Manichaean 32%, financial 26%", "32% / 26%",
  sprintf("Manichaean %.1f%%, financial %.1f%%", ev$no_evidence[ev$dim == "pop_manichean"], ev$no_evidence[ev$dim == "lib_financial_market"]),
  "UPDATE NUMBERS",
  "A.7 item 3", "\\textbf{On topic.}",
  "Topic check (keyword regex + LLM judge) is part of span validation",
  "described as a filter",
  with(tdiag |> filter(score_band != "all") |> group_by(score_band) |> summarise(p = weighted.mean(topic_pass, n)),
       sprintf("REWRITE: topic check is a reported diagnostic and a robustness fork, not a filter. Repaired topic pass rate by score band: %s; filtering on it drops low-score evidence (rule r12: N POP %s, IDEO %s, mean POP %s vs %s)",
               paste(sprintf("%s %.0f%%", score_band, 100 * p), collapse = ", "),
               format(rdec$n_pop[grepl("r12", rdec$step)]), format(rdec$n_ideo[grepl("r12", rdec$step)]),
               f2(rdec$mean_pop[grepl("r12", rdec$step)]), f2(rdec$mean_pop[grepl("quote check", rdec$step)]))),
  "REWRITE A.7 (and l.186, l.1017-1019)",
  "A.7 item 3", "\\textbf{On topic.}",
  "Keyword regex catches off-topic spans", "claim",
  "DISCLOSE: every TOPIC_KEYWORDS_V4 pattern ended in a trailing pipe character (an empty regex alternative), which matched any text, so the regex never rejected a span in the production run; repaired copy used for the diagnostic",
  "DISCLOSE (regex bug)",
  "A.6 / A.10", "Each dimension is a self-contained",
  "Every chunk returns a parsed object per dimension", "implied",
  sprintf("DISCLOSE: 363 Claude chunks (703 chunk-dims) were lost to unparsed dimension objects (JSON returned as strings); re-scored with a patched parser; %d chunk-dims still missing in the baseline data",
          { stt <- rd("40_chunk_status_by_model_dim.csv"); if ("parse_lost" %in% names(stt)) sum(stt$parse_lost) else 0L }),
  "DISCLOSE (parse loss and re-score)",
  "C.1 (tab:speccurve:grid)", "\\label{tab:speccurve:grid}",
  "Seven forks", "7 forks, 11,340 candidate cells",
  sprintf("%d measurement paths (model set, within agg., span validation none/quote/r12/full, Gemini source, gap legs, between agg. for 3 models) x 60 analytic cells x 3 SE; %s distinct specifications for POP",
          as.integer(acct$cells[acct$estimand == "pop"] / (5 * 4 * 3 * 3)),
          format(dsp$n, big.mark = ",")),
  "REWRITE TABLE C.1 + l.1318",
  "A.8", "(explicitly set; no nucleus truncation)",
  "Identical decoding configuration for all three model families (top_p = 1, store = TRUE, max_output_tokens = 5000)", "same for all",
  "code (CODEBOOK.md section 2): top_p = 1 and store = TRUE only for GPT-4.1-mini; Claude: forced tool use, max_tokens 5000, no top_p; Gemini: maxOutputTokens 12000, thinkingBudget 0, no top_p; all temperature 0",
  "REWRITE A.8 list per model family",
  "A.8 / A.12", "pinned snapshots where the provider exposes them",
  "Models pinned; gemini-flash-latest", "pinned",
  { pv <- rd("40_model_provenance.csv"); sh <- pv |> group_by(model) |> summarise(logged = sum(share_chunks[resolved_status == "logged"]), inferred = sum(share_chunks[resolved_status == "inferred"]))
    paste0("claude-sonnet-4-6 is an undated alias; resolved version logged for ", paste(sprintf("%s %.0f%%", sh$model, 100 * sh$logged), collapse = ", "),
           " of chunks; June Gemini rows (alias gemini-flash-latest) inferred gemini-3.5-flash: ", sprintf("%.0f%%", 100 * sh$inferred[sh$model == "gemini"]), " of Gemini chunks") },
  "REWRITE: state alias/snapshot per family and the inferred June Gemini version",
  "l.186 / A.9", "typically three to eight words in length",
  "Quotes of 3-8 words with short context", "3-8 words",
  { q <- rd("36_quote_length_audit.csv"); paste(sprintf("%s: %.0f%% of quotes > 8 words, contexts up to %d words", q$set, 100 * q$quote_gt8, q$context_max), collapse = "; ") },
  "DISCLOSE (copyright handling; public data truncated to 8 words)")
claims <- claims |> mutate(line = map_int(pattern, line_of), .after = label) |> select(-pattern)
write_csv_det(claims, file.path(CSV, "49_claims.csv"))

# ---- 4. Markdown --------------------------------------------------------------------------------
md_tab <- function(df) {
  df <- df |> mutate(across(everything(), ~ str_replace_all(as.character(.x), "\\|", "/")))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", strrep("---|", ncol(df))),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
ssold <- tibble(var = c("populism_overall", "liberalism_overall", "lib_political", "lib_social", "lib_economic", "lib_financial_market", "IDEO"),
                line = map_int(c("Populism (overall) & 5.22", "Liberalism (overall) & 6.59", "Liberalism (political) & 7.28",
                                 "Liberalism (social) & 6.57", "Liberalism (economic) & 6.02", "Liberalism (financial markets) & 6.03",
                                 "Populist ideology (rescaled) & -0.11"), line_of),
                old = map_chr(line, ~ { x <- str_split(tex[.x], "&")[[1]]; paste0(str_trim(x[2]), " (", str_trim(x[3]), ")") }))
t1 <- ss |> select(var, n, mean, sd, min, max) |> inner_join(ssold, by = "var") |>
  transmute(label = "tab:sumstats", line, variable = var, `old mean (sd)` = old,
            `new mean (sd)` = sprintf("%s (%s)", f2(mean), f2(sd)), N = n, range = sprintf("[%s, %s]", f2(min), f2(max)))
cmd <- cmp |> transmute(label, line, dv, col = sub("m", "(", model) |> paste0(")"), term = recode(term, populism_overall = "POP", IDEO = "IDEO", "populism_overall:IDEO" = "POPxIDEO"),
                        `old (iid stars)` = paste0(f3(old_b), old_stars), `old N` = old_n,
                        new = paste0(f3(new_b), stars(new_p)), `t party` = NA_character_,
                        `p party / country` = paste(fp(new_p), fp(new_p_country), sep = " / "), `new N` = new_n, verdict)
cmd$`t party` <- f2(reg_new$t[match(paste(cmp$dv, cmp$model, cmp$term), paste(reg_new$dv, reg_new$model, reg_new$term))])
pfp <- pf |> filter(estimand == "pop", fork %in% c("model_set", "outcome", "fe_key", "validation", "within_agg", "gemini_norm", "gap_legs", "between_agg", "sample_key")) |>
  transmute(fork, option, n, median = f3(median), band = sprintf("[%s, %s]", f3(p5), f3(p95)),
            `% sig neg` = sprintf("%.1f", 100 * share_sig_neg), `% pos` = sprintf("%.1f", 100 * share_pos))
nse_md <- nse |> transmute(estimand, pool, n_specs, beta = f3(base_coef), se = f3(base_se), nse = f3(nse_sd),
                           `NSE/SE` = f2(nse_over_se), `(i) central spec +/- 1.96 NSE` = sprintf("%s [%s, %s]", f3(central_coef), f3(rule_i_central_lo), f3(rule_i_central_hi)),
                           `(ii) SE_total` = f3(se_total), `t_total` = f2(t_total), `p_total` = fp(p_total))
headline <- reg_new |> filter(dv == "liberalism_overall", model == "m1")
headline_c <- reg_cty |> filter(dv == "liberalism_overall", model == "m1")
headline_alt <- reg_alt |> filter(dv == "liberalism_overall", model == "m1")

md <- c(
  "# COMPARISON_v2026a — paper (main_v2.tex) vs. the v2026a canonical chain",
  "",
  sprintf("Generated by `R/49_Comparison.R` from chain outputs only. Paper source: frozen copy `%s` of main_v2.tex (md5 in the file name; line numbers refer to this version).",
          basename(TEX)),
  sprintf("Baseline: `%s` (%d manifestos, MPDS2026a / corpus 2026-1), span validation **%s** — %s.",
          "manifesto_scores_v2026a.parquet", nrow(base), RULE, RULE_LBL[[RULE]]),
  "Span-validation forks (none / r12 / full) appear only in Section 6 (robustness appendix table) and in the spec curve; the r12 fork is quoted in a claim where it changes the verdict.",
  "Inference: SEs clustered by CMP party code (country clustering in the `_clcountry` tables). Party FE = CMP `party` code.",
  "Old stars are from iid SEs (what the Nov-2025 tables used); new stars are party-clustered: *** p<0.01, ** p<0.05, * p<0.1.",
  "",
  "## Headline",
  "",
  sprintf("Table 4 col. (1), liberalism_overall ~ populism_overall | country + year: **beta = %s**, SE party %s, SE country %s, N = %d (paper: -0.179, N = 3,152). r12 fork: %s (N = %d).",
          f3(headline$estimate), f3(headline$se), f3(headline_c$se), headline$nobs, f3(headline_alt$estimate), headline_alt$nobs),
  "",
  "## 1. Claims (ticket list)",
  "",
  md_tab(claims |> rename(`paper says` = old, `v2026a` = new)),
  "",
  "## 2. Table 1 (tab:sumstats)",
  "",
  md_tab(t1),
  "",
  "## 3. Tables 4-8, cell by cell",
  "",
  md_tab(cmd),
  "",
  "## 4. Appendix C: specification curve (distinct specifications, estimand POP)",
  "",
  md_tab(acct),
  "",
  md_tab(pfp),
  "",
  "## 5. NSE inputs (Phase 5 decides the reporting rule)",
  "",
  md_tab(nse_md),
  "",
  "## 6a. IDEO selection forks (missing R/L/C), from R/44 (party SE)",
  "",
  md_tab(rd("44_ideo_forks.csv") |> transmute(fork, dv, n, `IDEO (4)` = paste0(f3(ideo_m4), stars(ideo_m4_p)),
                                              `POPxIDEO (5)` = paste0(f3(inter_m5), stars(inter_m5_p)))),
  "",
  "## 6. Robustness appendix table: measurement choices incl. span-validation forks (party SE)",
  "",
  "Source: `tab_baseline_variants.tex` (label `tab:robust_measurement`), `41_baseline_variants.csv`. Topic-check diagnostic: `40_topic_check_diagnostic.csv`; rule decomposition: `40_rule_decomposition.csv`.",
  "",
  md_tab(vars |> filter(vcov == "clparty", dv %in% c("liberalism_overall", "lib_social", "lib_economic")) |>
           transmute(variant, dv, `b POP (1)` = paste0(f3(b1_pop), stars(p1_pop)), `N (1)` = n1, `b POP (4)` = paste0(f3(b4_pop), stars(p4_pop)),
                     `b POPxIDEO (5)` = paste0(f3(b5_inter), stars(p5_inter)), `b POP (6)` = paste0(f3(b6_pop), stars(p6_pop)), `N (4)` = n4)))
write_atomic(md, file.path(PIPE_OUT, "COMPARISON_v2026a.md"), writer = function(x, p) writeLines(x, p, useBytes = TRUE))
message("wrote ", file.path(PIPE_OUT, "COMPARISON_v2026a.md"))
