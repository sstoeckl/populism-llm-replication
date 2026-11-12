# ============================================================
# 50_Independence_1_CMP.R
#
# TEST 1 — Hand-coded CMP liberalism as dependent variable.
#
# If the negative populism -> liberalism association were a halo /
# shared-call artefact, it should vanish when liberalism is measured by
# an instrument that never saw the populism prompt: the CMP's human
# quasi-sentence codings of the SAME manifestos (zero API cost).
#
# Outputs (output/independence/tables/):
#   t1_convergent_validity.{csv,tex}   LLM liberalism vs CMP indices (r)
#   t1_cmp_dv_regressions.{csv,tex}    standardized slopes, all DV/X combos
#   t1_cmp_dv_modelsummary.tex         publication table (main specs)
#   t1_cmp_index_definitions.{csv,tex} documented category mapping
#   t1_attenuation_benchmark.{csv,tex} tab:indep:cmp; since 2026-11-11 with party-clustered
#                                      bootstrap CIs (500 draws) of the ratios (PLLM-0249)
# ============================================================

source("R/50_Independence_0_Setup.R")
# deterministic summation order and no thread oversubscription in the 500-draw bootstrap of 1d
# (2026-11-11; the chain convention of R/41-R/45)
fixest::setFixest_nthreads(1)
suppressPackageStartupMessages(library(modelsummary))
options(modelsummary_format_numeric_latex = "plain")

base <- load_analysis_base()
message("Analysis base: ", nrow(base), " manifestos; CMP-matched: ",
        sum(is.finite(base$CMP_LIB)))

write_tab(CMP_INDEX_DEF |> rename(`pro-liberal (R)` = pro_liberal, `anti-liberal (L)` = anti_liberal),
          "t1_cmp_index_definitions",
          caption = "CMP-based liberalism indices: category mapping. Index = log(R+0.5) - log(L+0.5), counts of quasi-sentences (Lowe et al. 2011).")

# ------------------------------------------------------------
# 1a. Convergent validity: LLM liberalism vs CMP indices
#     pooled Pearson r and within-country-year r (two-way demeaned)
# ------------------------------------------------------------
cmp_idx <- c("CMP_LIB", "CMP_POL", "CMP_SOC", "CMP_SOC_core", "CMP_ECON", "CMP_ECON_ordo",
             "CMP_ECON_welf", "CMP_rile_neg")

demean_cy <- function(d, v) {
  # residual from country + year FE (fixest::demean)
  as.numeric(fixest::demean(d[[v]], d[, c("countryname", "year")]))
}

conv <- expand_grid(llm = c(LLM_LIB, "populism_overall"), cmp = c(cmp_idx, "CMP_POP")) |>
  pmap_dfr(function(llm, cmp) {
    d <- base |> filter(is.finite(.data[[llm]]), is.finite(.data[[cmp]]))
    tibble(llm = llm, cmp = cmp, n = nrow(d),
           r_pooled = cor(d[[llm]], d[[cmp]]),
           rho_spearman = cor(d[[llm]], d[[cmp]], method = "spearman"),
           r_within_cy = cor(demean_cy(d, llm), demean_cy(d, cmp)))
  })
write_tab(conv, "t1_convergent_validity_long", caption = "Convergent validity of the LLM scores with CMP indices (long format).")
conv_wide <- conv |>
  select(llm, cmp, r_pooled) |>
  pivot_wider(names_from = cmp, values_from = r_pooled)
write_tab(conv_wide, "t1_convergent_validity", digits = 2, label = "tab:indep:cmpvalid",
          caption = "Convergent validity: correlation of the LLM scores with hand-coded CMP indices",
          note = "Pearson correlations at the manifesto level, pooled over all manifestos with both scores. Rows: LLM scores (cross-model means); columns: CMP indices, log(R + 0.5) - log(L + 0.5) of the quasi-sentence counts of the pro- and anti-liberal categories (Lowe et al. 2011); CMP rile reversed so that higher = more liberal.")
print(conv_wide, width = 200)

# ------------------------------------------------------------
# 1b. Headline spec with CMP DVs
#     feols(DV ~ X | countryname + year), cluster = party (paper);
#     country-clustered SEs as robustness. All variables z-scored on the
#     estimation sample => coefficients are SD-per-SD.
#     Common sample = manifestos with every LLM and CMP index finite.
# ------------------------------------------------------------
common <- base |>
  filter(if_all(c("populism_overall", "liberalism_overall", "CMP_LIB", "CMP_ECON",
                  "CMP_POL", "CMP_SOC"), is.finite))
message("Common sample: ", nrow(common))

specs <- bind_rows(
  # (A) LLM populism -> LLM liberalism (the headline; same-call)
  expand_grid(x = "populism_overall", dv = LLM_LIB[1:4], block = "A: LLM pop -> LLM lib (same call)"),
  # (B) LLM populism -> CMP liberalism (independent DV)
  expand_grid(x = "populism_overall", dv = cmp_idx, block = "B: LLM pop -> CMP lib (independent DV)"),
  # (C) CMP populism proxy -> LLM liberalism (independent X)
  expand_grid(x = c("CMP_POP", "CMP_POP_304"), dv = LLM_LIB[1:4], block = "C: CMP pop proxy -> LLM lib (independent X)"),
  # (D) CMP proxy -> CMP liberalism (fully hand-coded)
  expand_grid(x = c("CMP_POP", "CMP_POP_304"), dv = cmp_idx[1:7], block = "D: CMP pop proxy -> CMP lib (fully hand-coded)")
)

run_block <- function(dat, controls = NULL, label) {
  pmap_dfr(specs, function(x, dv, block) {
    m_p <- std_fe_reg(dat, dv, x, controls, cluster = ~party)
    m_c <- std_fe_reg(dat, dv, x, controls, cluster = ~countryname)
    tidy_coef(m_p, x) |>
      mutate(se_country = coeftable(m_c)[x, 2], p_country = coeftable(m_c)[x, 4]) |>
      mutate(block = block, x = x, dv = dv, controls = label, .before = 1)
  })
}

# Political index with CMP-5 sub-categories (direct democracy removed),
# on the subset coded with them; compared with CMP_POL on the same subset
pol_v5 <- map_dfr(c("CMP_POL_v5", "CMP_POL_v5same"), function(dv) {
  m <- std_fe_reg(common, dv, "populism_overall", cluster = ~party)
  tidy_coef(m, "populism_overall") |> mutate(dv = dv, .before = 1)
})
write_tab(pol_v5, "t1_political_v5_subcats", digits = 3,
          caption = "LLM populism -> CMP political liberalism, with/without 202.4 Direct Democracy (CMP-5 coded manifestos).")
print(pol_v5)

res <- bind_rows(
  run_block(common, NULL, "none"),
  run_block(common, "pop_ideology_direction", "+ LLM ideology"),
  # RILE is an independent (hand-coded) ideology control; omitted for CMP_rile_neg DV
  run_block(common |> mutate(rile_c = -CMP_rile_neg), "rile_c", "+ CMP RILE")
) |>
  filter(!(controls == "+ CMP RILE" & dv == "CMP_rile_neg")) |>
  mutate(sig = stars(p))
write_tab(res, "t1_cmp_dv_regressions", label = "tab:indep:cmpregs",
          caption = "LLM populism and liberalism outcomes scored by the LLM and by the CMP hand coding",
          note = "Standardised coefficients; country and year fixed effects; SE clustered by party (country-clustered SE and p in the last columns). Block A: LLM outcome (same call); block B: CMP index as outcome (independent of the LLM).")
print(res |> filter(controls == "none") |> select(block, x, dv, estimate, se, p, n), n = 60)

# ------------------------------------------------------------
# 1c. Publication table: unstandardized-DV versions of the key specs
# ------------------------------------------------------------
zs <- function(d, v) d |> mutate(across(all_of(v), ~ as.numeric(scale(.x))))
cz <- zs(common, c("populism_overall", "liberalism_overall", "lib_political", "lib_social",
                   "lib_economic", "CMP_LIB", "CMP_POL", "CMP_SOC", "CMP_ECON", "CMP_POP"))
mods <- list(
  "LLM Lib"      = feols(liberalism_overall ~ populism_overall | countryname + year, cz, cluster = ~party),
  "CMP Lib"      = feols(CMP_LIB  ~ populism_overall | countryname + year, cz, cluster = ~party),
  "CMP Pol"      = feols(CMP_POL  ~ populism_overall | countryname + year, cz, cluster = ~party),
  "CMP Soc"      = feols(CMP_SOC  ~ populism_overall | countryname + year, cz, cluster = ~party),
  "CMP Econ"     = feols(CMP_ECON ~ populism_overall | countryname + year, cz, cluster = ~party),
  "LLM Lib "     = feols(liberalism_overall ~ CMP_POP | countryname + year, cz, cluster = ~party),
  "CMP Lib "     = feols(CMP_LIB  ~ CMP_POP | countryname + year, cz, cluster = ~party)
)
modelsummary(mods, output = file.path(IND_TAB, "t1_cmp_dv_modelsummary.tex"),
             coef_map = c(populism_overall = "Populism (LLM, z)", CMP_POP = "Populism proxy (CMP 304+110, z)"),
             gof_map = c("nobs", "r2.within"), stars = c("*" = .1, "**" = .05, "***" = .01),
             title = "Populism and liberalism with independently measured variables. All variables z-scored; country and year FE; SE clustered by party.",
             escape = TRUE)

# ------------------------------------------------------------
# 1d. Attenuation benchmark.
#   Model: LLM_lib = T + e1 (+ halo), CMP = lambda*T + e2, e2 independent
#   of populism. Without halo, b_CMP / b_LLM (standardized) = r(LLM, CMP) /
#   reliability(LLM) >= r(LLM, CMP). So the observed ratio can be compared
#   with the within-country-year convergent correlation: ratio >= r is
#   consistent with NO shared-call halo; ratio << r (or a sign flip)
#   indicates that part of the same-call slope is not reproduced by the
#   independent instrument (halo, or construct mismatch between indices).
# ------------------------------------------------------------
pairs_att <- tribble(~llm, ~cmp,
  "liberalism_overall", "CMP_LIB", "lib_political", "CMP_POL", "lib_social", "CMP_SOC",
  "lib_social", "CMP_SOC_core", "lib_economic", "CMP_ECON", "lib_economic", "CMP_ECON_ordo",
  "lib_economic", "CMP_ECON_welf")
att <- pairs_att |>
  left_join(res |> filter(controls == "none", x == "populism_overall") |> select(llm = dv, b_llm = estimate), by = "llm") |>
  left_join(res |> filter(controls == "none", x == "populism_overall") |> select(cmp = dv, b_cmp = estimate, se_cmp = se), by = "cmp") |>
  left_join(conv |> select(llm, cmp, r_within_cy), by = c("llm", "cmp")) |>
  mutate(b_cmp_expected_no_halo = b_llm * r_within_cy,
         ratio_observed = b_cmp / b_llm,
         share_of_expected = b_cmp / b_cmp_expected_no_halo)

# PLLM-0249 (EMP-015, 2026-11-11): party-clustered bootstrap CIs for the CMP / LLM ratio and the
# observed / expected share. Each draw resamples CMP parties with replacement from the analysis base
# (all manifestos of a drawn party enter, as often as the party is drawn) and recomputes every
# ingredient exactly as above: the common sample, the standardised slopes b_llm and b_cmp (z-scored on
# the draw's estimation sample, country + year FE) and the within country-year correlation r on the
# draw's pairwise sample. 500 draws, fixed seed, sequential; 95% percentile intervals. Draws in which
# a ratio cannot be computed are dropped (count in column B).
BOOT_B <- 500L
set.seed(20261111L)
std_b <- function(dat, dv, x) {
  dd <- dat[is.finite(dat[[dv]]) & is.finite(dat[[x]]), c(dv, x, "countryname", "year")]
  dd[[dv]] <- as.numeric(scale(dd[[dv]])); dd[[x]] <- as.numeric(scale(dd[[x]]))
  unname(coef(feols(as.formula(paste(dv, "~", x, "| countryname + year")), data = dd, vcov = "iid", notes = FALSE))[x])
}
party_rows <- split(seq_len(nrow(base)), base$party)
boot_att <- map_dfr(seq_len(BOOT_B), function(b) {
  if (b %% 50L == 0L) message(sprintf("  CMP benchmark bootstrap: draw %d of %d", b, BOOT_B))
  draw <- sample(names(party_rows), length(party_rows), replace = TRUE)
  bb <- base[unlist(party_rows[draw], use.names = FALSE), ]
  cb <- bb |> filter(if_all(c("populism_overall", "liberalism_overall", "CMP_LIB", "CMP_ECON", "CMP_POL", "CMP_SOC"), is.finite))
  tryCatch({
    bl <- map_dbl(set_names(unique(pairs_att$llm)), ~ std_b(cb, .x, "populism_overall"))
    bc <- map_dbl(set_names(unique(pairs_att$cmp)), ~ std_b(cb, .x, "populism_overall"))
    # (computed outside mutate(): a function defined inside the data mask could not see its own
    # arguments in a nested .data[[ ]] lookup)
    rw <- map2_dbl(pairs_att$llm, pairs_att$cmp, function(l, k) {
      dd <- bb[is.finite(bb[[l]]) & is.finite(bb[[k]]), ]
      cor(demean_cy(dd, l), demean_cy(dd, k)) })
    pairs_att |> mutate(draw = b, b_llm = unname(bl[llm]), b_cmp = unname(bc[cmp]), r_within_cy = rw,
      ratio_observed = b_cmp / b_llm, share_of_expected = b_cmp / (b_llm * r_within_cy))
  }, error = function(e) NULL)
})
stopifnot("CMP benchmark bootstrap: no draw succeeded" = nrow(boot_att) > 0)
boot_ci <- boot_att |> filter(is.finite(ratio_observed), is.finite(share_of_expected)) |>
  group_by(llm, cmp) |>
  summarise(ratio_ci_lo = quantile(ratio_observed, .025, names = FALSE), ratio_ci_hi = quantile(ratio_observed, .975, names = FALSE),
            share_ci_lo = quantile(share_of_expected, .025, names = FALSE), share_ci_hi = quantile(share_of_expected, .975, names = FALSE),
            B = n(), .groups = "drop")
att <- att |> left_join(boot_ci, by = c("llm", "cmp")) |>
  relocate(ratio_ci_lo, ratio_ci_hi, .after = ratio_observed)
write_tab(att, "t1_attenuation_benchmark", digits = 3, label = "tab:indep:cmp",
          caption = "CMP benchmark: observed CMP slope against the slope expected without a halo",
          note = paste("Standardised coefficients of the LLM populism score, country and year fixed effects. Slope, LLM: outcome scored by the LLM; slope, CMP: outcome = CMP index. Expected CMP slope with no halo = slope, LLM times the within country-year correlation between the LLM score and the CMP index. Observed / expected: share of the expected slope reached by the CMP slope.",
                       sprintf("95%% CI: percentile intervals over %d bootstrap draws that resample parties (CMP party code) with replacement and recompute the slopes and the correlation in each draw; B: draws used.", BOOT_B)))
print(att)

saveRDS(list(conv = conv, res = res, att = att, n_common = nrow(common)),
        file.path(IND_CACHE, "t1_results.rds"))
message("Test 1 done.")

# ---- Test 3 tables from the archived results (2026-10-27) -------------------------------------------
# Test 3 (separate vs joint LLM calls) needs API calls and is not part of the chain; its numeric
# results are archived in output/independence/tables/ (t3_slopes.csv, t3_slope_bootstrap.csv; manifest
# inputs). Here they are written as paper-ready tables (tab:indep:slopes, tab:indep:boot).
# PLLM-0244 (2026-11-11): tab:indep:slopes also shows the four liberalism dimensions (joint vs separate
# calls, populism and the dimension from the same call type) from the archived per-dimension slopes
# t3_slopes_subdims.csv (written by R/50_Independence_3c_Compare.R in the same run as t3_slopes.csv;
# OLS and country + decade FE on the raw 0-10 scales, HC1 SE; no z-scored version). If that file is
# missing, the table has the overall rows only. NOTE: t3_slopes_subdims.csv should be listed as a
# manifest input in R/70 next to t3_slopes.csv.
T3_DIR <- "output/independence/tables"
if (all(file.exists(file.path(T3_DIR, c("t3_slopes.csv", "t3_slope_bootstrap.csv"))))) {
  SRC_LBL <- c(production = "production run", joint = "joint call", separate = "separate call")
  cellbs <- function(b, se) sprintf("%s (%s)", formatC(b, format = "f", digits = 3), formatC(se, format = "f", digits = 3))
  t3 <- read.csv(file.path(T3_DIR, "t3_slopes.csv"), stringsAsFactors = FALSE) |>
    filter(variant == "baseline rule", sample == "all") |>
    mutate(cell = cellbs(b, se), combo = paste(pop_src, lib_src))
  combos <- c("production production", "joint joint", "separate separate", "joint separate", "separate joint")
  t3w <- t3 |> filter(combo %in% combos) |> select(model, pop_src, lib_src, combo, spec, cell, n) |>
    group_by(model, combo) |> mutate(n_ols = n[spec == "OLS, raw 0-10"], n_fe = n[spec == "country + decade FE, raw 0-10"]) |> ungroup() |>
    select(-n) |> pivot_wider(names_from = spec, values_from = cell) |>
    mutate(model = factor(model, levels = c("gpt41mini", "sonnet")), combo = factor(combo, levels = combos)) |>
    arrange(model, combo) |>
    transmute(model = as.character(model), `Populism from` = SRC_LBL[pop_src], `Liberalism from` = SRC_LBL[lib_src],
              `OLS` = `OLS, raw 0-10`, `N (OLS)` = n_ols, `Country + decade FE` = `country + decade FE, raw 0-10`,
              `FE, z-scored` = `country + decade FE, z-scored`, `N (FE)` = n_fe) |>
    mutate(Outcome = "liberalism_overall", .after = model)
  f_sub <- file.path(T3_DIR, "t3_slopes_subdims.csv")
  if (file.exists(f_sub)) {
    t3d <- read.csv(f_sub, stringsAsFactors = FALSE) |>
      filter(lib_dim != "liberalism_overall", source %in% c("joint", "separate")) |>
      mutate(cell = cellbs(b, se), spec = if_else(spec == "OLS", "ols", "fe")) |>
      select(model, lib_dim, source, spec, cell, n) |>
      pivot_wider(names_from = spec, values_from = c(cell, n)) |>
      mutate(model = factor(model, levels = c("gpt41mini", "sonnet")),
             lib_dim = factor(lib_dim, levels = c("lib_political", "lib_social", "lib_economic", "lib_financial_market")),
             source = factor(source, levels = c("joint", "separate"))) |>
      arrange(model, lib_dim, source) |>
      transmute(model = as.character(model), Outcome = as.character(lib_dim),
                `Populism from` = SRC_LBL[as.character(source)], `Liberalism from` = SRC_LBL[as.character(source)],
                `OLS` = cell_ols, `N (OLS)` = n_ols, `Country + decade FE` = cell_fe, `FE, z-scored` = NA_character_, `N (FE)` = n_fe)
    t3w <- bind_rows(t3w, t3d)
  }
  write_tab(t3w, "t3_slopes", label = "tab:indep:slopes",
            caption = "Separate versus joint LLM calls: slope of liberalism on populism",
            note = paste("Rescored sample of the separate-call test (Appendix B.2): each manifesto scored again with populism and liberalism in one call (joint) and in two separate calls (separate); production run = the scores of the corpus.",
                         "Slope of the liberalism score on populism (overall), 0-10 scales, heteroskedasticity-robust (HC1) SE in parentheses; OLS, and with country and decade fixed effects (raw and, for overall liberalism, z-scored).",
                         if (file.exists(f_sub)) "Rows for the political, social, economic and financial-market dimensions: populism and the dimension both from the joint call or both from the separate calls." else ""))
  bt <- read.csv(file.path(T3_DIR, "t3_slope_bootstrap.csv"), stringsAsFactors = FALSE)
  btw <- bt |> mutate(cell = sprintf("%s [%s, %s]", formatC(estimate, format = "f", digits = 3),
                                     formatC(ci_lo, format = "f", digits = 3), formatC(ci_hi, format = "f", digits = 3))) |>
    select(model, spec, stat, cell, n, B) |> pivot_wider(names_from = stat, values_from = cell) |>
    mutate(model = factor(model, levels = c("gpt41mini", "sonnet")), spec = factor(spec, levels = c("OLS", "country + decade FE"))) |>
    arrange(model, spec) |>
    transmute(model = as.character(model), Specification = as.character(spec), `Joint call` = bj, `Separate calls` = bs,
              `Separate minus joint` = diff, `Separate / joint` = ratio, N = n, `Bootstrap draws` = B)
  write_tab(btw, "t3_slope_bootstrap", label = "tab:indep:boot",
            caption = "Separate versus joint calls: bootstrap comparison of the slopes",
            note = "Slope of liberalism (overall) on populism (overall), 0-10 scales; point estimate and 95% percentile interval over manifesto bootstrap draws (draws with a degenerate fixed-effects fit are dropped). A ratio above 1 means the slope is steeper with separate calls, i.e. the joint call does not inflate the association.")
}
