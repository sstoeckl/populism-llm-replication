# ============================================================
# 46_Numbers.R   (stage `numbers`; restricted: reads outputs of ideology, a13 and the B5 block of oa)
#
# Every in-text number of the paper as a LaTeX macro, read ONLY from chain outputs (no data, no
# model fits): PIPE_OUT/csv/*, PIPE_OUT/validation/a13/csv/*, PIPE_OUT/validation/independence/
# tables/*. Written 2026-10-27 (PLLM-0065).
#
#   PIPE_OUT/numbers.tex          \providecommand{\numXxx}{...} per number (CamelCase, no digits
#                                 in macro names, so they are valid LaTeX control words);
#                                 \providecommand never clashes with a definition already in main.tex
#   PIPE_OUT/csv/46_numbers.csv   register: macro, value (10 significant digits), formatted,
#                                 source_file (relative to PIPE_OUT), source_field (row selector and column)
#
# Formatting (paper style): coefficients, SEs and NSEs 3 decimals; t-statistics 2 decimals;
# correlations, ICCs and AUCs 2 decimals; percentages 1 decimal WITHOUT the % sign (macro names end
# in Pct; write \numXxxPct\%); counts with thousands separators as 3{,}371 (works in text and
# math); negative numbers as \ensuremath{-}0.459 (works in text and math).
# Must run after tables, speccurve, nse, ideology, oa, ind4 and a13.
# Since 2026-10-27 also the marginal effects at left / centre / right of R/41 section 13
# (\numMe<Outcome><Left|Centre|Right>[Se], \numInterP<Outcome>, \numMeIdeoAt<...>; Fin = financial markets).
# ============================================================

source("R/0_Config.R")

# Public mode (2026-10-27). Macros whose source file is restricted (not produced from the public
# inputs; the files R/71 --public lists as not public) get the placeholder below and public = FALSE.
# Both modes write numbers_public.tex and csv/46_numbers_public.csv with those placeholders, so the
# public files are byte-identical across modes; the full files numbers.tex / 46_numbers.csv
# (all values) are written in full mode only.
RESTRICTED_RE <- "^(csv/44_|csv/36_|csv/47_|csv/45_right_(quote_classes|protect_quotes|score_by_class)|validation/a13/|validation/independence/(tables/t2|cache/t2_)|csv/49_|argentina/)"
is_restricted <- function(f) grepl(RESTRICTED_RE, f)
PLACEHOLDER <- "\\textit{n/a (restricted)}"

# "ROOT/<path>": an archived file outside PIPE_OUT, relative to the project root (a manifest INPUT,
# e.g. ROOT/output/independence/tables/t3_slope_bootstrap.csv of the API-only Test 3)
rd <- function(f) {
  p <- if (startsWith(f, "ROOT/")) substring(f, 6) else file.path(PIPE_OUT, f)
  if (!file.exists(p)) stop("R/46: chain output missing: ", p, " (run the stages it comes from first)")
  utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
}
cache <- new.env()
tab <- function(f) { if (is.null(cache[[f]])) assign(f, rd(f), envir = cache); cache[[f]] }
# one value: file, column, and a row selector given as name = value pairs (all must match)
pick <- function(f, col, ...) {
  sel <- list(...)
  if (PIPE_PUBLIC && is_restricted(f))
    return(structure(NA_real_, src = list(file = f, field = ""), restricted = TRUE))
  x <- tab(f); ok <- rep(TRUE, nrow(x))
  for (k in names(sel)) ok <- ok & as.character(x[[k]]) == as.character(sel[[k]])
  if (sum(ok) != 1) stop(sprintf("R/46: %s [%s] selects %d rows (need 1)", f,
                                 paste(names(sel), sel, sep = "=", collapse = ", "), sum(ok)))
  if (!col %in% names(x)) stop("R/46: column ", col, " not in ", f)
  v <- x[[col]][ok]
  attr(v, "src") <- list(file = f, field = paste0(if (length(sel)) paste0(paste(names(sel), sel, sep = "=", collapse = "; "), " -> ") else "", col))
  v
}

neg <- function(s, v) ifelse(v < 0, paste0("\\ensuremath{-}", sub("^-", "", s)), s)
fdec <- function(v, d) { s <- formatC(v, format = "f", digits = d); s[s == formatC(-0, format = "f", digits = d)] <- formatC(0, format = "f", digits = d); neg(s, as.numeric(s)) }
fcount <- function(v) gsub(",", "{,}", formatC(round(v), format = "d", big.mark = ","), fixed = TRUE)
FMT <- list(coef = function(v) fdec(v, 3), se = function(v) fdec(v, 3), t = function(v) fdec(v, 2),
            r = function(v) fdec(v, 2), pct = function(v) fdec(100 * v, 1), pctval = function(v) fdec(v, 1),
            count = fcount, year = function(v) formatC(round(v), format = "d"),
            p = function(v) ifelse(v < 0.001, "<0.001", fdec(v, 3)))

reg <- list()
add <- function(macro, v, type, src = attr(v, "src"), value = as.numeric(v)) {
  restr <- isTRUE(attr(v, "restricted"))
  stopifnot(grepl("^num[A-Z][A-Za-z]*$", macro), length(value) == 1, restr || is.finite(value), type %in% names(FMT))
  if (macro %in% names(reg)) stop("R/46: duplicate macro ", macro)
  reg[[macro]] <<- data.frame(macro = macro, value = if (restr) NA_real_ else value,
                              formatted = if (restr) PLACEHOLDER else FMT[[type]](value),
                              source_file = src$file, source_field = src$field,
                              public = !is_restricted(src$file), stringsAsFactors = FALSE)
}
derived <- function(v, file, field) { attr(v, "src") <- list(file = file, field = field); v }

# ---- Corpus counts (41_corpus_counts.csv) ---------------------------------------------------------
CC <- "csv/41_corpus_counts.csv"
for (s in list(c("Corpus", "corpus (scoring pool)"), c("Analysis", "analysis (POP and LIB overall non-missing)"),
               c("Ideo", "with IDEO as well"))) {
  add(paste0("num", s[1], "Manifestos"), pick(CC, "manifestos", sample = s[2]), "count")
  add(paste0("num", s[1], "Parties"),    pick(CC, "parties", sample = s[2]), "count")
  add(paste0("num", s[1], "Countries"),  pick(CC, "countries", sample = s[2]), "count")
  add(paste0("num", s[1], "Elections"),  pick(CC, "elections", sample = s[2]), "count")
  add(paste0("num", s[1], "FirstYear"),  pick(CC, "first_year", sample = s[2]), "year")
  add(paste0("num", s[1], "LastYear"),   pick(CC, "last_year", sample = s[2]), "year")
}

# ---- Tables 4-8 (41_tab4_8_regressions_long.csv) ----------------------------------------------------
RL <- "csv/41_tab4_8_regressions_long.csv"
OUTC <- c(liberalism_overall = "Lib", lib_political = "LibPol", lib_social = "LibSoc", lib_economic = "LibEcon",
          lib_financial_market = "LibFin")
add("numHeadBeta",     pick(RL, "estimate", dv = "liberalism_overall", vcov = "clparty", model = "m1", term = "populism_overall"), "coef")
add("numHeadSe",       pick(RL, "se", dv = "liberalism_overall", vcov = "clparty", model = "m1", term = "populism_overall"), "se")
add("numHeadT",        pick(RL, "t", dv = "liberalism_overall", vcov = "clparty", model = "m1", term = "populism_overall"), "t")
add("numHeadN",        pick(RL, "nobs", dv = "liberalism_overall", vcov = "clparty", model = "m1", term = "populism_overall"), "count")
add("numHeadClusters", pick(RL, "n_clusters", dv = "liberalism_overall", vcov = "clparty", model = "m1", term = "populism_overall"), "count")
add("numHeadSeCountry", pick(RL, "se", dv = "liberalism_overall", vcov = "clcountry", model = "m1", term = "populism_overall"), "se")
add("numHeadCountryClusters", pick(RL, "n_clusters", dv = "liberalism_overall", vcov = "clcountry", model = "m1", term = "populism_overall"), "count")
for (dv in names(OUTC)) {
  o <- OUTC[[dv]]
  for (cc in list(c("ColOne", "m1", "populism_overall", "Beta"), c("ColFour", "m4", "populism_overall", "Beta"),
                  c("ColFour", "m4", "IDEO", "Ideo"), c("ColFive", "m5", "populism_overall:IDEO", "Inter"),
                  c("ColSix", "m6", "populism_overall", "Beta"), c("ColSix", "m6", "IDEO", "Ideo"),
                  c("ColSeven", "m7", "populism_overall", "Beta"), c("ColSeven", "m7", "populism_overall:IDEO", "Inter"))) {
    stem <- paste0("num", cc[1], cc[4], o)
    add(stem,                pick(RL, "estimate", dv = dv, vcov = "clparty", model = cc[2], term = cc[3]), "coef")
    add(paste0(stem, "Se"),  pick(RL, "se", dv = dv, vcov = "clparty", model = cc[2], term = cc[3]), "se")
    add(paste0(stem, "T"),   pick(RL, "t", dv = dv, vcov = "clparty", model = cc[2], term = cc[3]), "t")
    add(paste0(stem, "P"),   pick(RL, "p", dv = dv, vcov = "clparty", model = cc[2], term = cc[3]), "p")
  }
  add(paste0("numColOneN", o),  pick(RL, "nobs", dv = dv, vcov = "clparty", model = "m1", term = "populism_overall"), "count")
  add(paste0("numColFourN", o), pick(RL, "nobs", dv = dv, vcov = "clparty", model = "m4", term = "IDEO"), "count")
  add(paste0("numColSixN", o),  pick(RL, "nobs", dv = dv, vcov = "clparty", model = "m6", term = "IDEO"), "count")
  add(paste0("numColSevenN", o), pick(RL, "nobs", dv = dv, vcov = "clparty", model = "m7", term = "IDEO"), "count")
}

# ---- Marginal effects at left / centre / right (41_fig_me_points.csv; Marginal_Effects_Dots.pdf) --------
# Model (5) on the analysis sample, party-clustered; IDEO at its p10 / median / p90 (section 13 of R/41)
MEP <- "csv/41_fig_me_points.csv"
ME_O <- c(liberalism_overall = "Overall", lib_political = "Political", lib_social = "Social", lib_economic = "Economic",
          lib_financial_market = "Fin")
ME_AT <- c("Left (p10)" = "Left", "Centre (median)" = "Centre", "Right (p90)" = "Right")
for (v in names(ME_O)) {
  for (a in names(ME_AT)) {
    add(paste0("numMe", ME_O[[v]], ME_AT[[a]]), pick(MEP, "me", var = v, at = a), "coef")
    add(paste0("numMe", ME_O[[v]], ME_AT[[a]], "Se"), pick(MEP, "se", var = v, at = a), "se")
  }
  add(paste0("numInterP", ME_O[[v]]), pick(MEP, "p_inter", var = v, at = "Centre (median)"), "p")
  add(paste0("numMeN", ME_O[[v]]), pick(MEP, "n", var = v, at = "Centre (median)"), "count")
  add(paste0("numMeRefColOne", ME_O[[v]]), pick(MEP, "ref_m1", var = v, at = "Centre (median)"), "coef")
}
for (a in names(ME_AT)) add(paste0("numMeIdeoAt", ME_AT[[a]]), pick(MEP, "ideo_at", var = "liberalism_overall", at = a), "r")
# The figure models are the Tables 4-8 models (R/41 section 13), so these must equal the table macros
for (v in names(ME_O)) {
  stopifnot(identical(reg[[paste0("numInterP", ME_O[[v]])]]$value, reg[[paste0("numColFiveInter", OUTC[[v]], "P")]]$value),
            identical(reg[[paste0("numMeN", ME_O[[v]])]]$value, reg[[paste0("numColOneN", OUTC[[v]])]]$value) ||
              identical(reg[[paste0("numMeN", ME_O[[v]])]]$value,
                        as.numeric(pick(RL, "nobs", dv = v, vcov = "clparty", model = "m5", term = "populism_overall:IDEO"))),
            identical(reg[[paste0("numMeRefColOne", ME_O[[v]])]]$value, reg[[paste0("numColOneBeta", OUTC[[v]])]]$value))
}

# ---- Specification curve (42_headline_distribution.csv, 42_grid_accounting.csv) -----------------------
HD <- "csv/42_headline_distribution.csv"
for (e in list(c("Pop", "pop"), c("Inter", "inter"))) {
  sel <- list(set = "distinct specs, party SE", estimand = e[2])
  g <- function(col) do.call(pick, c(list(HD, col), sel))
  n <- g("n")
  add(paste0("numSpec", e[1], "N"), n, "count")
  add(paste0("numSpec", e[1], "ShareSigNegPct"), g("share_sig_neg"), "pct")
  add(paste0("numSpec", e[1], "ShareSigPosPct"), g("share_sig_pos"), "pct")
  add(paste0("numSpec", e[1], "CountSigPos"), derived(round(g("share_sig_pos") * n), HD, paste0("set=distinct specs, party SE; estimand=", e[2], " -> share_sig_pos * n")), "count")
  add(paste0("numSpec", e[1], "CountPos"), derived(round(g("share_pos") * n), HD, paste0("set=distinct specs, party SE; estimand=", e[2], " -> share_pos * n")), "count")
  add(paste0("numSpec", e[1], "SharePosPct"), g("share_pos"), "pct")
  add(paste0("numSpec", e[1], "Median"), g("median"), "coef")
  add(paste0("numSpec", e[1], "Mean"), g("mean"), "coef")
  add(paste0("numSpec", e[1], "PctTwentyFive"), g("p25"), "coef")
  add(paste0("numSpec", e[1], "PctSeventyFive"), g("p75"), "coef")
  add(paste0("numSpec", e[1], "PctTwoFive"), g("p2.5"), "coef")
  add(paste0("numSpec", e[1], "PctNinetySevenFive"), g("p97.5"), "coef")
}
GA <- "csv/42_grid_accounting.csv"
add("numSpecGridCells", pick(GA, "cells", estimand = "pop"), "count")
add("numSpecInterCellsFailed", pick(GA, "cells_failed", estimand = "inter"), "count")
add("numSpecRegressions", pick(GA, "regressions_run", estimand = "pop"), "count")

# ---- NSE pools (43_nse_rules.csv): measurement 276, outcome fixed 3,312, all 20,592 ------------------
NR <- "csv/43_nse_rules.csv"
for (pl in list(c("Measure", "measure"), c("Outcome", "outcome"), c("All", "all"))) {
  for (e in list(c("Pop", "pop"), c("Inter", "inter"))) {
    stem <- paste0("numNse", pl[1], e[1])
    add(paste0(stem, "Specs"),   pick(NR, "n_specs", pool = pl[2], estimand = e[2]), "count")
    add(stem,                    pick(NR, "nse_sd", pool = pl[2], estimand = e[2]), "se")
    add(paste0(stem, "SeTotal"), pick(NR, "se_total", pool = pl[2], estimand = e[2]), "se")
    add(paste0(stem, "TTotal"),  pick(NR, "t_total", pool = pl[2], estimand = e[2]), "t")
    add(paste0(stem, "Median"),  pick(NR, "median", pool = pl[2], estimand = e[2]), "coef")
    add(paste0(stem, "RatioSe"), pick(NR, "nse_over_se", pool = pl[2], estimand = e[2]), "t")
  }
}

# ---- IDEO validity (44_external_validity.csv; independence T4) --------------------------------------
EV <- "csv/44_external_validity.csv"
for (x in list(c("Rile", "rile"), c("RileLogit", "rile_logit"), c("ChesLrgen", "lrgen"), c("ChesLrecon", "lrecon"),
               c("ChesGaltan", "galtan"), c("Parlgov", "parlgov_lr"))) {
  add(paste0("numIdeoR", x[1]), pick(EV, "r", external = x[2], llm_measure = "IDEO"), "r")
  add(paste0("numIdeoRho", x[1]), pick(EV, "rho", external = x[2], llm_measure = "IDEO"), "r")
  add(paste0("numIdeoN", x[1]), pick(EV, "n", external = x[2], llm_measure = "IDEO"), "count")
}
T4 <- "validation/independence/tables/t4_populist_validity.csv"
add("numPopulistAuc", pick(T4, "AUC_llm_pop", pl_populist = 1), "r")
add("numPopulistN", pick(T4, "n", pl_populist = 1), "count")
add("numNonPopulistN", pick(T4, "n", pl_populist = 0), "count")
add("numPopulistMeanPop", pick(T4, "mean_llm_pop", pl_populist = 1), "r")
add("numNonPopulistMeanPop", pick(T4, "mean_llm_pop", pl_populist = 0), "r")

# ---- Determinism (A.13) and cross-family agreement ----------------------------------------------------
CD <- "validation/a13/csv/chunk_level_determinism.csv"
FAM <- c(sonnet = "Sonnet", gpt41mini = "Gpt", gemini = "Gemini", pooled = "Pooled")
for (m in names(FAM)) {
  add(paste0("numDetChunkExact", FAM[[m]], "Pct"), pick(CD, "exact_score", comparison = "run1_vs_run2", model = m), "pct")
  add(paste0("numDetChunkMad", FAM[[m]]), pick(CD, "mad", comparison = "run1_vs_run2", model = m), "r")
  add(paste0("numDetChunkR", FAM[[m]]), pick(CD, "r", comparison = "run1_vs_run2", model = m), "r")
}
MD <- "validation/a13/csv/manifesto_level_determinism.csv"
for (x in list(c("Pop", "populism_overall"), c("Lib", "liberalism_overall"))) {
  add(paste0("numDetManif", x[1], "Mad"), pick(MD, "mad", comparison = "run1_vs_run2", aggregation = "baseline_cwm", model = "cross_model_mean", dim = x[2]), "r")
  add(paste0("numDetManif", x[1], "R"), pick(MD, "r", comparison = "run1_vs_run2", aggregation = "baseline_cwm", model = "cross_model_mean", dim = x[2]), "r")
}
CF <- "validation/a13/csv/crossfamily_agreement_fullcorpus.csv"
for (x in list(c("Pop", "populism_overall"), c("Lib", "liberalism_overall"), c("LibEcon", "lib_economic"), c("LibSoc", "lib_social"))) {
  add(paste0("numAgree", x[1], "IccAll"), pick(CF, "icc_a1", dim = x[2], pair = "all_three"), "r")
  for (pr in list(c("SonnetGpt", "sonnet_gpt41mini"), c("SonnetGemini", "sonnet_gemini"), c("GptGemini", "gpt41mini_gemini")))
    add(paste0("numAgree", x[1], "R", pr[1]), pick(CF, "r", dim = x[2], pair = pr[2]), "r")
}

# ---- Model provenance (40_model_provenance.csv) ----------------------------------------------------------
MP <- "csv/40_model_provenance.csv"
add("numProvSonnetLoggedPct", pick(MP, "share_chunks", model = "sonnet", resolved_status = "logged"), "pct")
add("numProvGptLoggedPct", pick(MP, "share_chunks", model = "gpt41mini", resolved_status = "logged"), "pct")
add("numProvGeminiInferredPct", pick(MP, "share_chunks", model = "gemini", resolved_status = "inferred"), "pct")
add("numProvGeminiLoggedPct", derived(sum(tab(MP)$share_chunks[tab(MP)$model == "gemini" & tab(MP)$resolved_status == "logged"]),
                                      MP, "model=gemini; resolved_status=logged -> sum(share_chunks) over endpoints"), "pct")

# ---- Trends (41_fig4_5_trend_slopes.csv) ------------------------------------------------------------------
TS <- "csv/41_fig4_5_trend_slopes.csv"
for (x in list(c("Pop", "populism_overall", "all"), c("PopEighty", "populism_overall", "all, 1980+"), c("Lib", "liberalism_overall", "all"),
               c("LibLeft", "liberalism_overall", "Left"), c("LibRight", "liberalism_overall", "Right"))) {
  add(paste0("numTrend", x[1], "Slope"), pick(TS, "slope_manifesto_per_decade", series = x[2], group = x[3]), "coef")
  add(paste0("numTrend", x[1], "T"), pick(TS, "t_cl_year", series = x[2], group = x[3]), "t")
  add(paste0("numTrend", x[1], "SlopeYearMeans"), pick(TS, "slope_per_decade", series = x[2], group = x[3]), "coef")
}

# ---- Centrist component and alternative IDEO (stage oa, 45_*) ------------------------------------------------
add("numCentristRPop", pick("csv/45_centrist_cor_by_model.csv", "r_C_POP", model = "cross_model"), "r")
add("numCentristRAbsRl", pick("csv/45_centrist_cor_by_model.csv", "r_C_absRL", model = "cross_model"), "r")
add("numCentristRPc", pick("csv/45_centrist_cor_by_model.csv", "r_C_PC", model = "cross_model"), "r")
add("numCentristRPopGemini", pick("csv/45_centrist_cor_by_model.csv", "r_C_POP", model = "gemini"), "r")
add("numCentristMean", pick("csv/45_centrist_desc.csv", "mean", var = "C"), "coef")
add("numCentristSharePct", pick("csv/45_centrist_desc.csv", "mean", var = "C_share"), "pct")
add("numShrinkMean", pick("csv/45_ideo_vs_ideo_c.csv", "mean_shrink"), "r")
add("numIdeoCCor", pick("csv/45_ideo_vs_ideo_c.csv", "r_IDEO_IDEOc"), "r")
add("numIdeoCSignChanges", pick("csv/45_ideo_vs_ideo_c.csv", "sign_changes"), "count")
add("numIdeoCSameClassPct", pick("csv/45_ideo_vs_ideo_c.csv", "share_class_same_pm0.2"), "pct")
AV <- "csv/45_ideo_alt_validity.csv"
for (x in list(c("Ideo", "IDEO"), c("IdeoC", "IDEO_c"), c("IdeoDiff", "IDEO_diff"), c("Centrist", "C")))
  add(paste0("numRile", x[1], "R"), pick(AV, "r_rile", measure = x[2]), "r")
AR <- "csv/45_ideo_alt_regs.csv"
for (dv in names(OUTC)) {
  o <- OUTC[[dv]]
  add(paste0("numIdeoPlusCBetaC", o), pick(AR, "b_C_m4", dv = dv, iv = "IDEO + C"), "coef")
  add(paste0("numIdeoPlusCBetaIdeo", o), pick(AR, "b_ideo_m4", dv = dv, iv = "IDEO + C"), "coef")
  add(paste0("numIdeoPlusCInter", o), pick(AR, "b_inter_m5", dv = dv, iv = "IDEO + C"), "coef")
  add(paste0("numIdeoPlusCInterP", o), pick(AR, "p_inter_m5", dv = dv, iv = "IDEO + C"), "p")
  add(paste0("numIdeoCInter", o), pick(AR, "b_inter_m5", dv = dv, iv = "IDEO_c"), "coef")
  add(paste0("numIdeoDiffInter", o), pick(AR, "b_inter_m5", dv = dv, iv = "IDEO_diff"), "coef")
  add(paste0("numIdeoStdBeta", o), pick(AR, "b_ideo_m4_std", dv = dv, iv = "IDEO"), "coef")
}

# ---- R vs economic liberalism (stage oa, 45_*) ------------------------------------------------------------------
RV <- "csv/45_right_vs_lib.csv"
add("numRightLibEconR", pick(RV, "r_R_pooled", lib = "lib_economic"), "r")
add("numRightLibEconRWithin", pick(RV, "r_R_within_cy", lib = "lib_economic"), "r")
add("numRightLibEconRPartialPop", pick(RV, "r_R_partial_POP", lib = "lib_economic"), "r")
add("numRightLibEconRPartialPopL", pick(RV, "r_R_partial_POP_L", lib = "lib_economic"), "r")
add("numRightLibSocR", pick(RV, "r_R_pooled", lib = "lib_social"), "r")
add("numLeftLibEconR", pick(RV, "r_L_pooled", lib = "lib_economic"), "r")
RR <- "csv/45_right_regs.csv"
add("numRightRegBetaR", pick(RR, "estimate", model = "a", term = "R"), "coef")
add("numRightRegBetaRCmpId", pick(RR, "estimate", model = "b", term = "R"), "coef")
add("numRightRegBetaRCmpEcon", pick(RR, "estimate", model = "c", term = "R"), "coef")
add("numRightRegProtOnR", pick(RR, "estimate", model = "d", term = "per406"), "coef")
add("numRightRegMultiOnR", pick(RR, "estimate", model = "d", term = "per608"), "coef")
QC <- "csv/45_right_quote_classes.csv"
add("numRightQuoteIdentityPct", pick(QC, "share", model = "all", scope = "quote", cls = "identity/immigration only"), "pct")
add("numRightQuoteEconPct", pick(QC, "share", model = "all", scope = "quote", cls = "economic/trade only"), "pct")
add("numRightQuoteBothPct", pick(QC, "share", model = "all", scope = "quote", cls = "both"), "pct")
add("numRightQuoteNeitherPct", pick(QC, "share", model = "all", scope = "quote", cls = "neither"), "pct")
add("numRightQuoteCtxIdentityPct", pick(QC, "share", model = "all", scope = "quote+context", cls = "identity/immigration only"), "pct")
add("numRightQuoteCtxEconPct", pick(QC, "share", model = "all", scope = "quote+context", cls = "economic/trade only"), "pct")
add("numRightQuoteN", if (PIPE_PUBLIC) pick(QC, "n") else derived(sum(tab(QC)$n[tab(QC)$model == "all" & tab(QC)$scope == "quote"]), QC, "model=all; scope=quote -> sum(n)"), "count")
add("numRightQuoteProtectN", pick("csv/45_right_protect_quotes.csv", "n_prot_quotes", cls = "neither"), "count")
add("numRightQuoteProtectPct", pick("csv/45_right_protect_quotes.csv", "share_of_all_quotes", cls = "neither"), "pct")

add("numRightQuoteCtxBothPct", pick(QC, "share", model = "all", scope = "quote+context", cls = "both"), "pct")
add("numRightQuoteCtxNeitherPct", pick(QC, "share", model = "all", scope = "quote+context", cls = "neither"), "pct")
CC45 <- "csv/45_cmp_category_cor.csv"
for (x in list(c("SixOhOne", "per601"), c("SixOhEight", "per608"), c("FourOhSix", "per406"), c("FourOhSeven", "per407"),
               c("FourOhOne", "per401"), c("OneOhNine", "per109"), c("OneTen", "per110"))) {
  add(paste0("numRightCorPer", x[1]), pick(CC45, "r_R", cat = x[2]), "r")
  add(paste0("numLibEconCorPer", x[1]), pick(CC45, "r_libecon", cat = x[2]), "r")
}

# ---- Ideological extremity |IDEO| (stage oa, 45_abs_ideo_*; tab:oa2e) ----------------------------------------------
AC <- "csv/45_abs_ideo_cor.csv"
add("numAbsIdeoRPop", pick(AC, "value", block = "pooled r(|IDEO|, var)", var = "POP"), "r")
add("numAbsIdeoRIdeo", pick(AC, "value", block = "pooled r(|IDEO|, var)", var = "IDEO"), "r")
add("numAbsIdeoRPopWithin", pick(AC, "value", block = "within country-year r(|IDEO|, var)", var = "POP"), "r")
AB <- "csv/45_abs_ideo_regs.csv"
for (dv in names(OUTC)) {
  o <- OUTC[[dv]]
  add(paste0("numAbsIdeoBetaPop", o), pick(AB, "estimate", dv = dv, model = "c4_abs", term = "POP"), "coef")
  add(paste0("numAbsIdeoBetaIdeo", o), pick(AB, "estimate", dv = dv, model = "c4_abs", term = "IDEO"), "coef")
  add(paste0("numAbsIdeoBeta", o), pick(AB, "estimate", dv = dv, model = "c4_abs", term = "ABS"), "coef")
  add(paste0("numAbsIdeoBetaSe", o), pick(AB, "se", dv = dv, model = "c4_abs", term = "ABS"), "se")
  add(paste0("numAbsIdeoInter", o), pick(AB, "estimate", dv = dv, model = "c5_abs", term = "POP:IDEO"), "coef")
  add(paste0("numAbsIdeoInterP", o), pick(AB, "p", dv = dv, model = "c5_abs", term = "POP:IDEO"), "p")
  add(paste0("numAbsIdeoLeftExt", o), pick(AB, "estimate", dv = dv, model = "sym", term = "L_ext"), "coef")
  add(paste0("numAbsIdeoLeftExtP", o), pick(AB, "p", dv = dv, model = "sym", term = "L_ext"), "p")
  add(paste0("numAbsIdeoRightExt", o), pick(AB, "estimate", dv = dv, model = "sym", term = "R_ext"), "coef")
  add(paste0("numAbsIdeoRightExtP", o), pick(AB, "p", dv = dv, model = "sym", term = "R_ext"), "p")
}

# ---- External validity, paper Table 3 (stage ideology, 44_external_validity_table.csv; tab:external) -------------------
ET <- "csv/44_external_validity_table.csv"
for (x in list(c("Rile", "rile"), c("RileLogit", "rile_logit"), c("ChesLrgen", "lrgen"), c("ChesLrecon", "lrecon"),
               c("ChesGaltan", "galtan"), c("Parlgov", "parlgov_lr"))) {
  add(paste0("numPopR", x[1]), pick(ET, "value", measure = "POP", external = x[2]), "r")
  add(paste0("numPopN", x[1]), pick(ET, "n", measure = "POP", external = x[2]), "count")
}
add("numIdeoAucFarRight", pick(ET, "value", measure = "IDEO", external = "farright_vs_farleft"), "r")
add("numIdeoAucFarRightN", pick(ET, "n", measure = "IDEO", external = "farright_vs_farleft"), "count")
add("numPopAucPopulistN", pick(ET, "n", measure = "POP", external = "populist"), "count")

# ---- Halo / separate calls (independence Test 3, archived; OLS and country + decade FE) -----------------------------------
HB <- "ROOT/output/independence/tables/t3_slope_bootstrap.csv"
for (m in list(c("Gpt", "gpt41mini"), c("Sonnet", "sonnet"))) for (sp in list(c("", "OLS"), c("Fe", "country + decade FE"))) {
  stem <- paste0("numHalo", m[1], sp[1])
  add(paste0(stem, "Joint"),    pick(HB, "estimate", model = m[2], spec = sp[2], stat = "bj"), "coef")
  add(paste0(stem, "Separate"), pick(HB, "estimate", model = m[2], spec = sp[2], stat = "bs"), "coef")
  add(paste0(stem, "Ratio"),    pick(HB, "estimate", model = m[2], spec = sp[2], stat = "ratio"), "r")
  add(paste0(stem, "RatioLo"),  pick(HB, "ci_lo", model = m[2], spec = sp[2], stat = "ratio"), "r")
  add(paste0(stem, "RatioHi"),  pick(HB, "ci_hi", model = m[2], spec = sp[2], stat = "ratio"), "r")
  add(paste0(stem, "N"),        pick(HB, "n", model = m[2], spec = sp[2], stat = "ratio"), "count")
}

# ---- Quote-length audit (stage recheck, 36_quote_length_audit.csv) --------------------------------------------------------
QL <- "csv/36_quote_length_audit.csv"
for (m in list(c("Sonnet", "claude"), c("Gpt", "gpt41mini"), c("Gemini", "gemini"))) {
  add(paste0("numQlenGtEight", m[1], "Pct"), pick(QL, "quote_gt8", set = m[2]), "pct")
  add(paste0("numQlenMax", m[1]), pick(QL, "quote_max", set = m[2]), "count")
  add(paste0("numQlenSpans", m[1]), pick(QL, "spans", set = m[2]), "count")
  add(paste0("numContextLenMax", m[1]), pick(QL, "context_max", set = m[2]), "count")
  add(paste0("numContextLenMedian", m[1]), pick(QL, "context_median", set = m[2]), "count")
  add(paste0("numContextGtFifty", m[1], "Pct"), pick(QL, "context_gt50", set = m[2]), "pct")
}

# ---- Table 3: POPPA, GPS and matches (stage ideology; 44_external_validity_table.csv, 44_external_matches.csv) --------
for (x in list(c("PoppaPopulism", "poppa_populism"), c("PoppaLr", "poppa_lroverall"), c("PoppaLrecon", "poppa_lrecon"),
               c("GpsPopulism", "gps_v8_populism"), c("GpsEconLr", "gps_v4_econ_lr"), c("GpsSocial", "gps_v6_social"),
               c("CelicoRhet", "rfpopi_rhet"), c("CelicoIde", "rfpopi_ide"))) {
  add(paste0("numPopR", x[1]), pick(ET, "value", measure = "POP", external = x[2]), "r")
  add(paste0("numPopN", x[1]), pick(ET, "n", measure = "POP", external = x[2]), "count")
  add(paste0("numIdeoR", x[1]), pick(ET, "value", measure = "IDEO", external = x[2]), "r")
  add(paste0("numIdeoN", x[1]), pick(ET, "n", measure = "IDEO", external = x[2]), "count")
}
EM <- "csv/44_external_matches.csv"
for (x in list(c("Ches", "CHES"), c("Parlgov", "ParlGov"), c("Populist", "PopuList"), c("Poppa", "POPPA"), c("Gps", "Global Party Survey"), c("Celico", "RFPOPI (Celico et al. 2024)"))) {
  add(paste0("numMatch", x[1], "Parties"), pick(EM, "parties_matched", dataset = x[2]), "count")
  add(paste0("numMatch", x[1], "Manifestos"), pick(EM, "manifestos_matched", dataset = x[2]), "count")
}

# ---- Independence (Appendix B): evidence overlap, CMP benchmark, PopuList dummies ---------------------------------------
OV <- "validation/independence/tables/t2a_evidence_overlap.csv"
for (m in list(c("Sonnet", "sonnet"), c("Gpt", "gpt41mini"), c("Gemini", "gemini"))) {
  add(paste0("numOverlapIdentical", m[1], "Pct"), pick(OV, "pct_identical", pair_type = "populism sub-dim x liberalism sub-dim", model = m[2]), "pctval")
  add(paste0("numOverlapJaccard", m[1], "Pct"), pick(OV, "pct_jaccard_ge_.5", pair_type = "populism sub-dim x liberalism sub-dim", model = m[2]), "pctval")
  add(paste0("numOverlapSpan", m[1], "Pct"), pick(OV, "pct_span_overlap", pair_type = "populism sub-dim x liberalism sub-dim", model = m[2]), "pctval")
  add(paste0("numOverlapNear", m[1], "Pct"), pick(OV, "pct_within_300ch", pair_type = "populism sub-dim x liberalism sub-dim", model = m[2]), "pctval")
  add(paste0("numOverlapNearNull", m[1], "Pct"), pick(OV, "pct_within_300ch_null", pair_type = "populism sub-dim x liberalism sub-dim", model = m[2]), "pctval")
}
AT <- "validation/independence/tables/t1_attenuation_benchmark.csv"
for (x in list(c("Lib", "CMP_LIB"), c("Pol", "CMP_POL"), c("Soc", "CMP_SOC"), c("Econ", "CMP_ECON"),
               c("EconMarket", "CMP_ECON_ordo"), c("EconWelfare", "CMP_ECON_welf"))) {
  add(paste0("numAtten", x[1], "Pct"), pick(AT, "share_of_expected", cmp = x[2]), "pct")
  add(paste0("numAtten", x[1], "SlopeCmp"), pick(AT, "b_cmp", cmp = x[2]), "coef")
}
PR <- "validation/independence/tables/t4_populist_regressions.csv"
for (dv in names(OUTC)) {
  o <- OUTC[[dv]]
  add(paste0("numPopulistDummy", o), pick(PR, "estimate", dv = dv, controls = "none"), "coef")
  add(paste0("numPopulistDummyP", o), pick(PR, "p", dv = dv, controls = "none"), "p")
  add(paste0("numPopulistDummyFar", o), pick(PR, "estimate", dv = dv, controls = "+ PopuList far-right/far-left"), "coef")
  add(paste0("numPopulistDummyFarP", o), pick(PR, "p", dv = dv, controls = "+ PopuList far-right/far-left"), "p")
}

# ---- Appendix A: A.10 computational summary, parse loss, topic check, recheck ------------------------------------------------
A10 <- "csv/41_A10_computational_summary.csv"
for (x in list(c("ChunkRecords", "chunk records, all three models", "count"),
               c("VerifiedPct", "share of dimension judgements with status verified (10 dims, %)", "pctval"),
               c("SpanValidPct", "share verified and passing span validation (10 dims, %)", "pctval"),
               c("NoEvidencePct", "share no_evidence (%)", "pctval"), c("MixedPct", "share mixed (%)", "pctval")))
  add(paste0("numATen", x[1]), pick(A10, "value", item = x[2]), x[3])
PLC <- "csv/36_parse_loss_counts.csv"
add("numParseLossChunks", pick(PLC, "parse_lost_chunks_rev1"), "count")
add("numParseLossChunkDims", pick(PLC, "parse_lost_chunk_dims_rev1"), "count")
add("numParseLossChunksAllDims", pick(PLC, "parse_lost_chunks_all13_rev1"), "count")
add("numParseLossLenient", pick(PLC, "lenient_decoded_dims_rev2"), "count")
TC <- "csv/40_topic_check_diagnostic.csv"
tc_band <- function(b) { x <- tab(TC); x <- x[x$score_band == b, ]
  derived(sum(x$n * x$topic_pass) / sum(x$n), TC, sprintf("score_band=%s -> sum(n * topic_pass) / sum(n), all models and dims", b)) }
add("numTopicPassLowPct", tc_band("0-2"), "pct")
add("numTopicPassMidLowPct", tc_band("3-4"), "pct")
add("numTopicPassMidHighPct", tc_band("5-6"), "pct")
add("numTopicPassHighPct", tc_band("7-10"), "pct")
RQ <- "csv/36_quote_check_recomputed.csv"
add("numRecheckSpans", if (PIPE_PUBLIC) pick(RQ, "spans") else
  derived(sum(tab(RQ)$spans[tab(RQ)$set %in% c("claude", "gpt41mini", "gemini")]), RQ, "set in claude, gpt41mini, gemini -> sum(spans)"), "count")
add("numRecheckDisagree", if (PIPE_PUBLIC) pick(RQ, "n_disagree") else
  derived(sum(tab(RQ)$n_disagree), RQ, "all sets -> sum(n_disagree)"), "count")

# ---- OA additions 2026-10-27 (interaction robustness, quote lengths, validation by model, validity by model) ---------
IRB <- "csv/44_interaction_robustness.csv"
for (dv in names(OUTC)) {
  add(paste0("numInterRobSigNeg", OUTC[[dv]]), pick(IRB, "sig_neg", dv = dv), "count")
  add(paste0("numInterRobSigPos", OUTC[[dv]]), pick(IRB, "sig_pos", dv = dv), "count")
}
QLD <- "csv/47_quote_length_dist.csv"
for (m in list(c("Sonnet", "sonnet"), c("Gpt", "gpt41mini"), c("Gemini", "gemini"))) {
  add(paste0("numQlenMedian", m[1]), pick(QLD, "q_median", model = m[2]), "count")
  add(paste0("numQlenPNinety", m[1]), pick(QLD, "q_p90", model = m[2]), "count")
  add(paste0("numContextLenPNinety", m[1]), pick(QLD, "c_p90", model = m[2]), "count")
}
VBM <- "csv/47_validation_by_model.csv"
for (m in list(c("Sonnet", "sonnet"), c("Gpt", "gpt41mini"), c("Gemini", "gemini"))) {
  add(paste0("numValidQuoteOk", m[1], "Pct"), if (PIPE_PUBLIC) pick(VBM, "verified_quote_ok") else {
    x <- tab(VBM); x <- x[x$model == m[2], ]
    derived(sum(x$records * x$verified_quote_ok) / sum(x$records), VBM, sprintf("model=%s -> records-weighted mean of verified_quote_ok over dims", m[2])) }, "pct")
}
EVM <- "csv/44_external_validity_by_model.csv"
for (m in list(c("Sonnet", "sonnet"), c("Gpt", "gpt41mini"), c("Gemini", "gemini"))) for (w in c("Pop", "Ideo"))
  for (x in list(c("Rile", "rile"), c("ChesLrgen", "lrgen"), c("Parlgov", "parlgov_lr"), c("PoppaPopulism", "poppa_populism"),
                 c("PoppaLr", "poppa_lroverall"), c("GpsPopulism", "gps_v8_populism"), c("GpsEconLr", "gps_v4_econ_lr"),
                 c("CelicoRhet", "rfpopi_rhet"), c("CelicoIde", "rfpopi_ide")))
    add(paste0("num", w, "R", x[1], m[1]), pick(EVM, "value", model = m[2], measure = toupper(w), external = x[2]), "r")

# ---- Party fixed effects (stage oa, 45_partyfe*) -------------------------------------------------------------------
PS <- "csv/45_partyfe_sample.csv"
add("numPfePartiesTwoPlus", pick(PS, "parties_ge2"), "count")
add("numPfeManifestosTwoPlus", pick(PS, "manifestos_in_parties_ge2"), "count")
PD <- "csv/45_partyfe_decomposition.csv"
for (x in list(c("Pop", "populism_overall"), c("Lib", "liberalism_overall"), c("LibEcon", "lib_economic"), c("Ideo", "IDEO")))
  add(paste0("numPfeWithinShare", x[1], "Pct"), pick(PD, "within_share", var = x[2]), "pct")
PF <- "csv/45_partyfe.csv"
for (dv in names(OUTC)) {
  o <- OUTC[[dv]]
  add(paste0("numPfeBetaWithin", o), pick(PF, "b6", dv = dv), "coef")
  add(paste0("numPfeSeWithin", o), pick(PF, "se6", dv = dv), "se")
  add(paste0("numPfeSeWithinCountry", o), pick(PF, "se6_country", dv = dv), "se")
  add(paste0("numPfeBetaBetween", o), pick(PF, "b_between", dv = dv), "coef")
  add(paste0("numPfeEffectSdTotal", o), pick(PF, "eff_1sd_total_col1", dv = dv), "coef")
  add(paste0("numPfeEffectSdWithin", o), pick(PF, "eff_1sd_within_col6", dv = dv), "coef")
}
add("numPfeSdPopTotal", pick(PF, "sd_pop_total", dv = "liberalism_overall"), "r")
add("numPfeSdPopWithin", pick(PF, "sd_pop_within", dv = "liberalism_overall"), "r")

# ---- Argentina out of sample (stage argentina, argentina/csv/53_*; PLLM-0185, 2026-10-28) -----------------------
# La Libertad Avanza platforms (macro suffix: TwentyOne = 2021 CF, TwentyThreeP = 2023 PASO, TwentyThree = 2023
# general, TwentyFiveBa / TwentyFiveCf = 2025), baseline group means, residuals from model (5) of Tables 4-8.
AR_S <- "argentina/csv/53_argentina_scores.csv"; AR_G <- "argentina/csv/53_group_means.csv"
AR_P <- "argentina/csv/53_predicted_liberalism.csv"
AR_DOCS <- c(TwentyOne = "AR2021G_CF_LLA", TwentyThreeP = "AR2023P_ON_LLA", TwentyThree = "AR2023G_ON_LLA",
             TwentyFiveBa = "AR2025G_BA_LLA", TwentyFiveCf = "AR2025G_CF_LLA")
AR_O <- c(populism_overall = "Pop", pop_ideology_direction = "Ideo", lib_political = "LibPol", lib_social = "LibSoc",
          lib_economic = "LibEcon", lib_financial_market = "LibFin", liberalism_overall = "Lib")
for (e in names(AR_DOCS)) for (v in names(AR_O))
  add(paste0("numLla", AR_O[[v]], e), pick(AR_S, v, doc_id = AR_DOCS[[e]]), "r")
# the Argentina stage is not run publicly (restricted inputs): in public mode these counts are placeholders like the picks
ar_derived <- function(fun, field) if (PIPE_PUBLIC) structure(NA_real_, src = list(file = AR_S, field = field), restricted = TRUE) else
  derived(fun(tab(AR_S)), AR_S, field)
add("numArDocs",     ar_derived(nrow, "rows"), "count")
add("numArDocsIdeo", ar_derived(function(x) sum(!is.na(x$pop_ideology_direction)), "pop_ideology_direction non-missing"), "count")
add("numArDocsLla",  ar_derived(function(x) sum(x$milei == 1), "milei == 1"), "count")
AR_R <- c(World = "World", `Latin America` = "LatAm", Argentina = "Arg")
AR_SG <- c(all = "All", right = "Right", right_populist = "RightPop")
for (r in names(AR_R)) for (s in names(AR_SG)) for (v in c("populism_overall", "IDEO", names(AR_O)[3:7])) {
  vv <- if (v == "IDEO") "Ideo" else AR_O[[v]]
  val <- pick(AR_G, "mean", region = r, subgroup = s, outcome = v)
  # Argentina has no right or right-populist baseline party (n = 0, mean NA): no mean macro in either mode
  has_mean <- !(r == "Argentina" && s != "all")
  if (!PIPE_PUBLIC) stopifnot(is.finite(val) == has_mean)
  if (has_mean) add(paste0("numArMean", vv, AR_R[[r]], AR_SG[[s]]), val, "r")
  nn <- pick(AR_G, "n", region = r, subgroup = s, outcome = v)
  if (v == "populism_overall") add(paste0("numArN", AR_R[[r]], AR_SG[[s]]), nn, "count")
}
for (e in names(AR_DOCS)) for (v in names(AR_O)[3:7]) {
  add(paste0("numLlaPred", AR_O[[v]], e),   pick(AR_P, "pred_interaction", doc_id = AR_DOCS[[e]], outcome = v), "r")
  add(paste0("numLlaResid", AR_O[[v]], e),  pick(AR_P, "resid_interaction", doc_id = AR_DOCS[[e]], outcome = v), "r")
  add(paste0("numLlaResidSd", AR_O[[v]], e), pick(AR_P, "resid_interaction_sd", doc_id = AR_DOCS[[e]], outcome = v), "r")
}

# ---- Write --------------------------------------------------------------------------------------------------------
out <- do.call(rbind, unname(reg))
write_tex_macros <- function(o, file, what) write_atomic(c(
  sprintf("%% %s -- generated by R/46_Numbers.R from the v2026a chain outputs; do not edit by hand.", basename(file)),
  sprintf("%% Register with source file and field of every macro: csv/%s. Percent macros (...Pct) carry no %% sign.", what),
  if (grepl("public", file)) "% Public version: macros from restricted chain outputs show the placeholder n/a (restricted).",
  sprintf("\\providecommand{\\%s}{%s}", o$macro, o$formatted)), file, writer = function(x, p) writeLines(x, p))
pub <- out
pub$value[!pub$public] <- NA_real_; pub$formatted[!pub$public] <- PLACEHOLDER; pub$source_field[!pub$public] <- ""
write_csv_det(pub, file.path(PIPE_OUT, "csv", "46_numbers_public.csv"))
write_tex_macros(pub, file.path(PIPE_OUT, "numbers_public.tex"), "46_numbers_public.csv")
if (!PIPE_PUBLIC) {
  write_csv_det(out, file.path(PIPE_OUT, "csv", "46_numbers.csv"))
  write_tex_macros(out, file.path(PIPE_OUT, "numbers.tex"), "46_numbers.csv")
}
message(sprintf("R/46: %d macros (%d from restricted outputs) -> %s", nrow(out), sum(!out$public), PIPE_OUT))
