# ============================================================
# 50_Independence_0_Setup.R
#
# Shared setup for the populism-liberalism INDEPENDENCE tests
# (R/50_Independence_1..4). Addresses the editorial concern that
# populism and liberalism are scored in the SAME LLM call with
# partly overlapping definitions, so the negative association could
# be a halo / shared-method artefact.
#
# This file only builds the analysis base; it never touches the
# production pipeline or anything under data/, paper/, website/.
#
# Provides:
#   IND_DIR, IND_TAB, IND_FIG, IND_LOG, IND_CACHE   output folders
#   load_baseline()        manifesto_scores.parquet (HF, cached)
#   load_cmp_main()        CMP main dataset MPDS (manifestoR, cached)
#   build_cmp_indices()    hand-coded CMP liberalism / populism indices
#   CMP_INDEX_DEF          documented category mapping (tibble)
#   load_analysis_base()   baseline LLM scores x CMP indices, one row/manifesto
#   std_fe_reg()           standardized feols wrapper (country + year FE)
#   write_tab()            CSV + LaTeX (booktabs) writer
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(fixest); library(lubridate)
})

# IND_BASE = "june" (default) reproduces the 2026-10-07/08 run on the June baseline
# (output/independence/). IND_BASE = "v2026a" is the Phase-2 chain stage for the no-API
# tests 1, 2 and 4: v2026a baseline and raw files, local MPDS2026a, archived external CSVs,
# outputs in PIPE_OUT/validation/independence/. Test 3 (separate-call rescoring) stays as
# archived in output/independence/.
IND_BASE  <- Sys.getenv("IND_BASE", "june")
stopifnot(IND_BASE %in% c("june", "v2026a"))
if (IND_BASE == "v2026a") source("R/0_Config.R")
IND_DIR   <- if (IND_BASE == "v2026a") file.path(PIPE_OUT, "validation", "independence") else "output/independence/"
IND_EXT   <- "output/independence/cache/external"     # PopuList 4.0 + Partyfacts (archived public CSVs)
IND_TAB   <- file.path(IND_DIR, "tables")
IND_FIG   <- file.path(IND_DIR, "figures")
IND_LOG   <- file.path(IND_DIR, "logs")
IND_CACHE <- file.path(IND_DIR, "cache")
for (d in c(IND_DIR, IND_TAB, IND_FIG, IND_LOG, IND_CACHE))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

BASELINE_URL <- "https://huggingface.co/datasets/sstoeckl/populism-llm/resolve/main/data/manifesto_scores.parquet"

# ---- Baseline LLM scores (cross-model mean, cwm within) ----
load_baseline <- function(refresh = FALSE) {
  if (IND_BASE == "v2026a")
    return(read_parquet(file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet")) |> mutate(date = as.Date(date)))
  f <- file.path(IND_CACHE, "manifesto_scores.parquet")
  if (refresh || !file.exists(f)) download.file(BASELINE_URL, f, mode = "wb", quiet = TRUE)
  read_parquet(f) |> mutate(date = as.Date(date))
}

# ---- CMP main dataset (hand-coded quasi-sentence shares) ----
# MANIFESTO_KEY is read from .Renviron and never printed.
load_cmp_main <- function(refresh = FALSE) {
  if (IND_BASE == "v2026a") return(read_parquet(PIPE_MPDS))   # MPDS2026a, saved by R/30
  f <- file.path(IND_CACHE, "mp_maindataset.parquet")
  if (refresh || !file.exists(f)) {
    suppressPackageStartupMessages(library(manifestoR))
    mp_setapikey(key = Sys.getenv("MANIFESTO_KEY"))
    write_parquet(mp_maindataset(), f)
  }
  read_parquet(f)
}

# ---- CMP index definitions ----
#
# Scaling follows Lowe, Benoit, Mikhaylov & Laver (2011, LSQ): the
# empirical logit  theta = log(R + 0.5) - log(L + 0.5), where R and L are
# COUNTS of quasi-sentences (per * total / 100) in the pro- and anti-pole
# categories. Category selection follows the economic and GAL-TAN
# groupings of RILE (Laver & Budge 1992), Lowe et al. (2011) and
# Bakker & Hobolt (2013), mapped onto the paper's four liberalism
# dimensions. CMP has no financial-market category, so lib_financial_market
# has no hand-coded analogue.
#
# Variants are robustness checks on contested category assignments:
#   - per403 "Market Regulation" covers competition/consumer-protection
#     rules that the prompt treats as ORDO-liberal (pro-liberal); RILE and
#     Lowe treat it as left. ECON_ordo drops it.
#   - welfare (504/505) is in RILE but not in the prompt's lib_economic.
#   - per503 "Equality" is partly redistributive; SOC_core drops 503/705.
CMP_INDEX_DEF <- tribble(
  ~index,      ~dimension,  ~pro_liberal,                              ~anti_liberal,
  "CMP_ECON",  "economic",  "401,402,407,414",                         "403,404,406,412,413",
  "CMP_ECON_ordo", "economic", "401,402,407,414",                      "404,406,412,413",
  "CMP_ECON_welf", "economic", "401,402,407,414,505",                  "403,404,406,412,413,504",
  "CMP_POL",   "political", "201,202,203",                             "204,605",
  "CMP_SOC",   "social",    "503,604,607,705",                         "601,603,608",
  "CMP_SOC_core", "social", "604,607",                                 "603,608",
  "CMP_LIB",   "overall",   "401,402,407,414,201,202,203,503,604,607,705",
                            "403,404,406,412,413,204,605,601,603,608"
)

# CMP populism proxy. CMP has no populism category; the closest
# anti-elite signal is per304 "Political Corruption" (need to eliminate
# corruption and abuse of power) plus per110 "European Community/Union:
# Negative" (hostility to supranational elites). Neither category enters
# any liberalism index above, so there is NO mechanical overlap.
# This is a weak proxy, reported only as a supplementary check.
CMP_POP_CATS <- c("304", "110")

.cats <- function(s) paste0("per", trimws(strsplit(s, ",")[[1]]))

build_cmp_indices <- function(mpd) {
  out <- mpd |>
    transmute(party, date_num = as.integer(date), total, rile,
              across(starts_with("per") & !matches("^(pervote|peruncod)$")))
  cnt <- function(cols) {
    m <- as.matrix(out[, intersect(cols, names(out))])
    rowSums(m, na.rm = TRUE) * out$total / 100
  }
  shr <- function(cols) rowSums(as.matrix(out[, intersect(cols, names(out))]), na.rm = TRUE)
  for (i in seq_len(nrow(CMP_INDEX_DEF))) {
    R <- cnt(.cats(CMP_INDEX_DEF$pro_liberal[i]))
    L <- cnt(.cats(CMP_INDEX_DEF$anti_liberal[i]))
    out[[CMP_INDEX_DEF$index[i]]] <- log(R + 0.5) - log(L + 0.5)
  }
  # CMP_POL_v5: political index using the coding-scheme-5 sub-categories
  # (available only for manifestos coded with them, mostly 2014+): drops
  # 202.4 "Direct Democracy: Positive" (a populist demand) from the
  # pro-liberal pole and moves 202.2 "Democracy General: Negative" to the
  # anti-liberal pole.  R = 201 + 202.1 + 202.3 + 203;  L = 202.2 + 204 + 605.
  v5 <- !is.na(out$per202_4)
  R5 <- cnt(c("per201", "per202_1", "per202_3", "per203"))
  L5 <- cnt(c("per202_2", "per204", "per605"))
  out$CMP_POL_v5 <- ifelse(v5, log(R5 + 0.5) - log(L5 + 0.5), NA_real_)
  out$CMP_POL_v5same <- ifelse(v5, out$CMP_POL, NA_real_)   # CMP_POL on the same manifestos
  out$CMP_POP      <- shr(paste0("per", CMP_POP_CATS))   # % of quasi-sentences
  out$CMP_POP_304  <- out$per304
  out$CMP_rile_neg <- -out$rile                          # higher = more left
  out |>
    mutate(across(c(starts_with("CMP_")), ~ ifelse(is.na(total) | total <= 0, NA_real_, .x))) |>
    select(party, date_num, total, rile, starts_with("CMP_"))
}

# ---- Analysis base: LLM baseline x CMP indices ----
LLM_LIB  <- c("liberalism_overall", "lib_political", "lib_social", "lib_economic", "lib_financial_market")
LLM_POP  <- c("populism_overall", "pop_anti_elitism", "pop_people_centrism", "pop_manichean")

load_analysis_base <- function() {
  s   <- load_baseline()
  cmp <- build_cmp_indices(load_cmp_main()) |> select(-rile)
  s |>
    mutate(date_num = as.integer(format(date, "%Y%m"))) |>
    left_join(cmp, by = c("party", "date_num"))
}

# ---- Standardized FE regression ----
# z-scores the DV and focal regressor on the estimation sample so that
# coefficients are comparable across DVs measured on different scales
# (LLM 0-10 vs CMP logit). Returns the fixest object.
std_fe_reg <- function(dat, dv, x, controls = NULL, fe = "countryname + year",
                       cluster = ~party, standardize = TRUE) {
  vars <- c(dv, x, controls)
  d <- dat |> filter(if_all(all_of(vars), is.finite))
  if (standardize) d <- d |> mutate(across(all_of(c(dv, x)), ~ as.numeric(scale(.x))))
  rhs <- paste(c(x, controls), collapse = " + ")
  feols(as.formula(paste(dv, "~", rhs, "|", fe)), data = d, cluster = cluster)
}

tidy_coef <- function(m, term) {
  ct <- coeftable(m)
  tibble(estimate = ct[term, 1], se = ct[term, 2], t = ct[term, 3], p = ct[term, 4],
         n = nobs(m), within_r2 = as.numeric(fitstat(m, "wr2")))
}

# ---- Table writer: CSV + booktabs LaTeX ----
# Paper-ready tables (2026-10-27): full table environment with caption and \label, readable row and
# column labels (IND_LABELS; unknown names: underscores become spaces), booktabs, LaTeX-escaped
# cells, the tabular in a guarded \resizebox (shrinks only if wider than \linewidth; graphicx),
# the note below the tabular. Integer columns are printed with thousands separators.
IND_LABELS <- c(
  liberalism_overall = "Liberalism (overall)", lib_political = "Political liberalism", lib_social = "Social liberalism",
  lib_economic = "Economic liberalism", lib_financial_market = "Financial-market liberalism",
  populism_overall = "Populism (overall)", pop_anti_elitism = "Anti-elitism", pop_people_centrism = "People-centrism",
  pop_manichean = "Manichaean", pop_ideology_direction = "Ideology",
  CMP_LIB = "CMP liberalism", CMP_POL = "CMP political", CMP_SOC = "CMP social", CMP_SOC_core = "CMP social (core)",
  CMP_ECON = "CMP economic", CMP_ECON_ordo = "CMP economic (market)", CMP_ECON_welf = "CMP economic (welfare)",
  CMP_rile_neg = "CMP rile (reversed)", CMP_POP = "CMP populism",
  sonnet = "Sonnet 4.6", gpt41mini = "GPT-4.1-mini", gemini = "Gemini 3.5 Flash",
  llm = "LLM score", cmp = "CMP index", b_llm = "Slope, LLM", b_cmp = "Slope, CMP", se_cmp = "SE, CMP",
  r_within_cy = "r within country-year", b_cmp_expected_no_halo = "Expected CMP slope, no halo",
  ratio_observed = "CMP / LLM slope", share_of_expected = "Observed / expected",
  # bootstrap CIs of tab:indep:cmp (PLLM-0249, 2026-11-11)
  ratio_ci_lo = "95\\% CI, low", ratio_ci_hi = "95\\% CI, high", share_ci_lo = "95\\% CI, low", share_ci_hi = "95\\% CI, high",
  B = "B",
  estimate = "Estimate", se = "SE", t = "t", p = "p", n = "N", within_r2 = "Within $R^2$", se_country = "SE (country)",
  p_country = "p (country)", sig = "", block = "Block", x = "Regressor", dv = "Outcome", controls = "Controls",
  pair_type = "Dimension pair", model = "Model", n_pairs = "Pairs", pct_identical = "Identical (\\%)",
  pct_nested = "Nested (\\%)", "pct_jaccard_ge_.5" = "Jaccard $\\ge$ 0.5 (\\%)", pct_quote_in_other_ctx = "Quote in other context (\\%)",
  pct_span_overlap = "Span overlap (\\%)", pct_within_300ch = "Within 300 characters (\\%)",
  pct_within_300ch_null = "Within 300 characters, random pairs (\\%)",
  level = "Level", mean_r_heteromethod = "Mean $r$, across LLMs", mean_r_monomethod = "Mean $r$, same LLM",
  min_r_heteromethod = "Min $r$, across", min_r_monomethod = "Min $r$, same", max_r_heteromethod = "Max $r$, across",
  max_r_monomethod = "Max $r$, same", rel_pop = "Reliability, POP", rel_lib = "Reliability, LIB",
  pop_model = "Populism scored by", lib_model = "Liberalism scored by", same_model = "Same LLM",
  b_raw = "Estimate (raw)", se_raw = "SE (raw)", pl_populist = "PopuList populist", mean_llm_pop = "Mean LLM populism",
  sd_llm_pop = "SD LLM populism", mean_cmp_pop = "Mean CMP populism", AUC_llm_pop = "AUC", sample = "Sample",
  regressor = "Regressor", subsample = "Subsample", "TRUE" = "yes", "FALSE" = "no",
  # leave-one-out rows of tab:indep:crossrater (PLLM-0240, 2026-11-11)
  "gpt41mini+gemini" = "GPT-4.1-mini + Gemini 3.5 Flash", "sonnet+gemini" = "Sonnet 4.6 + Gemini 3.5 Flash",
  "sonnet+gpt41mini" = "Sonnet 4.6 + GPT-4.1-mini")
tex_escape <- function(x) {
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub(">=", "$\\ge$", x, fixed = TRUE); x <- gsub("<=", "$\\le$", x, fixed = TRUE)
  x <- gsub("->", "$\\rightarrow$", x, fixed = TRUE)
  x
}
ind_label <- function(x) { x <- as.character(x); out <- unname(IND_LABELS[x]); k <- is.na(out)
  out[k] <- tex_escape(x[k]); out }   # dictionary labels are already LaTeX
write_tab <- function(df, name, caption = NULL, digits = 3, note = NULL, label = paste0("tab:indep:", name)) {
  # v2026a chain stage: deterministic CSV (10 significant digits; numeric certification level of R/71)
  if (IND_BASE == "v2026a") write_csv_det(df, file.path(IND_TAB, paste0(name, ".csv"))) else
    write_csv(df, file.path(IND_TAB, paste0(name, ".csv")))
  df <- as.data.frame(df)
  num <- vapply(df, is.numeric, logical(1))
  cells <- lapply(seq_along(df), function(j) {
    v <- df[[j]]
    if (is.numeric(v)) {
      int <- all(is.na(v) | v == round(v)) && !grepl("^(p|t|r|estimate|se)$|^(b_|se_|p_|r_|mean|sd|share|ratio|rel_)", names(df)[j])
      out <- if (int) formatC(v, format = "d", big.mark = ",") else formatC(v, format = "f", digits = digits)
      out <- sub("^-", "$-$", out); out[is.na(v)] <- ""; out
    } else { out <- ind_label(v); out[is.na(v)] <- ""; out }
  })
  body <- do.call(paste, c(cells, sep = " & "))
  tex <- c(sprintf("%% generated by R/50 (%s); csv: %s.csv", IND_BASE, name),
           "\\begin{table}[ht]", if (!is.null(caption)) sprintf("\\caption{%s}", tex_escape(caption)),
           sprintf("\\label{%s}", label), "\\footnotesize", "\\centering",
           "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%",
           sprintf("\\begin{tabular}{%s}", paste(ifelse(num, "r", "l"), collapse = "")), "\\toprule",
           paste0(paste(ind_label(names(df)), collapse = " & "), " \\\\"), "\\midrule",
           paste0(body, " \\\\"), "\\bottomrule", "\\end{tabular}}",
           if (!is.null(note)) c("\\par\\smallskip", sprintf("\\begin{minipage}{\\linewidth}\\scriptsize %s\\end{minipage}", tex_escape(note))),
           "\\end{table}")
  writeLines(tex, file.path(IND_TAB, paste0(name, ".tex")))
  invisible(df)
}

stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")
