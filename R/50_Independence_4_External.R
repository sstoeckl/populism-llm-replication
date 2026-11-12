# ============================================================
# 50_Independence_4_External.R
#
# TEST 4 — External (expert-coded) populism measure + ideology controls.
#
# PopuList 4.0 (Rooduijn et al.; popu-list.org) classifies European
# parties (>= 2% vote or >= 1 seat, 31 countries, 1989+) as populist /
# far-right / far-left with start and end years. Parties not listed in a
# covered country are non-populist by construction. Mapping PopuList ->
# CMP party ids via Partyfacts (Döring & Regel 2019) external-parties file.
# Both files are public; cached under output/independence/cache/external/.
#
# Regressions (country + year FE, SE clustered by party):
#   (a) LLM liberalism ~ PopuList populist         (X independent of LLM)
#   (b) CMP liberalism ~ PopuList populist         (no LLM at all)
#   (c) as (a)/(b) + PopuList far-right / far-left (ideology partialled out)
#       and + CMP RILE (hand-coded left-right)
#   (d) LLM populism -> LLM liberalism among PopuList NON-populist
#       parties only (does the gradient exist outside populist parties?)
#
# Review batch 2026-11-11:
#   PLLM-0255  tab:indep:popvalid counts only manifestos with a populism score (as its AUC and as the
#              AUC row of Table 3, R/44): 295 / 1,287. It counted all 1,594 manifestos of the PopuList
#              sample (297 / 1,297) although 12 of them have no populism score. The regressions of
#              tab:indep:populist do not need a populism score; their N is the number of manifestos
#              with the outcome (and the controls), e.g. 1,587 for overall liberalism.
#   PLLM-0243  tab:indep:populist gets the within-group rows (d) for the five LLM liberalism measures:
#              LLM populism within PopuList populist and within non-populist parties.
# ============================================================

source("R/50_Independence_0_Setup.R")
suppressPackageStartupMessages(library(modelsummary))
options(modelsummary_format_numeric_latex = "plain")

EXT <- IND_EXT
dir.create(EXT, showWarnings = FALSE, recursive = TRUE)
if (IND_BASE == "v2026a") stopifnot("archived external CSVs missing" =
                                      file.exists(file.path(EXT, c("populist4.csv", "partyfacts_external.csv"))))
f_pl <- file.path(EXT, "populist4.csv"); f_pf <- file.path(EXT, "partyfacts_external.csv")
if (!file.exists(f_pl)) download.file("https://popu-list.github.io/Data/The%20PopuList%204.0.csv", f_pl, quiet = TRUE)
if (!file.exists(f_pf)) download.file("https://partyfacts.herokuapp.com/download/external-parties-csv/", f_pf, quiet = TRUE)

pl <- read_delim(f_pl, delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8")) |>
  filter(!is.na(partyfacts_id))
pf <- read_csv(f_pf, show_col_types = FALSE, guess_max = 1e5) |>
  filter(dataset_key == "manifesto") |>
  transmute(party = as.numeric(dataset_party_id), partyfacts_id)

base <- load_analysis_base()
POPULIST_COUNTRIES <- unique(pl$country_name)
pl_flags <- base |>
  select(doc_id, party, year) |>
  left_join(pf, by = "party", relationship = "many-to-many") |>
  left_join(pl |> select(partyfacts_id, populist, populist_start, populist_end, farright, farright_start,
                         farright_end, farleft, farleft_start, farleft_end),
            by = "partyfacts_id", relationship = "many-to-many") |>
  # PopuList semantics (2026-10-26, chain-review #13): the start/end window is the classified
  # period (1900-2100 = always, 2100-2100 = never); the 0/1 flag is the CURRENT status only (e.g.
  # BZO: farright = 0, window 1900-2008). Classification in the election year = inside the window.
  mutate(pl_populist = coalesce(year >= populist_start & year <= populist_end, FALSE),
         pl_farright = coalesce(year >= farright_start & year <= farright_end, FALSE),
         pl_farleft  = coalesce(year >= farleft_start & year <= farleft_end, FALSE)) |>
  group_by(doc_id) |>
  summarise(across(c(pl_populist, pl_farright, pl_farleft), ~ as.numeric(any(.x))), .groups = "drop")

# PopuList coverage: its countries, elections 1989+
base_pl <- base |>
  mutate(cn = if_else(countryname == "Czech Republic", "Czechia", countryname)) |>
  filter(cn %in% POPULIST_COUNTRIES | countryname %in% POPULIST_COUNTRIES, year >= 1989) |>
  left_join(pl_flags, by = "doc_id")
message("PopuList countries not matched in CMP: ",
        paste(setdiff(POPULIST_COUNTRIES, c(unique(base$countryname), "Czechia")), collapse = ", "))

message(sprintf("PopuList sample: %d manifestos, %d countries, populist = %d (%.1f%%)",
                nrow(base_pl), n_distinct(base_pl$countryname), sum(base_pl$pl_populist),
                100 * mean(base_pl$pl_populist)))

# Convergent validity of LLM populism vs PopuList
val <- base_pl |>
  filter(is.finite(populism_overall)) |>          # PLLM-0255: the sample of the AUC (and of Table 3)
  group_by(pl_populist) |>
  summarise(n = n(), mean_llm_pop = mean(populism_overall, na.rm = TRUE),
            sd_llm_pop = sd(populism_overall, na.rm = TRUE), mean_cmp_pop = mean(CMP_POP, na.rm = TRUE))
auc <- with(base_pl |> filter(is.finite(populism_overall)), {
  r <- rank(populism_overall); n1 <- sum(pl_populist == 1); n0 <- sum(pl_populist == 0)
  (sum(r[pl_populist == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
})
val <- val |> mutate(AUC_llm_pop = auc)
write_tab(val, "t4_populist_validity", digits = 3, label = "tab:indep:popvalid",
          caption = "LLM populism by PopuList classification",
          note = "Manifestos with a populism score of the PopuList countries from 1989 on (the sample of the AUC in Table 3); parties not on the list are non-populist. PopuList 4.0 matched via Partyfacts, classified for the election year. AUC: probability that a populist manifesto scores higher on LLM populism than a non-populist one.")
print(val)

# ---- Regressions ----
DVS <- c("liberalism_overall", "lib_political", "lib_social", "lib_economic", "lib_financial_market",
         "CMP_LIB", "CMP_POL", "CMP_SOC", "CMP_ECON", "CMP_ECON_ordo")
reg <- function(d, dv, rhs, label, sample_label = "PopuList sample") {
  d <- d |> filter(is.finite(.data[[dv]]))
  d[[dv]] <- as.numeric(scale(d[[dv]]))          # DV in SD units; regressor is a 0/1 dummy
  m <- feols(as.formula(paste(dv, "~", rhs, "| countryname + year")), d, cluster = ~party)
  x <- strsplit(rhs, " \\+ ")[[1]][1]
  tibble(sample = sample_label, dv = dv, regressor = x, controls = label,
         estimate = coef(m)[[x]], se = se(m)[[x]], p = pvalue(m)[[x]], n = nobs(m))
}
res <- map_dfr(DVS, function(dv) bind_rows(
  reg(base_pl, dv, "pl_populist", "none"),
  reg(base_pl, dv, "pl_populist + pl_farright + pl_farleft", "+ PopuList far-right/far-left"),
  reg(base_pl |> filter(is.finite(CMP_rile_neg)), dv, "pl_populist + pl_farright + pl_farleft + CMP_rile_neg",
      "+ far-right/far-left + CMP RILE"),
  reg(base_pl |> filter(is.finite(pop_ideology_direction)), dv, "pl_populist + pl_farright + pl_farleft + pop_ideology_direction",
      "+ far-right/far-left + LLM ideology")
)) |> mutate(sig = stars(p))
# PLLM-0243: within-group slopes of LLM liberalism on LLM populism (both z-scored within the group;
# country and year FE; SE clustered by party), appended to tab:indep:populist
within_rows <- map_dfr(c(1, 0), function(g) map_dfr(LLM_LIB, function(dv) {
  m <- std_fe_reg(base_pl |> filter(pl_populist == g), dv, "populism_overall", cluster = ~party)
  tibble(sample = if (g == 1) "PopuList populist parties" else "PopuList non-populist parties", dv = dv,
         regressor = "populism_overall", controls = "within group", estimate = coef(m)[["populism_overall"]],
         se = se(m)[["populism_overall"]], p = pvalue(m)[["populism_overall"]], n = nobs(m))
})) |> mutate(sig = stars(p))
res <- bind_rows(res, within_rows)
write_tab(res, "t4_populist_regressions", digits = 3, label = "tab:indep:populist",
          caption = "Liberalism on the PopuList populist classification",
          note = paste("Outcome z-scored; country and year fixed effects; SE clustered by party.",
                       "Rows with sample PopuList sample: regressor = PopuList populist classification (0/1); controls added stepwise: PopuList far-right and far-left classifications, CMP rile, LLM ideology; N = manifestos with the outcome (and the controls), which need not have a populism score.",
                       "Rows with sample PopuList populist parties and PopuList non-populist parties: LLM populism (overall) as the regressor within each group, outcome and regressor z-scored within the group."))
print(res |> select(dv, controls, estimate, se, p, n), n = 60)

# (d) LLM populism gradient inside non-populist parties (and inside populist ones)
grad <- map_dfr(c(0, 1), function(g) map_dfr(c("liberalism_overall", "lib_political", "lib_social", "lib_economic",
                                               "CMP_LIB", "CMP_ECON"), function(dv) {
  d <- base_pl |> filter(pl_populist == g)
  m <- std_fe_reg(d, dv, "populism_overall", cluster = ~party)
  tidy_coef(m, "populism_overall") |>
    mutate(subsample = if (g == 1) "PopuList populist parties" else "PopuList non-populist parties",
           dv = dv, .before = 1)
}))
write_tab(grad, "t4_gradient_within_groups", digits = 3,
          caption = "Standardized LLM-populism slope within PopuList non-populist and populist parties.")
print(grad)

# Publication table
d_ms <- base_pl |> mutate(across(c(liberalism_overall, lib_political, lib_social, lib_economic,
                                   CMP_LIB, CMP_ECON, CMP_SOC), ~ as.numeric(scale(.x))))
mods <- list(
  "LLM Lib"  = feols(liberalism_overall ~ pl_populist + pl_farright + pl_farleft | countryname + year, d_ms, cluster = ~party),
  "LLM Pol"  = feols(lib_political ~ pl_populist + pl_farright + pl_farleft | countryname + year, d_ms, cluster = ~party),
  "LLM Soc"  = feols(lib_social ~ pl_populist + pl_farright + pl_farleft | countryname + year, d_ms, cluster = ~party),
  "LLM Econ" = feols(lib_economic ~ pl_populist + pl_farright + pl_farleft | countryname + year, d_ms, cluster = ~party),
  "CMP Lib"  = feols(CMP_LIB ~ pl_populist + pl_farright + pl_farleft | countryname + year, d_ms, cluster = ~party),
  "CMP Soc"  = feols(CMP_SOC ~ pl_populist + pl_farright + pl_farleft | countryname + year, d_ms, cluster = ~party),
  "CMP Econ" = feols(CMP_ECON ~ pl_populist + pl_farright + pl_farleft | countryname + year, d_ms, cluster = ~party))
modelsummary(mods, output = file.path(IND_TAB, "t4_populist_modelsummary.tex"),
             coef_map = c(pl_populist = "Populist (PopuList)", pl_farright = "Far right (PopuList)",
                          pl_farleft = "Far left (PopuList)"),
             gof_map = c("nobs", "r2.within"), stars = c("*" = .1, "**" = .05, "***" = .01),
             title = "Liberalism (z-scored) and expert-coded populism (PopuList 4.0), European parties 1989+. Country and year FE; SE clustered by party.")

saveRDS(list(val = val, res = res, grad = grad, n = nrow(base_pl)), file.path(IND_CACHE, "t4_results.rds"))
message("Test 4 done.")
