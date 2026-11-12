# ============================================================
# 44_Ideology_Diagnosis.R
#
# Why do the paper's ideology (IDEO) results fail on v2026a? No API calls.
#   1. Bridge: from the printed Nov-2025 Tables 4-8 (gpt-4o-mini, data/CMP/20251111-poplib_results.RData,
#      aggregated exactly as R/_archive/4_Stats.R) to the v2026a tables, switching one ingredient
#      at a time (cumulative): old -> v2026a POP/LIB scores; old -> new IDEO; iid -> party-
#      clustered SE; party FE by name -> CMP party id; old -> new sample. Each failing claim is
#      evaluated at every step; single-ingredient switches from the printed setup attribute it.
#   2. IDEO measurement quality: per-model NA shares, agreement and ICC for L, R, C and IDEO;
#      instability of IDEO = (R-L)/(R+L) (rev3; until rev2 (R-L)/(R+L+C)) when the denominator is
#      small; alternative constructions incl. the rev2 construction IDEO_c.
#   3. External validity: CMP rile, Lowe logit rile, CHES lrgen/lrecon/galtan, ParlGov
#      left_right, PopuList far-left/far-right (sources and versions in data/external/SOURCES.csv).
#   4. Concept: verbatim L/R/C prompt text (R/3c and the old R/3), POP-IDEO overlap, VIF, joint
#      distribution and the support of the POP x IDEO interaction.
#   5. Tables 4-8 IDEO models and interactions re-estimated with the external ideology measures;
#      5a (2026-11-11, PLLM-0250): ideology level effects of column (4) with CHES lrgen / lrecon /
#      galtan, ParlGov and CMP rile -> tables/oa_ideo_levels_external.tex (tab:oa2k),
#      csv/44_ideo_levels_external.csv.
#   6. PIPE_OUT/IDEOLOGY_DIAGNOSIS.md (paper tickets) + csv / tex / figure outputs.
# ============================================================

suppressPackageStartupMessages({ library(tidyverse); library(arrow); library(fixest) })
source("R/0_Config.R")
source("R/1_Aggregation.R")
setFixest_nthreads(1)
setFixest_notes(FALSE)
CSV <- file.path(PIPE_OUT, "csv"); FIG <- file.path(PIPE_OUT, "figures"); TAB <- file.path(PIPE_OUT, "tables")
f2 <- function(x) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = 2))
f3 <- function(x) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = 3))
fp <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
stars <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
# display labels for the LaTeX tables (csv files keep the codes)
DV_TEX <- c(liberalism_overall = "Liberalism (overall)", lib_political = "Liberalism (political)", lib_social = "Liberalism (social)",
            lib_economic = "Liberalism (economic)", lib_financial_market = "Liberalism (financial markets)")
tex_lbl <- function(x) { x <- gsub("left_right", "left-right", x, fixed = TRUE); gsub("([&%$#_{}])", "\\\\\\1", x) }
DVS <- c(liberalism_overall = 4, lib_political = 5, lib_social = 6, lib_economic = 7, lib_financial_market = 8)
# frozen copy of paper/R0/main_v2.tex (R/48), so paper edits never change this stage
TEX <- enc2utf8(read.csv(PIPE_PAPER_LINES, stringsAsFactors = FALSE, encoding = "UTF-8")$text)
line_of <- function(pattern) { k <- grep(pattern, TEX, fixed = TRUE)[1]; if (is.na(k)) NA_integer_ else k }

# ---- 0. Data -------------------------------------------------------------------------------
new <- read_parquet(file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet")) |>
  mutate(IDEO = pop_ideology_direction, date = as.Date(date))

# Old (Nov 2025) scores, aggregated exactly as R/_archive/4_Stats.R l.95-133
e <- new.env(); load("data/CMP/20251111-poplib_results.RData", envir = e)
dat_old <- read_parquet("data/CMP/full_data.parquet",
                        col_select = c("doc_id", "manifesto_id", "text", "date", "countryname", "party", "partyname")) |>
  mutate(date = as.Date(date)) |> filter(!is.na(manifesto_id), !is.na(text)) |> select(-text, -manifesto_id)
old <- e$results |> map("raw") |> bind_rows() |>
  group_by(doc_id_root) |>
  summarise(across(c(populism_overall, pop_anti_elitism, pop_people_centrism, pop_manichean),
                   ~ weighted.mean(.x, populism_confidence, na.rm = TRUE)),
            across(c(liberalism_overall, lib_political, lib_social, lib_economic, lib_financial_market),
                   ~ weighted.mean(.x, liberalism_confidence, na.rm = TRUE)),
            across(c(pop_ideology_overall, pop_ideology_left, pop_ideology_right, pop_ideology_centrist),
                   ~ weighted.mean(.x, pop_ideology_confidence, na.rm = TRUE)),
            .groups = "drop") |>
  mutate(IDEO = (pop_ideology_right - pop_ideology_left) / (pop_ideology_right + pop_ideology_left + pop_ideology_centrist)) |>
  left_join(dat_old, by = c("doc_id_root" = "doc_id")) |>
  rename(doc_id = doc_id_root) |> mutate(year = as.integer(format(date, "%Y"))) |> arrange(doc_id)
rm(e); invisible(gc())

# ---- 1. Bridge -----------------------------------------------------------------------------
LIBV <- names(DVS)
fit_cells <- function(data, dv, vc, pfe) {
  f <- function(rhs, fe) as.formula(paste(dv, "~", rhs, "|", fe))
  ms <- list(m1 = f("populism_overall", "countryname + year"), m2 = f("IDEO", "countryname + year"),
             m3 = f("populism_overall + IDEO", "countryname"), m4 = f("populism_overall + IDEO", "countryname + year"),
             m5 = f("populism_overall * IDEO", "countryname + year"),
             m6 = f("populism_overall + IDEO", paste("countryname + year +", pfe)))
  imap_dfr(ms, function(fm, mn) {
    m <- feols(fm, data = data)
    ct <- coeftable(summary(m, vcov = vc))
    tibble(model = mn, term = rownames(ct), estimate = ct[, 1], se = ct[, 2], p = ct[, 4], n = nobs(m))
  })
}
new_on_old <- old |> select(doc_id, countryname, party, partyname, year) |>
  left_join(new |> select(doc_id, all_of(c("populism_overall", LIBV)), IDEO_new = IDEO), by = "doc_id")
steps <- list(
  S0 = list(lbl = "printed setup: old scores, old IDEO, iid SE, party FE by name, old sample",
            data = old, vc = "iid", pfe = "partyname"),
  S1 = list(lbl = "+ v2026a POP and LIB scores", vc = "iid", pfe = "partyname",
            data = new_on_old |> left_join(old |> select(doc_id, IDEO), by = "doc_id")),
  S2 = list(lbl = "+ v2026a IDEO", vc = "iid", pfe = "partyname",
            data = new_on_old |> mutate(IDEO = IDEO_new)),
  S3 = list(lbl = "+ party-clustered SE", vc = ~party, pfe = "partyname",
            data = new_on_old |> mutate(IDEO = IDEO_new)),
  S4 = list(lbl = "+ party FE by CMP party id", vc = ~party, pfe = "party",
            data = new_on_old |> mutate(IDEO = IDEO_new)),
  S5 = list(lbl = "+ v2026a sample (= v2026a tables)", vc = ~party, pfe = "party", data = new),
  # single switches from the printed setup (attribution)
  X_ideo = list(lbl = "printed setup, only IDEO switched to v2026a", vc = "iid", pfe = "partyname",
                data = old |> select(-IDEO) |> left_join(new |> select(doc_id, IDEO), by = "doc_id")),
  X_scores = list(lbl = "printed setup, only POP/LIB switched to v2026a", vc = "iid", pfe = "partyname",
                  data = new_on_old |> left_join(old |> select(doc_id, IDEO), by = "doc_id")),
  X_se = list(lbl = "printed setup, only SE party-clustered", vc = ~party, pfe = "partyname", data = old))
bridge <- imap_dfr(steps, function(s, k) map_dfr(LIBV, function(dv) fit_cells(s$data, dv, s$vc, s$pfe) |>
                                                  mutate(step = k, dv = dv, .before = 1))) |>
  left_join(tibble(step = names(steps), step_label = map_chr(steps, "lbl")), by = "step")
write_csv_det(bridge, file.path(CSV, "44_bridge_cells.csv"))
cors_step <- imap_dfr(steps, function(s, k) map_dfr(LIBV, function(dv) {
  ok <- is.finite(s$data$IDEO) & is.finite(s$data[[dv]])
  tibble(step = k, dv = dv, r_ideo = cor(s$data$IDEO[ok], s$data[[dv]][ok]), n = sum(ok))
}))
write_csv_det(cors_step, file.path(CSV, "44_bridge_ideo_correlations.csv"))

# Check S0 against the printed tables (iid t-stats and coefficients)
printed <- map_dfr(names(DVS), function(dv) {
  i0 <- line_of(sprintf("\\label{tab:%d}", DVS[[dv]])); blk <- TEX[i0:(i0 + 25)]
  get <- function(pat, term) { k <- grep(pat, blk)[1]; cells <- str_split(blk[k], "&")[[1]][-1]
    tibble(dv = dv, model = paste0("m", seq_along(cells)), term = term, line = i0 + k - 1L,
           printed = suppressWarnings(as.numeric(str_match(cells, "\\{(-?[0-9.]+)\\}")[, 2]))) }
  bind_rows(get("^Populism Overall", "populism_overall"), get("^Populist Ideology", "IDEO"),
            get("^Populism:Ideology", "populism_overall:IDEO")) |> filter(!is.na(printed))
}) |> mutate(term = recode(term, "populism_overall:IDEO" = "populism_overall:IDEO"))
s0chk <- printed |> left_join(bridge |> filter(step == "S0") |> select(dv, model, term, estimate), by = c("dv", "model", "term"))
S0_MAXDIFF <- max(abs(s0chk$printed - s0chk$estimate), na.rm = TRUE)
stopifnot("the printed setup must reproduce the printed Tables 4-8 (3 decimals)" = S0_MAXDIFF < 0.0005)
write_csv_det(s0chk, file.path(CSV, "44_bridge_S0_vs_printed.csv"))

# Claims (IDEO-related) evaluated at every step
cell <- function(st, dv, m, term) bridge |> filter(step == st, dv == !!dv, model == m, term == !!term)
neg_sig_count <- function(st, dv) bridge |> filter(step == st, dv == !!dv, term == "IDEO", model %in% paste0("m", 2:6)) |>
  summarise(k = sum(estimate < 0 & p < .05)) |> pull(k)
inter <- function(st, dv) cell(st, dv, "m5", "populism_overall:IDEO")
CLAIMS <- tribble(
  ~id, ~line_pat, ~claim, ~test,
  "l195", "our ideology variable is also not systematically correlated", "IDEO not correlated with the liberalism dimensions (|r| < 0.3 for all five)",
    function(st) all(abs(cors_step$r_ideo[cors_step$step == st]) < 0.3),
  "l346", "Populist ideology is also negative and statistically significant in most models, testifying", "Tab. 4: IDEO negative and significant (5%) in most of models (2)-(6)",
    function(st) neg_sig_count(st, "liberalism_overall") >= 3,
  "l385_ideo", "Populist ideology is also negative and statistically significant in most models. Figure", "Tab. 5: IDEO negative and significant in most models",
    function(st) neg_sig_count(st, "lib_political") >= 3,
  "l422_ideo", "Populist ideology is also negative and statistically significant in all models, with", "Tab. 6: IDEO negative and significant in all models",
    function(st) neg_sig_count(st, "lib_social") == 5,
  "l459_ideo", "Populist ideology is again negative and statistically significant in most models", "Tab. 7: IDEO negative and significant in most models",
    function(st) neg_sig_count(st, "lib_economic") >= 3,
  "l496_ideo", "Populist ideology is also negative and statistically significant in all models, while", "Tab. 8: IDEO negative and significant in all models",
    function(st) neg_sig_count(st, "lib_financial_market") == 5,
  "l375", "Differences between both extreme ends are, in fact, not statistically significant", "Overall: POP x IDEO not significant",
    function(st) inter(st, "liberalism_overall")$p >= .05,
  "l385_int", "where differences between both extreme ends are also not statistically significant", "Political: POP x IDEO not significant",
    function(st) inter(st, "lib_political")$p >= .05,
  "l422_int", "now marginally significant", "Social: POP x IDEO negative, (marginally) significant",
    function(st) inter(st, "lib_social")$estimate < 0 & inter(st, "lib_social")$p < .10,
  "l459_int", "nowhere near statistical significance", "Economic: POP x IDEO nowhere near significance (p >= 0.10)",
    function(st) inter(st, "lib_economic")$p >= .10,
  "l496_int", "on the ideological scale are now statistically significant", "Financial: POP x IDEO positive and significant",
    function(st) inter(st, "lib_financial_market")$estimate > 0 & inter(st, "lib_financial_market")$p < .05)
ST <- names(steps)
claim_steps <- CLAIMS |> mutate(line = map_int(line_pat, line_of)) |>
  mutate(res = map(test, function(t) map_lgl(ST, t) |> set_names(ST))) |>
  select(id, line, claim, res) |> unnest_wider(res)
claim_steps <- claim_steps |>
  mutate(dies_at = pmap_chr(across(all_of(paste0("S", 0:5))), function(...) {
    v <- c(...); if (!v[1]) "false in the printed table already" else {
      k <- which(!v)[1]; if (is.na(k)) "survives" else names(v)[k] } }))
write_csv_det(claim_steps, file.path(CSV, "44_bridge_claims.csv"))

# ---- 2. IDEO measurement quality ----------------------------------------------------------
LRC <- c(L = "pop_ideology_left", R = "pop_ideology_right", C = "pop_ideology_centrist")
pm <- map_dfr(AGG_MODELS, function(m) new |> transmute(doc_id, model = m,
  L = .data[[paste0(LRC[["L"]], "_", m)]], R = .data[[paste0(LRC[["R"]], "_", m)]],
  C = .data[[paste0(LRC[["C"]], "_", m)]], IDEO = ideo_direction(R, L)))
na_tab <- pm |> group_by(model) |>
  summarise(across(c(L, R, C, IDEO), ~ mean(is.na(.x)), .names = "na_{.col}"), n = n(), .groups = "drop") |>
  bind_rows(new |> summarise(model = "cross_model", na_L = mean(is.na(pop_ideology_left)), na_R = mean(is.na(pop_ideology_right)),
                             na_C = mean(is.na(pop_ideology_centrist)), na_IDEO = mean(is.na(IDEO)), n = n()))
k_models <- new |> count(k = n_models_pop_ideology_right, name = "n_docs") |> mutate(var = "R")
icc_twoway <- function(mat) {
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]; n <- nrow(mat); k <- ncol(mat)
  if (n < 3) return(c(n = n, icc_a1 = NA, icc_c1 = NA))
  gm <- mean(mat); msr <- k * sum((rowMeans(mat) - gm)^2) / (n - 1); msc <- n * sum((colMeans(mat) - gm)^2) / (k - 1)
  mse <- sum((mat - outer(rowMeans(mat), rep(1, k)) - outer(rep(1, n), colMeans(mat)) + gm)^2) / ((n - 1) * (k - 1))
  c(n = n, icc_a1 = (msr - mse) / (msr + (k - 1) * mse + k * (msc - mse) / n), icc_c1 = (msr - mse) / (msr + (k - 1) * mse))
}
agree <- map_dfr(c("L", "R", "C", "IDEO"), function(v) {
  w <- pm |> select(doc_id, model, all_of(v)) |> pivot_wider(names_from = model, values_from = all_of(v))
  pr <- map_dfr(list(c("sonnet", "gpt41mini"), c("sonnet", "gemini"), c("gpt41mini", "gemini")), function(p) {
    ok <- is.finite(w[[p[1]]]) & is.finite(w[[p[2]]])
    tibble(pair = paste(p, collapse = "_"), n = sum(ok), r = cor(w[[p[1]]][ok], w[[p[2]]][ok]))
  })
  ic <- icc_twoway(as.matrix(w[, AGG_MODELS]))
  bind_rows(pr, tibble(pair = "all_three_icc_a1", n = ic[["n"]], r = ic[["icc_a1"]])) |> mutate(var = v, .before = 1)
})
# Denominator instability (rev3: denominator R + L)
den <- new |> mutate(den = pop_ideology_right + pop_ideology_left) |> filter(is.finite(IDEO))
den <- den |> mutate(d_band = cut(den, c(-Inf, 3, 6, 9, 12, Inf), labels = c("<=3", "3-6", "6-9", "9-12", ">12")),
                     # IDEO change when R moves by +1 point (sensitivity of the ratio)
                     dIDEO_R1 = ideo_direction(pop_ideology_right + 1, pop_ideology_left) - IDEO)
# leave-one-model-out IDEO vs full IDEO
loo <- map_dfr(AGG_MODELS, function(m) {
  others <- setdiff(AGG_MODELS, m)
  x <- pm |> filter(model %in% others) |> group_by(doc_id) |>
    summarise(across(c(L, R, C), ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)), .groups = "drop") |>
    mutate(IDEO_loo = ideo_direction(R, L), drop = m)
  x |> select(doc_id, drop, IDEO_loo)
}) |> inner_join(den |> select(doc_id, IDEO, d_band), by = "doc_id") |> filter(is.finite(IDEO_loo))
den_tab <- den |> group_by(d_band) |>
  summarise(n = n(), mean_abs_IDEO = mean(abs(IDEO)), mean_dIDEO_R1 = mean(abs(dIDEO_R1)), .groups = "drop") |>
  left_join(loo |> group_by(d_band) |> summarise(mad_loo = mean(abs(IDEO_loo - IDEO)), .groups = "drop"), by = "d_band")
den_q <- quantile(den$den, c(.05, .25, .5, .75, .95))
# Alternative constructions
alt <- new |> mutate(
  IDEO_diff  = (pop_ideology_right - pop_ideology_left) / 10,
  IDEO_logit = log(pop_ideology_right + 0.5) - log(pop_ideology_left + 0.5),
  IDEO_c     = ideo_direction_c(pop_ideology_right, pop_ideology_left, pop_ideology_centrist))   # construction until rev2
alt_cor <- cor(alt[, c("IDEO", "IDEO_diff", "IDEO_logit", "IDEO_c")], use = "pairwise.complete.obs")
write_csv_det(na_tab, file.path(CSV, "44_ideo_na_shares.csv"))
write_csv_det(agree, file.path(CSV, "44_ideo_agreement.csv"))
write_csv_det(den_tab, file.path(CSV, "44_ideo_denominator.csv"))
write_csv_det(as_tibble(alt_cor, rownames = "measure"), file.path(CSV, "44_ideo_alt_constructions_cor.csv"))
old_new_ideo <- inner_join(old |> select(doc_id, IDEO_old = IDEO), new |> select(doc_id, IDEO), by = "doc_id") |>
  filter(is.finite(IDEO_old), is.finite(IDEO))
R_OLD_NEW <- cor(old_new_ideo$IDEO_old, old_new_ideo$IDEO)

# ---- 3. External validity -------------------------------------------------------------------
mpds <- read_parquet(PIPE_MPDS)
RILE_R <- c(104, 201, 203, 305, 401, 402, 407, 414, 505, 601, 603, 605, 606)
RILE_L <- c(103, 105, 106, 107, 202, 403, 404, 406, 412, 413, 504, 506, 701)
cnt <- function(codes) rowSums(as.matrix(mpds[, paste0("per", codes)]), na.rm = TRUE) * mpds$total / 100
ext_cmp <- mpds |> transmute(party, date_num = as.integer(date), rile,
                             rile_logit = ifelse(total > 0, log(cnt(RILE_R) + 0.5) - log(cnt(RILE_L) + 0.5), NA_real_)) |>
  distinct(party, date_num, .keep_all = TRUE)
ches <- read.csv("data/external/ches_1999-2024_meansV2.csv") |> filter(!is.na(cmp_id)) |>
  transmute(party = as.numeric(cmp_id), ches_year = year, lrgen, lrecon, galtan)
parlgov <- read.csv("data/external/parlgov_view_party.csv") |> filter(!is.na(cmp)) |>
  group_by(party = as.numeric(cmp)) |> summarise(parlgov_lr = mean(left_right, na.rm = TRUE), .groups = "drop")
pl <- read_delim("output/independence/cache/external/populist4.csv", delim = ";", show_col_types = FALSE,
                 locale = locale(encoding = "UTF-8")) |> filter(!is.na(partyfacts_id))
pf <- read_csv("output/independence/cache/external/partyfacts_external.csv", show_col_types = FALSE, guess_max = 1e5) |>
  filter(dataset_key == "manifesto") |> transmute(party = as.numeric(dataset_party_id), partyfacts_id)
base <- new |> mutate(date_num = as.integer(format(date, "%Y%m")))
ches_match <- base |> select(doc_id, party, year) |>
  inner_join(ches, by = "party", relationship = "many-to-many") |>
  mutate(gap = abs(ches_year - year)) |> filter(gap <= 2) |>
  arrange(doc_id, gap, ches_year) |> group_by(doc_id) |> slice(1) |> ungroup() |>
  select(doc_id, lrgen, lrecon, galtan, ches_year)
pl_flags <- base |> select(doc_id, party, year) |>
  left_join(pf, by = "party", relationship = "many-to-many") |>
  left_join(pl |> select(partyfacts_id, farright, farright_start, farright_end, farleft, farleft_start, farleft_end,
                         populist, populist_start, populist_end), by = "partyfacts_id", relationship = "many-to-many") |>
  group_by(doc_id) |>
  # window = classified period, flag = current status only (see R/50_Independence_4; review #13)
  summarise(pl_farright = as.numeric(any(coalesce(year >= farright_start & year <= farright_end, FALSE))),
            pl_farleft  = as.numeric(any(coalesce(year >= farleft_start & year <= farleft_end, FALSE))),
            pl_populist = as.numeric(any(coalesce(year >= populist_start & year <= populist_end, FALSE))),
            pl_matched  = any(!is.na(farright)), .groups = "drop")
ext <- base |> left_join(ext_cmp, by = c("party", "date_num"), suffix = c("", "_mpds")) |>
  left_join(ches_match, by = "doc_id") |> left_join(parlgov, by = "party") |> left_join(pl_flags, by = "doc_id") |>
  left_join(old |> select(doc_id, IDEO_old = IDEO), by = "doc_id")
EXT_VARS <- c(rile = "CMP rile", rile_logit = "CMP rile, Lowe logit", lrgen = "CHES lrgen", lrecon = "CHES lrecon",
              galtan = "CHES galtan", parlgov_lr = "ParlGov left_right")
ext_val <- map_dfr(names(EXT_VARS), function(v) {
  map_dfr(c("IDEO", "IDEO_c", "IDEO_old", "IDEO_diff", "IDEO_logit"), function(iv) {
    x <- if (iv %in% names(ext)) ext[[iv]] else alt[[iv]][match(ext$doc_id, alt$doc_id)]
    ok <- is.finite(x) & is.finite(ext[[v]])
    tibble(external = v, external_label = EXT_VARS[[v]], llm_measure = iv, n = sum(ok),
           r = if (sum(ok) > 10) cor(x[ok], ext[[v]][ok]) else NA_real_,
           rho = if (sum(ok) > 10) cor(x[ok], ext[[v]][ok], method = "spearman") else NA_real_)
  })
})
pm_ext <- map_dfr(AGG_MODELS, function(m) {
  x <- pm |> filter(model == m) |> select(doc_id, IDEO_m = IDEO) |> inner_join(ext |> select(doc_id, all_of(names(EXT_VARS))), by = "doc_id")
  map_dfr(names(EXT_VARS), function(v) { ok <- is.finite(x$IDEO_m) & is.finite(x[[v]])
    tibble(model = m, external = v, n = sum(ok), r = if (sum(ok) > 10) cor(x$IDEO_m[ok], x[[v]][ok]) else NA_real_) })
})
pl_tab <- ext |> filter(pl_matched %in% TRUE) |>
  mutate(group = case_when(pl_farright == 1 ~ "far right", pl_farleft == 1 ~ "far left", TRUE ~ "neither")) |>
  group_by(group) |> summarise(n = n(), n_ideo = sum(is.finite(IDEO)), mean_IDEO = mean(IDEO, na.rm = TRUE),
                               mean_POP = mean(populism_overall, na.rm = TRUE), .groups = "drop")
auc <- function(score, y) { ok <- is.finite(score) & !is.na(y); s <- score[ok]; y <- y[ok]
  r <- rank(s); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
fr_fl <- ext |> filter(pl_farright == 1 | pl_farleft == 1, !(pl_farright == 1 & pl_farleft == 1))
AUC_FR_FL <- auc(fr_fl$IDEO, fr_fl$pl_farright)
write_csv_det(ext_val, file.path(CSV, "44_external_validity.csv"))
# 44_external_validity_by_model.csv is written in section 3c (POP and IDEO, all Table 3 indicators)
write_csv_det(pl_tab, file.path(CSV, "44_populist_far_groups.csv"))

# ---- 3b. Paper Table 3 (tab:external; decision D7, PLLM-0014 / PLLM-0132; 2026-10-27) ----------------
# Rows: external indicators; columns: r with POP (LLM) and with IDEO (LLM), N each.
#   CMP rile / rile logit: same manifesto.  CHES: nearest wave within +-2 years (as above).
#   ParlGov: party mean (time-invariant).  PopuList: classified for the election year (AUCs).
#   POPPA 2018 and GPS 2019 (single survey waves): matched to CMP party codes via Partyfacts (POPPA
#   falls back to its own cmp_id), each party's election nearest to the survey year within +-2
#   years (ties: the earlier election); one manifesto per party.
#   RFPOPI (Celico, Rode & Rodriguez Carreno 2024; random-forest populism indicators, party-election
#   level, V-Party based): populism_rhet (rhetorical) and populism_ide (ideational), 0-10. Matched
#   via its Partyfacts id (pf_party_id) to CMP party codes; per manifesto the RFPOPI row of the same
#   election year, otherwise the nearest within +-1 year (ties: the earlier year).
pf_all <- read_csv("output/independence/cache/external/partyfacts_external.csv", show_col_types = FALSE, guess_max = 1e5)
pf_cmp <- pf_all |> filter(dataset_key == "manifesto") |> transmute(party = as.numeric(dataset_party_id), partyfacts_id)
pf_key <- function(key) pf_all |> filter(dataset_key == key) |> transmute(ext_id = as.numeric(dataset_party_id), partyfacts_id)
nearest_election <- function(m, survey_year) {     # m: ext_id, party; returns one manifesto per party
  m |> distinct(ext_id, party, key_used) |>
    inner_join(base |> select(doc_id, party, year), by = "party", relationship = "many-to-many") |>
    mutate(gap = abs(year - survey_year)) |> filter(gap <= 2) |>
    arrange(party, gap, year, doc_id) |> group_by(party) |> slice(1) |> ungroup()
}
poppa <- read_tsv("data/external/poppa_2018_party_means.tab", show_col_types = FALSE)
poppa_map <- bind_rows(
  poppa |> transmute(ext_id = party_id) |> inner_join(pf_key("poppa"), by = "ext_id") |>
    inner_join(pf_cmp, by = "partyfacts_id", relationship = "many-to-many") |> transmute(ext_id, party, key_used = "Partyfacts"),
  poppa |> filter(!is.na(cmp_id)) |> transmute(ext_id = party_id, party = as.numeric(cmp_id), key_used = "POPPA cmp_id")) |>
  group_by(ext_id) |> filter(key_used == "Partyfacts" | !any(key_used == "Partyfacts")) |> ungroup()
poppa_m <- nearest_election(poppa_map, 2018) |>
  left_join(poppa |> transmute(ext_id = party_id, poppa_populism = populism, poppa_lroverall = lroverall, poppa_lrecon = lrecon), by = "ext_id") |>
  group_by(doc_id) |> slice(1) |> ungroup()
gps <- read_tsv("data/external/gps_2019_party_v2_1.tab", show_col_types = FALSE, locale = locale(encoding = "latin1"))
gps_map <- gps |> transmute(ext_id = ID_GPS) |> inner_join(pf_key("gps"), by = "ext_id") |>
  inner_join(pf_cmp, by = "partyfacts_id", relationship = "many-to-many") |> transmute(ext_id, party, key_used = "Partyfacts")
gps_m <- nearest_election(gps_map, 2019) |>
  left_join(gps |> transmute(ext_id = ID_GPS, gps_v8_populism = V8_Scale, gps_v4_econ_lr = V4_Scale, gps_v6_social = V6_Scale), by = "ext_id") |>
  group_by(doc_id) |> slice(1) |> ungroup()
rfp <- read_delim("data/external/rfpopi_final_dataset.csv", delim = ";", show_col_types = FALSE,
                  locale = locale(encoding = "UTF-8", decimal_mark = ","), na = c("", "NA")) |>
  transmute(partyfacts_id = as.numeric(pf_party_id), rf_year = as.integer(year),
            rfpopi_rhet = as.numeric(populism_rhet), rfpopi_ide = as.numeric(populism_ide)) |>
  filter(!is.na(partyfacts_id), !is.na(rf_year))
celico_m <- base |> select(doc_id, party, year) |>
  inner_join(pf_cmp, by = "party", relationship = "many-to-many") |>
  inner_join(rfp, by = "partyfacts_id", relationship = "many-to-many") |>
  mutate(gap = abs(rf_year - year)) |> filter(gap <= 1) |>
  arrange(doc_id, gap, rf_year, partyfacts_id) |> group_by(doc_id) |> slice(1) |> ungroup()
ext3 <- ext |> left_join(poppa_m |> select(doc_id, starts_with("poppa_")), by = "doc_id") |>
  left_join(gps_m |> select(doc_id, starts_with("gps_")), by = "doc_id")
ext3 <- ext3 |> left_join(celico_m |> select(doc_id, starts_with("rfpopi_")), by = "doc_id")
EXT3 <- tribble(
  ~source,                     ~external,          ~external_label,
  "CMP (Manifesto Project 2026a)", "rile",         "rile",                 # PLLM-0258: no dataset code names
  "CMP (Manifesto Project 2026a)", "rile_logit",   "rile, logit scale",
  "CHES 1999--2024",           "lrgen",            "lrgen (general left-right)",
  "CHES 1999--2024",           "lrecon",           "lrecon (economic left-right)",
  "CHES 1999--2024",           "galtan",           "galtan (GAL-TAN)",
  "ParlGov",                   "parlgov_lr",       "left\\_right",
  "POPPA 2018",                "poppa_populism",   "populism",
  "POPPA 2018",                "poppa_lroverall",  "left-right, overall",
  "POPPA 2018",                "poppa_lrecon",     "left-right, economic",
  "Global Party Survey 2019",  "gps_v8_populism",  "V8 populist rhetoric",
  "Global Party Survey 2019",  "gps_v4_econ_lr",   "V4 economic left-right",
  "Global Party Survey 2019",  "gps_v6_social",    "V6 social conservatism",
  "RFPOPI (Celico et al. 2024)", "rfpopi_rhet",    "populism, rhetorical",
  "RFPOPI (Celico et al. 2024)", "rfpopi_ide",     "populism, ideational")
ext_tab <- map_dfr(seq_len(nrow(EXT3)), function(i) map_dfr(c(POP = "populism_overall", IDEO = "IDEO"), function(x) {
  v <- EXT3$external[i]
  if (!v %in% names(ext3)) return(tibble(source = EXT3$source[i], external = v, external_label = EXT3$external_label[i],
                                         llm = x, stat = "r", value = NA_real_, n = 0L, available = FALSE))
  ok <- is.finite(ext3[[x]]) & is.finite(ext3[[v]])
  tibble(source = EXT3$source[i], external = v, external_label = EXT3$external_label[i], llm = x, stat = "r",
         value = if (sum(ok) > 10) cor(ext3[[x]][ok], ext3[[v]][ok]) else NA_real_, n = sum(ok), available = TRUE)
}, .id = "measure"))
# POP sample as Test 4 (R/50_Independence_4): PopuList's countries, elections 1989+, parties not
# on the list are non-populist by construction
plm <- ext |> mutate(cn = if_else(countryname == "Czech Republic", "Czechia", countryname)) |>
  filter(cn %in% unique(pl$country_name) | countryname %in% unique(pl$country_name), year >= 1989,
         is.finite(populism_overall))
fr_fl_i <- fr_fl |> filter(is.finite(IDEO))
ext_auc <- tibble(source = "PopuList 4.0", measure = c("POP", "IDEO"), llm = c("populism_overall", "IDEO"),
                  external = c("populist", "farright_vs_farleft"),
                  external_label = c("populist vs. not (AUC)", "far right vs. far left (AUC)"), stat = "AUC",
                  value = c(auc(plm$populism_overall, plm$pl_populist), AUC_FR_FL),
                  n = c(nrow(plm), nrow(fr_fl_i)), available = TRUE,
                  n_pos = c(sum(plm$pl_populist == 1), sum(fr_fl_i$pl_farright == 1)),
                  n_neg = c(sum(plm$pl_populist == 0), sum(fr_fl_i$pl_farright == 0)))
write_csv_det(bind_rows(ext_tab, ext_auc), file.path(CSV, "44_external_validity_table.csv"))
# matches register (csv/44_external_matches.csv, tab_external_matches.tex for the online appendix)
# PLLM-0255 (2026-11-11): `var` may name several indicators of one source; a manifesto counts as
# matched if any of them has a value. POPPA was counted on its populism score only (178 manifestos),
# but some POPPA parties have the left-right scores without a populism score, so Table 3 shows up to
# 189 manifestos for POPPA left-right; the register now counts all three POPPA indicators.
mrow <- function(dataset, version, key, window, var) {
  var <- intersect(var, names(ext3))
  ok <- if (length(var)) rowSums(sapply(var, function(v) is.finite(ext3[[v]]))) > 0 else rep(FALSE, nrow(ext3))
  tibble(dataset = dataset, version = version, key_used = key, window = window,
         parties_matched = n_distinct(ext3$party[ok]), manifestos_matched = sum(ok))
}
matches <- bind_rows(
  mrow("CMP rile", "Manifesto Project 2026a", "same manifesto (party, election month)", "same election", "rile"),
  mrow("CHES", "trend file 1999-2024 (means V2)", "CHES cmp_id", "nearest wave, +-2 years", "lrgen"),
  mrow("ParlGov", "development version (view_party)", "ParlGov cmp", "time-invariant party mean", "parlgov_lr"),
  ext |> filter(pl_matched %in% TRUE) |> summarise(dataset = "PopuList", version = "4.0", key_used = "Partyfacts",
    window = "classified for the election year", parties_matched = n_distinct(party), manifestos_matched = n()),
  mrow("POPPA", "2018, v2.0 (party means)",
       sprintf("Partyfacts (%d parties), POPPA cmp_id (%d)", sum(poppa_m$key_used == "Partyfacts"), sum(poppa_m$key_used == "POPPA cmp_id")),
       "nearest election to 2018, +-2 years", c("poppa_populism", "poppa_lroverall", "poppa_lrecon")),
  mrow("Global Party Survey", "2019, party file V2.1 (Dataverse v3.0)", "Partyfacts", "nearest election to 2019, +-2 years", c("gps_v8_populism", "gps_v4_econ_lr", "gps_v6_social")),
  mrow("RFPOPI (Celico et al. 2024)", "final dataset, GitHub commit 2870ebb (2024-02-16)", "Partyfacts",
       "same election year, else nearest +-1 year", "rfpopi_rhet"))
write_csv_det(matches, file.path(CSV, "44_external_matches.csv"))
write_atomic(c("% v2026a baseline; generated by R/44_Ideology_Diagnosis.R; sources: data/external/SOURCES.csv",
  "\\begin{table}[ht]", "\\caption{Matching of the external indicators to the manifestos}", "\\label{tab:oa1b}",
  "\\footnotesize", "\\centering", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%",
  "\\begin{tabular}{lllrr}", "\\toprule",
  "Dataset & Version & Match key & Parties & Manifestos \\\\", "\\midrule",
  with(matches, sprintf("%s & %s & %s; %s & %s & %s \\\\", tex_lbl(dataset), tex_lbl(version), tex_lbl(key_used), gsub("\\+-", "$\\\\pm$", tex_lbl(window)),
                        scales::comma(parties_matched), scales::comma(manifestos_matched))),
  "\\bottomrule", "\\end{tabular}}", "\\par\\smallskip",
  "\\begin{minipage}{\\linewidth}\\scriptsize Parties and manifestos of the corpus with a value of at least one indicator of the source. Partyfacts links the survey's party identifiers to CMP party codes. For the single-wave surveys (POPPA, Global Party Survey) each party enters once, with its election nearest to the survey year. RFPOPI is at the party-election level: each manifesto takes the RFPOPI value of the same election year, otherwise of the nearest year within one year.\\end{minipage}",
  "\\end{table}"), file.path(TAB, "tab_external_matches.tex"), writer = function(x, p) writeLines(x, p))
cell3 <- function(src, v, m) { x <- ext_tab |> filter(external == v, measure == m)
  if (!x$available) return(c("\\multicolumn{1}{c}{--}", "")) ; c(f2(x$value), scales::comma(x$n)) }
t3_rows <- unlist(map(unique(EXT3$source), function(src) {
  e <- EXT3 |> filter(source == src)
  c(sprintf("\\multicolumn{5}{l}{\\emph{%s}} \\\\", src),
    map_chr(seq_len(nrow(e)), function(i) { a <- cell3(src, e$external[i], "POP"); b <- cell3(src, e$external[i], "IDEO")
      sprintf("\\quad %s & %s & %s & %s & %s \\\\", e$external_label[i], a[1], a[2], b[1], b[2]) }))
}))
write_atomic(c("% v2026a baseline; generated by R/44_Ideology_Diagnosis.R; external sources: data/external/SOURCES.csv",
  "\\begin{table}[ht]", "\\caption{Correlation of the LLM Measures with External Indicators}", "\\label{tab:external}",   # Title Case (PLLM-0256)
  "\\footnotesize", "\\centering", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%",
  "\\begin{tabular}{lrrrr}", "\\toprule",
  " & \\multicolumn{2}{c}{Populism (LLM)} & \\multicolumn{2}{c}{Ideology (LLM)} \\\\",   # PLLM-0258
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}", "External indicator & $r$ & N & $r$ & N \\\\", "\\midrule",
  t3_rows,
  "\\multicolumn{5}{l}{\\emph{PopuList 4.0}} \\\\",
  sprintf("\\quad populist vs.\\ not, AUC & %s & %s & & \\\\", f2(ext_auc$value[1]), scales::comma(ext_auc$n[1])),
  sprintf("\\quad far right vs.\\ far left, AUC & & & %s & %s \\\\", f2(ext_auc$value[2]), scales::comma(ext_auc$n[2])),
  "\\bottomrule", "\\end{tabular}}", "\\par\\smallskip",
  # PLLM-0258: acronyms expanded, no dataset code names in the note
  paste0("\\begin{minipage}{\\linewidth}\\scriptsize Pearson correlations at the manifesto level; N = manifestos with both scores. ",
         "Matching: the right-left index (rile) of the Comparative Manifesto Project (CMP) and its logit version (Lowe et al.\\ 2011, from the category counts of the Manifesto Project 2026a) from the same manifesto; ",
         "Chapel Hill Expert Survey (CHES) trend file for the same party in the wave nearest to the election year, at most two years away (lrgen: general left-right; lrecon: economic left-right; galtan: green-alternative-libertarian (GAL) vs.\\ traditional-authoritarian-nationalist (TAN)); ParlGov left\\_right as the party's time-invariant mean; ",
         "Populism and Political Parties Expert Survey (POPPA) 2018 (Meijers and Zaslove) and Global Party Survey (GPS) 2019 (Norris): each party's election nearest to the survey year, at most two years away, party identifiers linked to CMP codes via Partyfacts; ",
         "PopuList 4.0 matched via Partyfacts and classified for the election year. AUC: probability that a manifesto of the first group scores higher than one of the second; ",
         "populist vs.\\ not uses all manifestos with a populism score from PopuList's countries since 1989, parties not on the list counted as non-populist (",
         scales::comma(ext_auc$n_pos[1]), " / ", scales::comma(ext_auc$n_neg[1]), "); far right vs.\\ far left: ",
         scales::comma(ext_auc$n_pos[2]), " / ", scales::comma(ext_auc$n_neg[2]), ". ",
         "RFPOPI (random-forest populism indicators of Celico, Rode and Rodr\\'{\\i}guez Carre\\~{n}o 2024): rhetorical and ideational populism of the party in the same election year (or within one year), linked via Partyfacts. ",
         " Details of the matching: Online Appendix, Table~\\ref{tab:oa1b}.\\end{minipage}"),
  "\\end{table}"),
  file.path(TAB, "tab_external_validity.tex"), writer = function(x, p) writeLines(x, p))

# ---- 3c. External validity by model family (tab:oa1a; OA1; 2026-10-27) --------------------------------
# Same indicators and matching as Table 3 (ext3, plm, fr_fl_i); POP and IDEO of each model family
# (per-model scores; IDEO per model = (R - L) / (R + L) of that model) and of the cross-model mean.
MOD4 <- c(sonnet = "Sonnet 4.6", gpt41mini = "GPT-4.1-mini", gemini = "Gemini 3.5 Flash", cross_model = "Cross-model mean")
llm_of <- function(m, what) {
  if (m == "cross_model") return(if (what == "POP") ext3$populism_overall else ext3$IDEO)
  x <- if (what == "POP") new[[paste0("populism_overall_", m)]] else
    ideo_direction(new[[paste0("pop_ideology_right_", m)]], new[[paste0("pop_ideology_left_", m)]])
  x[match(ext3$doc_id, new$doc_id)]
}
avail3 <- EXT3 |> filter(external %in% names(ext3))
by_model <- map_dfr(names(MOD4), function(m) map_dfr(c("POP", "IDEO"), function(w) {
  y <- llm_of(m, w)
  bind_rows(
    map_dfr(seq_len(nrow(avail3)), function(i) { v <- avail3$external[i]; ok <- is.finite(y) & is.finite(ext3[[v]])
      tibble(model = m, measure = w, source = avail3$source[i], external = v, stat = "r", n = sum(ok),
             value = if (sum(ok) > 10) cor(y[ok], ext3[[v]][ok]) else NA_real_) }),
    if (w == "POP") { yy <- y[match(plm$doc_id, ext3$doc_id)]; ok <- is.finite(yy)
      tibble(model = m, measure = w, source = "PopuList 4.0", external = "populist", stat = "AUC", n = sum(ok),
             value = auc(yy[ok], plm$pl_populist[ok])) } else {
      yy <- y[match(fr_fl_i$doc_id, ext3$doc_id)]; ok <- is.finite(yy)
      tibble(model = m, measure = w, source = "PopuList 4.0", external = "farright_vs_farleft", stat = "AUC", n = sum(ok),
             value = auc(yy[ok], fr_fl_i$pl_farright[ok])) })
}))
write_csv_det(by_model, file.path(CSV, "44_external_validity_by_model.csv"))
bm_cell <- function(m, w, v) { x <- by_model |> filter(model == m, measure == w, external == v)
  if (!nrow(x) || is.na(x$value)) "" else sub("^-0\\.00$", "0.00", f2(x$value)) }
bm_rows <- unlist(map(unique(avail3$source), function(src) {
  e <- avail3 |> filter(source == src)
  c(sprintf("\\multicolumn{9}{l}{\\emph{%s}} \\\\", src),
    map_chr(seq_len(nrow(e)), function(i) sprintf("\\quad %s & %s \\\\", e$external_label[i],
      paste(unlist(map(names(MOD4), function(m) c(bm_cell(m, "POP", e$external[i]), bm_cell(m, "IDEO", e$external[i])))), collapse = " & "))))
}))
auc_row <- function(v, lbl) sprintf("\\quad %s & %s \\\\", lbl, paste(unlist(map(names(MOD4), function(m)
  c(bm_cell(m, "POP", v), bm_cell(m, "IDEO", v)))), collapse = " & "))
nr <- by_model |> filter(stat == "r", n > 0)
write_atomic(c("% v2026a baseline; generated by R/44_Ideology_Diagnosis.R; external sources: data/external/SOURCES.csv",
  "\\begin{table}[ht]", "\\caption{Correlation of the LLM measures with external indicators, by model family}", "\\label{tab:oa1a}",
  "\\footnotesize", "\\centering", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%",
  "\\begin{tabular}{lrrrrrrrr}", "\\toprule",
  paste0(" & ", paste(sprintf("\\multicolumn{2}{c}{%s}", MOD4), collapse = " & "), " \\\\"),
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}",
  paste0("External indicator & ", paste(rep("POP & IDEO", 4), collapse = " & "), " \\\\"), "\\midrule",
  bm_rows, "\\multicolumn{9}{l}{\\emph{PopuList 4.0 (AUC)}} \\\\",
  auc_row("populist", "populist vs.\\ not (POP)"), auc_row("farright_vs_farleft", "far right vs.\\ far left (IDEO)"),
  "\\bottomrule", "\\end{tabular}}", "\\par\\smallskip",
  sprintf("\\begin{minipage}{\\linewidth}\\scriptsize Pearson correlations at the manifesto level; matching and samples as in Table~\\ref{tab:external}. Per model family: that model's populism score and its ideology score $(R-L)/(R+L)$; cross-model mean: the paper's measures. N per cell between %s and %s (fewer manifestos for a single model where it gives no score). AUC rows: populist vs.\\ not uses the populism score, far right vs.\\ far left the ideology score.\\end{minipage}",
          scales::comma(min(nr$n)), scales::comma(max(nr$n))),
  "\\end{table}"), file.path(TAB, "tab_external_validity_by_model.tex"), writer = function(x, p) writeLines(x, p))

# ---- 4. Concept: overlap of POP and IDEO --------------------------------------------------------
cx <- new |> filter(is.finite(IDEO), is.finite(populism_overall))
R_POP_IDEO <- cor(cx$populism_overall, cx$IDEO); R_POP_ABSIDEO <- cor(cx$populism_overall, abs(cx$IDEO))
R_POP_R <- cor(new$populism_overall, new$pop_ideology_right, use = "complete.obs")
R_POP_L <- cor(new$populism_overall, new$pop_ideology_left, use = "complete.obs")
R_POP_IOV <- cor(new$populism_overall, new$pop_ideology_overall, use = "complete.obs")
vif <- function(X) { X <- scale(X, scale = FALSE); sapply(seq_len(ncol(X)), function(j) 1 / (1 - summary(lm(X[, j] ~ X[, -j]))$r.squared)) }
VIF5 <- vif(cbind(POP = cx$populism_overall, IDEO = cx$IDEO, INT = cx$populism_overall * cx$IDEO)) |> set_names(c("POP", "IDEO", "POPxIDEO"))
VIF5c <- { p <- cx$populism_overall - mean(cx$populism_overall); i <- cx$IDEO - mean(cx$IDEO)
  vif(cbind(POP = p, IDEO = i, INT = p * i)) |> set_names(c("POP", "IDEO", "POPxIDEO")) }
joint <- cx |> mutate(pop_band = cut(populism_overall, c(-Inf, 3, 5, 7, Inf), labels = c("POP<=3", "3-5", "5-7", "POP>7")),
                      ideo_band = cut(IDEO, c(-Inf, -0.2, 0.2, Inf), labels = c("left (<-0.2)", "centre", "right (>0.2)"))) |>
  count(pop_band, ideo_band) |> pivot_wider(names_from = ideo_band, values_from = n, values_fill = 0)
HI_L <- sum(cx$populism_overall > 7 & cx$IDEO < -0.2); HI_R <- sum(cx$populism_overall > 7 & cx$IDEO > 0.2)
host <- ext |> filter(is.finite(IDEO), is.finite(lrgen), is.finite(populism_overall)) |>
  mutate(pop_band = if_else(populism_overall > 5, "POP > 5", "POP <= 5")) |>
  group_by(pop_band) |> summarise(n = n(), r_IDEO_lrgen = cor(IDEO, lrgen), r_IDEO_galtan = cor(IDEO, galtan),
                                  r_IDEO_lrecon = cor(IDEO, lrecon), .groups = "drop")
write_csv_det(joint, file.path(CSV, "44_pop_ideo_joint.csv"))
write_csv_det(host, file.path(CSV, "44_ideo_host_by_pop.csv"))
write_csv_det(tibble(stat = c("r(POP, IDEO)", "r(POP, |IDEO|)", "r(POP, R)", "r(POP, L)", "r(POP, pop_ideology_overall)",
                              paste("VIF raw", names(VIF5)), paste("VIF centred", names(VIF5c)),
                              "n POP>7 & IDEO<-0.2", "n POP>7 & IDEO>0.2", "AUC IDEO far right vs far left (PopuList)",
                              "r(IDEO Nov2025, IDEO v2026a)"),
                     value = c(R_POP_IDEO, R_POP_ABSIDEO, R_POP_R, R_POP_L, R_POP_IOV, VIF5, VIF5c, HI_L, HI_R, AUC_FR_FL, R_OLD_NEW)),
              file.path(CSV, "44_concept_stats.csv"))
p_joint <- ggplot(cx, aes(IDEO, populism_overall)) + geom_point(alpha = 0.25, size = 0.7) +
  geom_density_2d(colour = "steelblue", linewidth = 0.3) + theme_minimal(base_size = 11) +
  labs(x = "Ideology, IDEO = (R - L) / (R + L)", y = "Populism (overall)",
       title = "Joint distribution of populism and ideology (v2026a)")
save_pdf(p_joint, file.path(FIG, "ideo_pop_joint.pdf"), 7, 5)

# ---- 5. Tables 4-8 IDEO models with external ideology -------------------------------------------
XVARS <- c(IDEO = "IDEO (LLM)", rile = "CMP rile", rile_logit = "CMP rile (logit)", lrgen = "CHES lrgen",
           parlgov_lr = "ParlGov left_right")
ext_z <- ext |> mutate(across(all_of(names(XVARS)), ~ as.numeric(scale(.x)), .names = "z_{.col}"))
reest <- map_dfr(names(XVARS), function(xv) map_dfr(LIBV, function(dv) {
  z <- paste0("z_", xv)
  dd <- ext_z |> filter(is.finite(.data[[z]]))
  f <- function(rhs, fe) as.formula(paste(dv, "~", rhs, "|", fe))
  g <- function(fm, term) { m <- tryCatch(feols(fm, data = dd), error = function(e) NULL)
    if (is.null(m) || !term %in% names(coef(m))) return(tibble(estimate = NA_real_, p = NA_real_, n = NA_integer_))
    ct <- coeftable(summary(m, vcov = ~party)); tibble(estimate = ct[term, 1], p = ct[term, 4], n = nobs(m)) }
  bind_rows(g(f(z, "countryname + year"), z) |> mutate(model = "m2", term = "X"),
            g(f(paste("populism_overall +", z), "countryname + year"), z) |> mutate(model = "m4", term = "X"),
            g(f(paste("populism_overall *", z), "countryname + year"), paste0("populism_overall:", z)) |> mutate(model = "m5", term = "POPxX"),
            g(f(paste("populism_overall +", z), "countryname + year + party"), z) |> mutate(model = "m6", term = "X")) |>
    mutate(x = xv, x_label = XVARS[[xv]], dv = dv, .before = 1)
}))
# IDEO on each external measure's own sample (separates sample from measure)
reest_same <- map_dfr(setdiff(names(XVARS), "IDEO"), function(xv) map_dfr(LIBV, function(dv) {
  dd <- ext_z |> filter(is.finite(.data[[paste0("z_", xv)]]), is.finite(z_IDEO))
  m <- feols(as.formula(paste(dv, "~ populism_overall * z_IDEO | countryname + year")), data = dd)
  ct <- coeftable(summary(m, vcov = ~party))
  m4 <- feols(as.formula(paste(dv, "~ populism_overall + z_IDEO | countryname + year")), data = dd)
  c4 <- coeftable(summary(m4, vcov = ~party))
  tibble(sample_of = xv, dv = dv, n = nobs(m), ideo_m4 = c4["z_IDEO", 1], ideo_m4_p = c4["z_IDEO", 4],
         inter_m5 = ct["populism_overall:z_IDEO", 1], inter_m5_p = ct["populism_overall:z_IDEO", 4])
}))
# PopuList far-right / far-left dummies in place of IDEO
pl_reg <- map_dfr(LIBV, function(dv) {
  dd <- ext |> filter(pl_matched %in% TRUE)
  m4 <- feols(as.formula(paste(dv, "~ populism_overall + pl_farright + pl_farleft | countryname + year")), data = dd)
  m5 <- feols(as.formula(paste(dv, "~ populism_overall * (pl_farright + pl_farleft) | countryname + year")), data = dd)
  c4 <- coeftable(summary(m4, vcov = ~party)); c5 <- coeftable(summary(m5, vcov = ~party))
  tibble(dv = dv, n = nobs(m4), farright = c4["pl_farright", 1], farright_p = c4["pl_farright", 4],
         farleft = c4["pl_farleft", 1], farleft_p = c4["pl_farleft", 4],
         pop_x_farright = c5["populism_overall:pl_farright", 1], pop_x_farright_p = c5["populism_overall:pl_farright", 4],
         pop_x_farleft = c5["populism_overall:pl_farleft", 1], pop_x_farleft_p = c5["populism_overall:pl_farleft", 4])
})
write_csv_det(reest, file.path(CSV, "44_reestimate_external.csv"))
# Robustness of the POP x ideology interaction across the five ideology measures (5% level)
inter_rob <- reest |> filter(model == "m5", !is.na(estimate)) |> group_by(dv) |>
  summarise(n_measures = n(), sig_neg = sum(estimate < 0 & p < .05), sig_pos = sum(estimate > 0 & p < .05),
            cells = paste(sprintf("%s %s%s", x, formatC(estimate, format = "f", digits = 3),
                                  case_when(p < .01 ~ "***", p < .05 ~ "**", p < .1 ~ "*", TRUE ~ "")), collapse = "; "),
            .groups = "drop") |>
  mutate(verdict = case_when(sig_neg == n_measures ~ "robust: right steeper (all measures)",
                             sig_pos == n_measures ~ "robust: right flatter (all measures)",
                             sig_neg > 0 & sig_pos == 0 ~ sprintf("right steeper in %d of %d measures", sig_neg, n_measures),
                             sig_pos > 0 & sig_neg == 0 ~ sprintf("right flatter in %d of %d measures", sig_pos, n_measures),
                             sig_pos > 0 & sig_neg > 0 ~ "sign depends on the ideology measure",
                             TRUE ~ "no significant difference with any measure"))
write_csv_det(inter_rob, file.path(CSV, "44_interaction_robustness.csv"))
# OA table (tab:oa2f; 2026-10-27): POP x ideology interaction of model (5) across the five ideology measures
ir_long <- reest |> filter(model == "m5") |> select(x, x_label, dv, estimate, p, n)
write_csv_det(ir_long, file.path(CSV, "44_interaction_robustness_long.csv"))
IR_DV <- c(liberalism_overall = "Liberalism (overall)", lib_political = "Political", lib_social = "Social",
           lib_economic = "Economic", lib_financial_market = "Financial markets")
IR_X <- names(XVARS)
ir_cell <- function(dv, xv) { z <- ir_long |> filter(dv == !!dv, x == xv)
  if (!nrow(z) || is.na(z$estimate)) "--" else paste0(sub("^-", "$-$", f3(z$estimate)), stars(z$p)) }
ir_n <- function(xv) { z <- ir_long |> filter(x == xv, !is.na(estimate)); if (!nrow(z)) "--" else
  if (min(z$n) == max(z$n)) scales::comma(min(z$n)) else paste0(scales::comma(min(z$n)), "--", scales::comma(max(z$n))) }
write_atomic(c("% v2026a baseline; generated by R/44_Ideology_Diagnosis.R; SE clustered by party",
  "\\begin{table}[ht]", "\\caption{The populism $\\times$ ideology interaction across five ideology measures}", "\\label{tab:oa2f}",
  "\\footnotesize", "\\centering", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%",
  sprintf("\\begin{tabular}{l%sl}", strrep("r", length(IR_X))), "\\toprule",
  paste0("Outcome & ", paste(map_chr(IR_X, ~ tex_lbl(unique(ir_long$x_label[ir_long$x == .x]))), collapse = " & "), " & Verdict \\\\"), "\\midrule",
  map_chr(names(IR_DV), function(dv) paste0(IR_DV[[dv]], " & ", paste(map_chr(IR_X, ~ ir_cell(dv, .x)), collapse = " & "), " & ",
                                             inter_rob$verdict[inter_rob$dv == dv], " \\\\")),
  "\\midrule", paste0("N & ", paste(map_chr(IR_X, ir_n), collapse = " & "), " & \\\\"),
  "\\bottomrule", "\\end{tabular}}", "\\par\\smallskip",
  "\\begin{minipage}{\\linewidth}\\scriptsize Coefficient on populism $\\times$ ideology in column (5) of Tables 4--8 (country and year fixed effects, SE clustered by party), with the ideology measure in the column standardised to mean 0 and SD 1. The samples differ by measure, because each external measure covers a different subset of manifestos (N). Verdict: number of measures with a significant ($p<0.05$) negative (right-leaning parties: steeper decline) or positive (flatter) interaction. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\\end{minipage}",
  "\\end{table}"), file.path(TAB, "oa_interaction_robustness.tex"), writer = function(x, p) writeLines(x, p))
write_csv_det(reest_same, file.path(CSV, "44_reestimate_ideo_same_sample.csv"))
write_csv_det(pl_reg, file.path(CSV, "44_reestimate_populist_dummies.csv"))
tex_rows <- reest |> filter(model %in% c("m4", "m5")) |>
  mutate(cell = paste0(f3(estimate), stars(p))) |> select(x_label, dv, model, cell, n) |>
  pivot_wider(names_from = model, values_from = c(cell, n)) |>
  mutate(row = sprintf("%s & %s & %s & %s & %s \\\\", tex_lbl(x_label), DV_TEX[dv], cell_m4, cell_m5, n_m4))
write_atomic(c("% v2026a; ideology measures standardised (z); higher = right; country + year FE; SE clustered by party; generated by R/44_Ideology_Diagnosis.R",
  "\\begin{table}[ht]", "\\centering", "\\caption{Ideology coefficients with external ideology measures in place of IDEO}",
  "\\label{tab:ideo_external}", "\\scriptsize", "\\begin{tabular}{llrrr}", "\\toprule",
  "Ideology measure & Outcome & $\\beta_{X}$ (4) & $\\beta_{POP\\times X}$ (5) & N (4) \\\\", "\\midrule",
  tex_rows$row, "\\bottomrule", "\\end{tabular}", "\\end{table}"),
  file.path(TAB, "tab_ideo_external.tex"), writer = function(x, p) writeLines(x, p))

# ---- 5a. Ideology level effects of column (4) with external measures (tab:oa2k; PLLM-0250, 2026-11-11) --
# CONTR-007: the LLM ideology components overlap with the liberalism definitions (left ideology vs
# economic illiberalism). Column (4) of Tables 4-8 (LIB ~ POP + X | country + year, SE by party) with
# X = an external left-right measure in place of IDEO: CHES lrgen (general), CHES lrecon (economic),
# CHES galtan (cultural; higher = TAN), ParlGov left_right, CMP rile; all standardised (mean 0, SD 1
# over the manifestos with the measure; higher = right). Matching as in section 3 (CHES nearest wave
# within +-2 years, ParlGov party mean, rile same manifesto). For comparison: the LLM measure IDEO
# (standardised the same way) on its own sample and on each external measure's sample.
LV_X <- c(IDEO = "LLM ideology", lrgen = "CHES lrgen", lrecon = "CHES lrecon", galtan = "CHES galtan",
          parlgov_lr = "ParlGov left_right", rile = "CMP rile")
ext_lv <- ext |> mutate(across(all_of(names(LV_X)), ~ as.numeric(scale(.x)), .names = "z_{.col}"))
lv_fit <- function(dd, dv, z) {
  m <- feols(as.formula(paste(dv, "~ populism_overall +", z, "| countryname + year")), data = dd, cluster = ~party)
  ct <- coeftable(m)
  tibble(b_x = ct[z, 1], se_x = ct[z, 2], p_x = ct[z, 4], b_pop = ct["populism_overall", 1],
         se_pop = ct["populism_overall", 2], p_pop = ct["populism_overall", 4], n = nobs(m),
         n_clusters = n_distinct(dd$party[obs(m)]))
}
ideo_lv <- map_dfr(names(LV_X), function(xv) map_dfr(LIBV, function(dv) {
  z <- paste0("z_", xv)
  dd <- ext_lv |> filter(is.finite(.data[[z]]))
  own <- lv_fit(dd, dv, z) |> mutate(x = xv, x_label = LV_X[[xv]], dv = dv, .before = 1)
  # LLM IDEO on the same sample (manifestos with both measures)
  same <- if (xv == "IDEO") NULL else lv_fit(dd |> filter(is.finite(z_IDEO)), dv, "z_IDEO") |>
    rename_with(~ paste0(.x, "_ideo_same"))
  bind_cols(own, same %||% tibble(b_x_ideo_same = NA_real_, se_x_ideo_same = NA_real_, p_x_ideo_same = NA_real_,
                                  b_pop_ideo_same = NA_real_, se_pop_ideo_same = NA_real_, p_pop_ideo_same = NA_real_,
                                  n_ideo_same = NA_integer_, n_clusters_ideo_same = NA_integer_))
}))
write_csv_det(ideo_lv, file.path(CSV, "44_ideo_levels_external.csv"))
lv_cell <- function(b, se, p) ifelse(is.na(b), "--", sprintf("%s%s (%s)", sub("^-", "$-$", f3(b)), stars(p), f3(se)))
lv_rows <- function(pref) map_chr(LIBV, function(dv) {
  z <- ideo_lv |> filter(dv == !!dv)
  cells <- map_chr(names(LV_X), function(xv) { r <- z[z$x == xv, ]
    if (pref == "own") lv_cell(r$b_x, r$se_x, r$p_x) else lv_cell(r$b_x_ideo_same, r$se_x_ideo_same, r$p_x_ideo_same) })
  paste0(DV_TEX[[dv]], " & ", paste(cells, collapse = " & "), " \\\\")
})
lv_n <- function(col) map_chr(names(LV_X), function(xv) { n <- ideo_lv[[col]][ideo_lv$x == xv]
  if (all(is.na(n))) "--" else if (min(n) == max(n)) scales::comma(min(n)) else paste0(scales::comma(min(n)), "--", scales::comma(max(n))) })
write_atomic(c("% v2026a baseline; generated by R/44_Ideology_Diagnosis.R; SE clustered by party",
  "\\begin{table}[ht]", "\\caption{Ideology level effects with external ideology measures}", "\\label{tab:oa2k}",
  "\\scriptsize", "\\centering", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%",
  sprintf("\\begin{tabular}{l%s}", strrep("r", length(LV_X))), "\\toprule",
  paste0("Outcome & ", paste(tex_lbl(LV_X), collapse = " & "), " \\\\"), "\\midrule",
  sprintf("\\multicolumn{%d}{l}{\\emph{Panel A. Coefficient of the ideology measure in the column}} \\\\", length(LV_X) + 1),
  lv_rows("own"), paste0("N & ", paste(lv_n("n"), collapse = " & "), " \\\\"), "\\midrule",
  sprintf("\\multicolumn{%d}{l}{\\emph{Panel B. Coefficient of the LLM ideology measure on the sample of the column}} \\\\", length(LV_X) + 1),
  lv_rows("same"), paste0("N & ", paste(lv_n("n_ideo_same"), collapse = " & "), " \\\\"),
  "\\bottomrule", "\\end{tabular}}", "\\par\\smallskip",
  "\\begin{minipage}{\\linewidth}\\scriptsize Column (4) of Tables 4--8: liberalism on populism and an ideology measure, country and year fixed effects, standard errors clustered by party (CMP party code) in parentheses. Each ideology measure is standardised to mean 0 and SD 1 over the manifestos it covers; higher values are further right (CHES galtan: further towards traditional-authoritarian-nationalist). LLM ideology: $(R-L)/(R+L)$. CHES lrgen, lrecon and galtan: general, economic and cultural left-right placement of the Chapel Hill Expert Survey, wave nearest to the election year within two years; ParlGov left\\_right: the party's time-invariant mean; CMP rile: right-left index of the same manifesto. Panel B: the LLM ideology measure in place of the external one, on the manifestos that have both. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.\\end{minipage}",
  "\\end{table}"), file.path(TAB, "oa_ideo_levels_external.tex"), writer = function(x, p) writeLines(x, p))

# ---- 5b. The liberalism measures themselves: old (Nov 2025) vs v2026a ------------------------------
# The bridge shows the IDEO coefficients flip when the POP/LIB scores change (S1), with IDEO
# held at its old values. So: structure and validity of old vs new liberalism scores.
SUB <- c("lib_political", "lib_social", "lib_economic", "lib_financial_market")
lib_struct <- map_dfr(list(old = old, new = new), function(x) {
  cm <- cor(x[, SUB], use = "pairwise.complete.obs"); ev1 <- eigen(cm)$values[1] / length(SUB)
  tibble(mean_offdiag_r = mean(cm[upper.tri(cm)]), r_social_economic = cm["lib_social", "lib_economic"],
         r_social_financial = cm["lib_social", "lib_financial_market"], first_eigen_share = ev1)
}, .id = "scores")
meta_cmp <- read_parquet(PIPE_META, col_select = c("doc_id", "markeco", "planeco")) |> distinct(doc_id, .keep_all = TRUE) |>
  mutate(cmp_market = markeco - planeco)
val_dv <- map_dfr(list(old = old, new = new), function(x) {
  y <- x |> select(doc_id, all_of(SUB)) |> inner_join(ext |> select(doc_id, lrecon, galtan, lrgen), by = "doc_id") |>
    left_join(meta_cmp |> select(doc_id, cmp_market), by = "doc_id") |>
    filter(doc_id %in% intersect(old$doc_id, new$doc_id))
  rr <- function(a, b) { ok <- is.finite(y[[a]]) & is.finite(y[[b]]); c(r = cor(y[[a]][ok], y[[b]][ok]), n = sum(ok)) }
  tribble(~pair, ~expected,
          "lib_economic ~ CHES lrecon", "+", "lib_financial_market ~ CHES lrecon", "+",
          "lib_economic ~ CMP markeco - planeco", "+", "lib_social ~ CHES galtan", "-", "lib_political ~ CHES galtan", "-") |>
    mutate(v = map(pair, function(pp) { a <- sub(" ~.*", "", pp)
      b <- c("CHES lrecon" = "lrecon", "CMP markeco - planeco" = "cmp_market", "CHES galtan" = "galtan")[[sub(".*~ ", "", pp)]]
      rr(a, b) }), r = map_dbl(v, "r"), n = map_dbl(v, "n")) |> select(-v)
}, .id = "scores")
write_csv_det(lib_struct, file.path(CSV, "44_lib_structure_old_new.csv"))
write_csv_det(val_dv, file.path(CSV, "44_lib_validity_old_new.csv"))

# ---- 5c. IDEO selection forks (chain-review #5) ---------------------------------------------------
# IDEO is NA if any of R, L, C is missing, and R/L/C can rest on different model subsets. Forks:
#   IDEO       baseline
#   IDEO_zero  missing R/L/C = 0 where POP is non-missing
#   IDEO_ge2   >= 2 models contribute to each of R, L, C
#   IDEO_all3  all 3 models contribute to each of R, L, C
IDEO_FORKS <- c(IDEO = "pop_ideology_direction", IDEO_zero = "pop_ideology_direction_zero",
                IDEO_ge2 = "pop_ideology_direction_ge2", IDEO_all3 = "pop_ideology_direction_all3")
ideo_sel <- new |> filter(!is.na(populism_overall)) |> group_by(IDEO_present = !is.na(IDEO)) |>
  summarise(n = n(), mean_POP = mean(populism_overall), mean_LIB = mean(liberalism_overall, na.rm = TRUE),
            share_R_without_L_or_C = mean(!is.na(pop_ideology_right) & (is.na(pop_ideology_left) | is.na(pop_ideology_centrist))),
            .groups = "drop")
ideo_forks <- map_dfr(names(IDEO_FORKS), function(fk) map_dfr(LIBV, function(dv) {
  dd <- new |> mutate(X = .data[[IDEO_FORKS[[fk]]]])
  m4 <- feols(as.formula(paste(dv, "~ populism_overall + X | countryname + year")), data = dd)
  m5 <- feols(as.formula(paste(dv, "~ populism_overall * X | countryname + year")), data = dd)
  c4 <- coeftable(summary(m4, vcov = ~party)); c5 <- coeftable(summary(m5, vcov = ~party))
  tibble(fork = fk, dv = dv, n = nobs(m5), n_ideo = sum(!is.na(dd$X)),
         ideo_m4 = c4["X", 1], ideo_m4_p = c4["X", 4],
         inter_m5 = c5["populism_overall:X", 1], inter_m5_p = c5["populism_overall:X", 4])
}))
write_csv_det(ideo_sel, file.path(CSV, "44_ideo_selection.csv"))
write_csv_det(ideo_forks, file.path(CSV, "44_ideo_forks.csv"))

# ---- 6. IDEOLOGY_DIAGNOSIS.md --------------------------------------------------------------------
S <- function(st, dv, m, term = "IDEO") { x <- cell(st, dv, m, term); sprintf("%s%s", f3(x$estimate), stars(x$p)) }
ev <- function(xv, dv, m) { x <- reest |> filter(x == xv, dv == !!dv, model == m); sprintf("%s%s", f3(x$estimate), stars(x$p)) }
xr <- function(v, iv = "IDEO") { x <- ext_val |> filter(external == v, llm_measure == iv); sprintf("%s (n %s)", f2(x$r), format(x$n, big.mark = ",")) }
step_lbl <- c(S1 = "the v2026a POP/LIB scores", S2 = "the v2026a IDEO", S3 = "party-clustered SEs",
              S4 = "party FE by CMP id", S5 = "the v2026a sample")
single <- c(X_scores = "v2026a POP/LIB scores alone", X_ideo = "v2026a IDEO alone", X_se = "party-clustered SE alone")
cause_of <- function(id, dies) {
  r <- claim_steps[claim_steps$id == id, ]
  kill <- names(single)[!unlist(r[names(single)])]
  if (dies == "survives") return("survives all steps")
  if (dies == "false in the printed table already") {
    if (isTRUE(r$X_se)) return("the printed table contradicted the text only through iid stars; true with party-clustered SEs")
    return("the printed table itself contradicts the sentence")
  }
  paste0("first fails at ", dies, " (", step_lbl[[dies]], "); single switches that kill it: ",
         if (length(kill)) paste(single[kill], collapse = ", ") else "none alone") }
honest <- c(
  l195 = sprintf("IDEO is strongly associated with the liberalism dimensions in the expected host-ideology directions: r = %s (social), %s (economic), %s (financial). Report Table 2 as is and drop the 'not correlated' sentence.",
                 f2(cors_step$r_ideo[cors_step$step == "S5" & cors_step$dv == "lib_social"]),
                 f2(cors_step$r_ideo[cors_step$step == "S5" & cors_step$dv == "lib_economic"]),
                 f2(cors_step$r_ideo[cors_step$step == "S5" & cors_step$dv == "lib_financial_market"])),
  l346 = sprintf("Conditional on populism, ideology is not significantly related to overall liberalism (col. 4: %s); its components pull in opposite directions (social %s, economic %s).",
                 S("S5", "liberalism_overall", "m4"), S("S5", "lib_social", "m4"), S("S5", "lib_economic", "m4")),
  l385_ideo = sprintf("Right-leaning manifestos are less politically liberal (col. 4: %s); keep, with the v2026a numbers.", S("S5", "lib_political", "m4")),
  l422_ideo = sprintf("Right-leaning manifestos are much less socially liberal (col. 4: %s); keep, update.", S("S5", "lib_social", "m4")),
  l459_ideo = sprintf("Right-leaning manifestos are MORE economically liberal (col. 4: %s; CHES lrgen: %s). Replace the sentence.",
                      S("S5", "lib_economic", "m4"), ev("lrgen", "lib_economic", "m4")),
  l496_ideo = sprintf("Right-leaning manifestos are MORE financial-market liberal (col. 4: %s; CHES lrgen: %s). Replace the sentence.",
                      S("S5", "lib_financial_market", "m4"), ev("lrgen", "lib_financial_market", "m4")),
  l375 = sprintf("Keep: no significant left-right difference in the populism slope for overall liberalism (b3 = %s).", S("S5", "liberalism_overall", "m5", "populism_overall:IDEO")),
  l385_int = sprintf("Keep (b3 = %s), noting it is negative (right somewhat worse) and significant in the r12 fork.", S("S5", "lib_political", "m5", "populism_overall:IDEO")),
  l422_int = sprintf("Strengthen: right-wing populism erodes social liberalism clearly more (b3 = %s); drop 'marginally'.", S("S5", "lib_social", "m5", "populism_overall:IDEO")),
  l459_int = sprintf("Replace: b3 = %s with the LLM IDEO, but across ideology measures: %s (%s). Report as not robust.",
                     S("S5", "lib_economic", "m5", "populism_overall:IDEO"),
                     inter_rob$verdict[inter_rob$dv == "lib_economic"], inter_rob$cells[inter_rob$dv == "lib_economic"]),
  l496_int = sprintf("Replace: b3 = %s with the LLM IDEO; across ideology measures: %s (%s). Drop 'right less control-oriented'.",
                     S("S5", "lib_financial_market", "m5", "populism_overall:IDEO"),
                     inter_rob$verdict[inter_rob$dv == "lib_financial_market"], inter_rob$cells[inter_rob$dv == "lib_financial_market"]))
tickets <- claim_steps |> mutate(cause = map2_chr(id, dies_at, cause_of), instead = honest[id],
                                 at_S0 = S0, at_S5 = S5) |>
  transmute(line, claim, `holds in printed setup` = at_S0, `holds in v2026a` = at_S5, `dies at` = dies_at, cause, `paper ticket` = instead)
md_tab <- function(df) {
  df <- df |> mutate(across(everything(), ~ str_replace_all(as.character(.x), "\\|", "/")))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", strrep("---|", ncol(df))),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
bridge_md <- bridge |> filter(step %in% ST[1:6], term %in% c("IDEO", "populism_overall:IDEO"), model %in% c("m4", "m5")) |>
  filter(!(term == "IDEO" & model == "m5")) |>
  mutate(cell = paste0(f3(estimate), stars(p)), col = paste(dv, if_else(term == "IDEO", "IDEO(4)", "POPxIDEO(5)"))) |>
  select(col, step, cell) |> pivot_wider(names_from = step, values_from = cell)
ext_md <- ext_val |> filter(llm_measure %in% c("IDEO", "IDEO_old")) |>
  transmute(external = external_label, llm = recode(llm_measure, IDEO = "IDEO v2026a", IDEO_old = "IDEO Nov 2025"),
            r = f2(r), n) |> pivot_wider(names_from = llm, values_from = c(r, n))
src <- read.csv("data/external/SOURCES.csv")
pr <- function(p, from) { k <- grep(p, readLines(from, warn = FALSE), fixed = TRUE); trimws(readLines(from, warn = FALSE)[k]) }
killed_by <- function(sw) claim_steps$id[claim_steps$S0 & !claim_steps[[sw]]]
cl_lab <- setNames(claim_steps$claim, claim_steps$id)
cell_txt <- function(st, dv) S(st, dv, "m4")
ls_old <- lib_struct |> filter(scores == "old"); ls_new <- lib_struct |> filter(scores == "new")
vd <- function(sc, pair) { x <- val_dv |> filter(scores == sc, pair == !!pair); f2(x$r) }
verdict <- c(
  sprintf("1. **Where the claims die.** Of the ideology claims that hold in the printed setup, %d die when only the POP/LIB scores are replaced by v2026a (old IDEO kept): %s. Replacing only IDEO kills %d (%s); switching only to party-clustered SEs kills %d (%s). The IDEO coefficient in Table 4 col. (4) goes %s (printed) -> %s (new POP/LIB, old IDEO) -> %s (new IDEO); Table 8: %s -> %s -> %s.",
          length(killed_by("X_scores")), paste(cl_lab[killed_by("X_scores")], collapse = "; "),
          length(killed_by("X_ideo")), if (length(killed_by("X_ideo"))) paste(cl_lab[killed_by("X_ideo")], collapse = "; ") else "none",
          length(killed_by("X_se")), if (length(killed_by("X_se"))) paste(cl_lab[killed_by("X_se")], collapse = "; ") else "none",
          cell_txt("S0", "liberalism_overall"), cell_txt("S1", "liberalism_overall"), cell_txt("S2", "liberalism_overall"),
          cell_txt("S0", "lib_financial_market"), cell_txt("S1", "lib_financial_market"), cell_txt("S2", "lib_financial_market")),
  sprintf(paste0("2. **", if (length(killed_by("X_scores")) > length(killed_by("X_ideo")) && ls_old$r_social_economic > ls_new$r_social_economic)
                   "Cause: the liberalism measures, not IDEO." else "Cause: mixed (liberalism and IDEO measures).", "** The Nov-2025 liberalism sub-scores were close to one general factor (mean inter-correlation %s, first eigenvalue %s%% of variance, r(social, economic) = %s); in v2026a they are distinct (mean %s, %s%%, r(social, economic) = %s). The new economic and financial scores are also more valid against independent instruments: r with CHES lrecon %s (old %s) and %s (old %s); with CMP markeco - planeco %s (old %s); lib_social with CHES galtan %s (old %s). Once economic/financial liberalism is measured separately from social liberalism, right-leaning ideology is positively related to it, as host-ideology theory predicts."),
          f2(ls_old$mean_offdiag_r), formatC(100 * ls_old$first_eigen_share, format = "f", digits = 0), f2(ls_old$r_social_economic),
          f2(ls_new$mean_offdiag_r), formatC(100 * ls_new$first_eigen_share, format = "f", digits = 0), f2(ls_new$r_social_economic),
          vd("new", "lib_economic ~ CHES lrecon"), vd("old", "lib_economic ~ CHES lrecon"),
          vd("new", "lib_financial_market ~ CHES lrecon"), vd("old", "lib_financial_market ~ CHES lrecon"),
          vd("new", "lib_economic ~ CMP markeco - planeco"), vd("old", "lib_economic ~ CMP markeco - planeco"),
          vd("new", "lib_social ~ CHES galtan"), vd("old", "lib_social ~ CHES galtan")),
  sprintf(paste0("3. **", { rl <- ext_val$r[ext_val$external == "lrgen" & ext_val$llm_measure %in% c("IDEO", "IDEO_old")]
                        if (all(rl > 0.7)) "IDEO itself is a valid left-right measure, old and new." else "IDEO validity is limited." }, "** v2026a IDEO correlates %s with CMP rile, %s with CHES lrgen, %s with CHES galtan and %s with ParlGov left_right; the Nov-2025 IDEO correlates %s with CHES lrgen; old and new IDEO correlate %s. PopuList far-right vs far-left manifestos are separated with AUC %s."),
          xr("rile"), xr("lrgen"), xr("galtan"), xr("parlgov_lr"), xr("lrgen", "IDEO_old"), f2(R_OLD_NEW), f2(AUC_FR_FL)),
  sprintf(paste0("4. **", { e <- reest |> filter(x == "lrgen", model == "m4"); s5 <- bridge |> filter(step == "S5", model == "m4", term == "IDEO")
                        if (all(sign(e$estimate[match(LIBV[-1], e$dv)]) == sign(s5$estimate[match(LIBV[-1], s5$dv)])))
                          "External ideology reproduces the v2026a level pattern." else "External ideology only partly reproduces the v2026a level pattern." }, "** With CHES lrgen (z) in place of IDEO (col. 4): social %s, economic %s, financial %s, overall %s. The interactions do not replicate robustly: with lrgen social %s, economic %s, financial %s; with rile social %s, economic %s."),
          ev("lrgen", "lib_social", "m4"), ev("lrgen", "lib_economic", "m4"), ev("lrgen", "lib_financial_market", "m4"),
          ev("lrgen", "liberalism_overall", "m4"), ev("lrgen", "lib_social", "m5"), ev("lrgen", "lib_economic", "m5"),
          ev("lrgen", "lib_financial_market", "m5"), ev("rile", "lib_social", "m5"), ev("rile", "lib_economic", "m5")),
  sprintf("4b. **IDEO selection (missing R/L/C).** IDEO is missing for %d manifestos with POP; they are less populist (mean POP %s vs %s). Across the selection forks (baseline / missing = 0 / >= 2 models / all 3 models; N %s), the social interaction is %s and the economic interaction %s (`44_ideo_forks.csv`).",
          ideo_sel$n[!ideo_sel$IDEO_present], f2(ideo_sel$mean_POP[!ideo_sel$IDEO_present]), f2(ideo_sel$mean_POP[ideo_sel$IDEO_present]),
          paste(ideo_forks$n[ideo_forks$dv == "lib_social"], collapse = " / "),
          paste(with(ideo_forks |> filter(dv == "lib_social"), paste0(f3(inter_m5), stars(inter_m5_p))), collapse = " / "),
          paste(with(ideo_forks |> filter(dv == "lib_economic"), paste0(f3(inter_m5), stars(inter_m5_p))), collapse = " / ")),
  sprintf("5. **Concept.** The prompt labels the dimensions *populist* ideology, but the scored content is host-ideology issue content (redistribution and equality; national protectionism and anti-immigration), so IDEO measures host ideology. IDEO tracks CHES lrgen also among low-populism manifestos (r = %s for POP <= 5; %s for POP > 5). POP and IDEO overlap little (r = %s; r(POP, |IDEO|) = %s), centred VIFs are low (%s), and the interaction has support on both sides, though thinner on the right (POP > 7: %d left vs %d right manifestos).",
          f2(host$r_IDEO_lrgen[host$pop_band == "POP <= 5"]), f2(host$r_IDEO_lrgen[host$pop_band == "POP > 5"]),
          f2(R_POP_IDEO), f2(R_POP_ABSIDEO), paste(sprintf("%s %s", names(VIF5c), f2(VIF5c)), collapse = ", "), HI_L, HI_R),
  sprintf("6. **Measurement caveats to disclose.** IDEO is missing for %.1f%% of manifestos (per model: Sonnet %.0f%%, GPT %.0f%%, Gemini %.0f%%, because Gemini rarely gives L/R/C evidence); the ratio is more sensitive when R+L is small (mean |change in IDEO| for +1 point in R: %s in the lowest band, R+L %s, vs %s in the highest, > 12), but small denominators are rare (R+L <= 6 for %d of %d manifestos). R-L, the logit construction and the rev2 construction (R-L)/(R+L+C) correlate %s, %s and %s with IDEO.",
          100 * na_tab$na_IDEO[na_tab$model == "cross_model"], 100 * na_tab$na_IDEO[na_tab$model == "sonnet"],
          100 * na_tab$na_IDEO[na_tab$model == "gpt41mini"], 100 * na_tab$na_IDEO[na_tab$model == "gemini"],
          f2(den_tab$mean_dIDEO_R1[1]), as.character(den_tab$d_band[1]), f2(den_tab$mean_dIDEO_R1[nrow(den_tab)]),
          sum(den$den <= 6), nrow(den),
          f2(alt_cor["IDEO", "IDEO_diff"]), f2(alt_cor["IDEO", "IDEO_logit"]), f2(alt_cor["IDEO", "IDEO_c"])),
  sprintf("7. **What the paper can claim.** (a) Populism lowers liberalism on both sides of the spectrum (unchanged, robust). (b) Right-leaning host ideology goes with lower political and social, but higher economic and financial-market liberalism: level effects that CHES confirms. (c) Left-right differences in the populism slope, across five ideology measures (LLM IDEO, rile, logit rile, CHES lrgen, ParlGov): %s. The answer to the 'Milei question' rests on (a) and (b): right-leaning parties are more pro-market in level, but more populist manifestos are less economically liberal on both sides.",
          paste(sprintf("%s: %s", inter_rob$dv, inter_rob$verdict), collapse = "; ")))
md <- c("# IDEOLOGY_DIAGNOSIS — why the paper's ideology results fail on v2026a",
  "", "Generated by `R/44_Ideology_Diagnosis.R` (chain stage `ideology`, no API calls). All numbers are chain outputs (`output/v2026a/csv/44_*.csv`).",
  sprintf("Printed setup reproduced from `data/CMP/20251111-poplib_results.RData` with the R/4 aggregation: max |coefficient - printed| = %s over all Table 4-8 cells.", f3(S0_MAXDIFF)),
  "", "## Overall verdict", "", verdict,
  "", "## Paper tickets (one per failing or affected claim)", "", md_tab(tickets),
  "", "## 1. Bridge: printed Tables 4-8 -> v2026a, one ingredient at a time (cumulative; party SE from S3)", "",
  paste0("Steps: ", paste(sprintf("%s = %s", ST[1:6], map_chr(steps[1:6], "lbl")), collapse = "; "), "."), "",
  md_tab(bridge_md), "",
  "Single switches from the printed setup (attribution): see `44_bridge_cells.csv`, steps `X_ideo`, `X_scores`, `X_se`; claim status per step in `44_bridge_claims.csv`:", "",
  md_tab(claim_steps |> select(id, line, X_ideo, X_scores, X_se, dies_at) |> mutate(across(c(X_ideo, X_scores, X_se), as.character))),
  "", "Liberalism scores, old vs v2026a (structure and validity on common manifestos):", "",
  md_tab(lib_struct |> mutate(across(where(is.double), f2))), "",
  md_tab(val_dv |> mutate(r = f2(r)) |> select(-n) |> pivot_wider(names_from = scores, values_from = r)),
  "", "## 2. IDEO measurement quality", "",
  md_tab(na_tab |> mutate(across(starts_with("na_"), ~ sprintf("%.1f%%", 100 * .x)))), "",
  md_tab(agree |> mutate(r = f2(r))), "",
  sprintf("Denominator R+L (cross-model means): 5/25/50/75/95%% quantiles %s.", paste(f2(den_q), collapse = " / ")), "",
  md_tab(den_tab |> mutate(across(where(is.double), f3))), "",
  "Correlation of alternative constructions:", "", md_tab(as_tibble(alt_cor, rownames = "measure") |> mutate(across(where(is.double), f2))),
  "", "## 3. External validity (pooled correlations)", "", md_tab(ext_md), "",
  "By model (IDEO computed per model):", "", md_tab(pm_ext |> mutate(r = f2(r)) |> select(-n) |> pivot_wider(names_from = external, values_from = r)), "",
  "PopuList groups (manifestos of PopuList-covered parties):", "", md_tab(pl_tab |> mutate(across(where(is.double), f2))),
  sprintf("AUC of IDEO for far right vs far left: %s.", f2(AUC_FR_FL)), "",
  "Sources:", "", md_tab(src |> select(file, url, retrieved_at_utc, md5)),
  "PopuList 4.0 and Partyfacts external-parties file: archived 2026-10-07 in output/independence/cache/external/; CMP rile and per-codes: MPDS2026a.",
  "", "## 4. Concept", "",
  "Production prompt (R/3c_Populism_v4_safe.R), verbatim:", "", "```",
  pr("pop_ideology_left:", "R/3c_Populism_v4_safe.R"), pr("pop_ideology_right:", "R/3c_Populism_v4_safe.R"),
  pr("pop_ideology_centrist:", "R/3c_Populism_v4_safe.R"), pr("pop_ideology_overall:", "R/3c_Populism_v4_safe.R"), "```", "",
  "Nov-2025 prompt (R/_archive/3_Populism.R), verbatim:", "", "```",
  pr("pop_ideology_left:", "R/_archive/3_Populism.R"), pr("pop_ideology_right:", "R/_archive/3_Populism.R"),
  pr("pop_ideology_centrist:", "R/_archive/3_Populism.R"), "```", "",
  "The labels name *populist* ideology, but the scored content is host-ideology issue content (redistribution, social equality; national protectionism, anti-immigration). v4 dropped 'cultural threat narratives' (right) and 'deliberate non-positioning' (centrist) from the definitions.", "",
  sprintf("r(POP, IDEO) = %s; r(POP, |IDEO|) = %s; r(POP, R) = %s; r(POP, L) = %s; r(POP, pop_ideology_overall) = %s.",
          f2(R_POP_IDEO), f2(R_POP_ABSIDEO), f2(R_POP_R), f2(R_POP_L), f2(R_POP_IOV)),
  sprintf("VIF in model (5): raw %s; centred %s.", paste(sprintf("%s %s", names(VIF5), f2(VIF5)), collapse = ", "),
          paste(sprintf("%s %s", names(VIF5c), f2(VIF5c)), collapse = ", ")), "",
  "Joint distribution (manifestos):", "", md_tab(joint), "",
  "IDEO vs CHES by populism level (does IDEO track host ideology also for non-populist manifestos?):", "",
  md_tab(host |> mutate(across(where(is.double), f2))),
  "", "## 5. Tables 4-8 with external ideology (z-scored, higher = right; party SE)", "",
  md_tab(reest |> filter(model %in% c("m2", "m4", "m5")) |> mutate(cell = paste0(f3(estimate), stars(p))) |>
           select(x_label, dv, model, cell) |> pivot_wider(names_from = model, values_from = cell)), "",
  "Robustness of the POP x ideology interaction across the five measures (party SE, 5%):", "",
  md_tab(inter_rob |> select(dv, verdict, cells)), "",
  "IDEO selection forks (missing R/L/C; chain-review #5): IDEO-present vs missing manifestos, and col. 4 / col. 5 under each fork (party SE):", "",
  md_tab(ideo_sel |> mutate(across(where(is.double), f2))), "",
  md_tab(ideo_forks |> transmute(fork, dv, n, `IDEO (4)` = paste0(f3(ideo_m4), stars(ideo_m4_p)),
                                 `POPxIDEO (5)` = paste0(f3(inter_m5), stars(inter_m5_p)))), "",
  "Same-sample check (IDEO on the sample of each external measure, z-scored):", "",
  md_tab(reest_same |> transmute(sample_of, dv, n, `IDEO (4)` = paste0(f3(ideo_m4), stars(ideo_m4_p)),
                                 `POPxIDEO (5)` = paste0(f3(inter_m5), stars(inter_m5_p)))), "",
  "PopuList far-right / far-left dummies instead of IDEO (col. 4 and interactions):", "",
  md_tab(pl_reg |> transmute(dv, n, `far right` = paste0(f3(farright), stars(farright_p)), `far left` = paste0(f3(farleft), stars(farleft_p)),
                             `POP x far right` = paste0(f3(pop_x_farright), stars(pop_x_farright_p)),
                             `POP x far left` = paste0(f3(pop_x_farleft), stars(pop_x_farleft_p)))),
  "", "Figure: `figures/ideo_pop_joint.pdf`; table: `tables/tab_ideo_external.tex`.")
write_atomic(md, file.path(PIPE_OUT, "IDEOLOGY_DIAGNOSIS.md"), writer = function(x, p) writeLines(x, p, useBytes = TRUE))
message("R/44 done; S0 max |diff| vs printed = ", f3(S0_MAXDIFF))
