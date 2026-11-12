# ============================================================
# 53_Argentina_Baseline.R  (PLLM-0185, 2026-10-28)
#
# Out-of-sample manifesto scores for the Argentine 2021-2025 platforms (CNE filings) from the
# archived per-chunk scores of R/52 (data/CMP/argentina_oos/poplib_v4_<m>_raw.parquet and
# _validated_spans.parquet), aggregated with THE aggregation path of the chain
# (R/1_Aggregation.R, canonical rule (a'): verified chunks whose quote passes the quote check,
# own-block confidence weights, plain mean across models, IDEO = (R - L)/(R + L)). The span
# checks are the R/34 definitions (quote_ok = verbatim & not from the prompt; the repaired topic
# regex and the R/12 judge give topic_ok, a diagnostic only). The v2026a baseline is read, never
# written: these documents are an illustration placed on the baseline's regressions.
#
# Outputs (PIPE_OUT/argentina/ and data/CMP/argentina_oos/):
#   PIPE_DERIVED/argentina_scores_oos.parquet / .csv          one row per platform (38)
#   csv/53_argentina_scores.csv                               same, chain copy
#   csv/53_chunk_status_by_model_dim.csv                      evidence status per model x dim
#   csv/53_model_versions.csv                                 requested / resolved model per model
#   csv/53_group_means.csv                                    world / Latin America / Argentina x
#                                                             all / right / populist / right-wing populist
#   csv/53_predicted_liberalism.csv                           baseline Table 4-8 models (cols 4, 5)
#                                                             predicted for each platform; residuals
#   csv/53_lla_table.csv, tables/tab_argentina_lla.tex        La Libertad Avanza 2021-2025 (+ N of the reference rows)
#   logs/53_argentina.txt
#
# PLLM-0241 (2026-11-11):
#   csv/53_predicted_liberalism.csv gains the bootstrap interval columns of column (5)
#   csv/53_prediction_intervals.csv          party-clustered bootstrap (B = 500, seed 20261111) of column (5):
#                                            SE of the prediction, residual SD, 95% prediction interval
#                                            (normal: pred +- 1.96 sqrt(Var_boot + sigma^2); percentile:
#                                            bootstrap prediction + resampled residual), for the Argentine
#                                            platforms (out of sample) and the US Republican platforms (in sample)
#   tables/tab_argentina_lla_intervals.tex   LLA: scored, predicted, 95% PI per liberalism score
#   csv/53_argentina_all38.csv, csv/53_argentina_all38_summary.csv, tables/tab_argentina_all38.tex
#                                            all 38 platforms: predicted vs scored (overall, economic,
#                                            financial-market), residuals in residual SDs; distribution of
#                                            the non-LLA residuals and LLA relative to it
#   csv/53_fe_support.csv                    which baseline manifestos the Argentina and year FEs rest on
#   csv/53_lla_reference_rows.csv            reference rows of tab_argentina_lla with N
# PLLM-0252 (2026-11-11):
#   csv/53_us_republican.csv                 Republican platforms 2016/2020/2024 (CMP 61620) from the baseline:
#                                            scores and in-sample predictions of columns (4) and (5)
#   tables/tab_argentina_lla_US.tex, tables/tab_argentina_lla_intervals_US.tex
#                                            variants with the Republican platforms as reference rows
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(stringi); library(fixest)
})
source("R/0_Config.R")
source("R/1_Aggregation.R")
suppressMessages(source("R/3c_Populism_v4_safe.R"))
setFixest_nthreads(1)

AR_DIR <- "data/external/argentina"
OOS    <- "data/CMP/argentina_oos"
OUT    <- file.path(PIPE_OUT, "argentina")
for (d in file.path(OUT, c("csv", "tables", "figures", "logs"))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
CSV <- file.path(OUT, "csv")
log_lines <- character()
logf <- function(...) { l <- sprintf(...); message(l); log_lines <<- c(log_lines, l) }
write_tex <- function(x, path) write_atomic(x, path, writer = function(x, p) writeLines(x, p, useBytes = TRUE))

# ---- 1. Register and archived chunk scores ---------------------------------------------
docs <- read_csv(file.path(AR_DIR, "argentina_docs.csv"), col_types = cols(.default = "c")) |>
  mutate(date = as.Date(date), year = as.integer(year), milei = as.integer(milei), words = as.integer(words))

# span checks as in R/34 (quote check; fixed topic regex + judge as diagnostic)
KW_FIXED <- map(TOPIC_KEYWORDS_V4, ~ sub("\\|+$", "", .x))
KW_FIXED$populism_overall   <- paste(KW_FIXED[c("pop_anti_elitism", "pop_people_centrism", "pop_manichean")], collapse = "|")
KW_FIXED$liberalism_overall <- paste(KW_FIXED[c("lib_political", "lib_social", "lib_economic", "lib_financial_market")], collapse = "|")
span_checks <- function(sp) {
  sp |>
    mutate(quote_ok = verbatim_found %in% TRUE & !(hallucination_flag %in% TRUE),
           txt = paste(coalesce(quote, ""), coalesce(context, ""))) |>
    group_by(dim_name) |>
    mutate(topic_rx = if (cur_group()$dim_name %in% names(KW_FIXED))
      stri_detect_regex(txt, KW_FIXED[[cur_group()$dim_name]], case_insensitive = TRUE) else NA) |>
    ungroup() |>
    mutate(topic_ok = topic_rx %in% TRUE | judge_topic %in% TRUE) |>
    select(doc_id, doc_id_root, dim_name, quote_ok, topic_rx, judge = judge_topic, topic_ok)
}

raw <- list(); cl <- list(); prov <- list()
for (m in AGG_MODELS) {
  stem <- AGG_FILE_STEM[[m]]
  f_raw <- file.path(OOS, paste0("poplib_v4_", stem, "_raw.parquet"))
  f_sp  <- file.path(OOS, paste0("poplib_v4_", stem, "_validated_spans.parquet"))
  if (!file.exists(f_raw)) { logf("no raw file for %s (%s) - model leg skipped", m, f_raw); next }
  r <- read_parquet(f_raw); sp <- read_parquet(f_sp)
  ck <- span_checks(sp)
  stopifnot(!anyDuplicated(ck[c("doc_id", "dim_name")]))
  cl[[m]]  <- chunk_dim_long(r, ck, m)
  raw[[m]] <- r
  prov[[m]] <- r |> count(model = m, model_requested, endpoint, model_resolved, name = "chunks") |>
    mutate(docs = n_distinct(r$doc_id_root))
  logf("%s: %d documents, %d chunks, %d spans, quote check passed %.1f%%, resolved: %s", m,
       n_distinct(r$doc_id_root), nrow(r), nrow(sp), 100 * mean(ck$quote_ok),
       paste(unique(coalesce(r$model_resolved, "not logged")), collapse = "/"))
}
stopifnot(length(cl) > 0)
cl <- bind_rows(cl)
write_csv_det(chunk_status_summary(cl), file.path(CSV, "53_chunk_status_by_model_dim.csv"))
write_csv_det(bind_rows(prov), file.path(CSV, "53_model_versions.csv"))

# ---- 2. Aggregate (canonical rule) --------------------------------------------------------
wl   <- aggregate_within(cl, validation = AGG_RULES$canonical$validation)
base <- combine_models(wl, stat = AGG_RULES$canonical$stat, between = "mean")
per_model <- wl |> mutate(col = paste0(dim, "_", model)) |> select(doc_id, col, cwm) |>
  pivot_wider(names_from = col, values_from = cwm)
model_cols <- as.vector(outer(AGG_DIMS, AGG_MODELS, paste, sep = "_"))
for (cc in setdiff(model_cols, names(per_model))) per_model[[cc]] <- NA_real_
legs <- wl |> distinct(doc_id, model) |> arrange(doc_id, match(model, AGG_MODELS)) |>
  group_by(doc_id) |> summarise(model_legs = paste(model, collapse = "+"), n_model_legs = n(), .groups = "drop")
resolved <- bind_rows(imap(raw, ~ .x |> transmute(doc_id = doc_id_root, model = .y, model_resolved))) |>
  group_by(doc_id, model) |>
  summarise(resolved = paste(sort(unique(coalesce(model_resolved, "not logged"))), collapse = "+"), .groups = "drop") |>
  mutate(model = paste0("resolved_", model)) |> pivot_wider(names_from = model, values_from = resolved)

scores <- docs |>
  left_join(base |> select(doc_id, all_of(AGG_DIMS), pop_ideology_direction = IDEO, pop_ideology_direction_c = IDEO_c,
                           starts_with("n_models_")), by = "doc_id") |>
  left_join(per_model |> select(doc_id, all_of(model_cols)), by = "doc_id") |>
  left_join(legs, by = "doc_id") |> left_join(resolved, by = "doc_id") |>
  mutate(source = "Camara Nacional Electoral, plataformas electorales (OCR); out-of-sample, not in the v2026a baseline",
         aggregation = "verified chunks; span validation 'quote'; own-block confidence-weighted mean within model; mean across models",
         baseline_version = PIPE_VERSION$baseline) |>
  arrange(date, district, doc_id)
stopifnot(nrow(scores) == nrow(docs))
# derived file next to the baseline (PIPE_DERIVED, so that R/71 reruns into its scratch copy)
write_atomic(scores, file.path(PIPE_DERIVED, "argentina_scores_oos.parquet"), compression = "zstd", compression_level = 9)
write_csv_det(scores, file.path(PIPE_DERIVED, "argentina_scores_oos.csv"))
write_csv_det(scores, file.path(CSV, "53_argentina_scores.csv"))
logf("scores: %d platforms, %d with populism, %d with IDEO, %d with liberalism", nrow(scores),
     sum(!is.na(scores$populism_overall)), sum(!is.na(scores$pop_ideology_direction)), sum(!is.na(scores$liberalism_overall)))

# ---- 3. Group means from the v2026a baseline -------------------------------------------
LATAM <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic",
           "Ecuador", "Mexico", "Panama", "Peru", "Uruguay")
OUTCOMES <- c("populism_overall", "IDEO", "lib_political", "lib_social", "lib_economic", "lib_financial_market",
              "liberalism_overall", "pop_anti_elitism", "pop_people_centrism", "pop_manichean")
bl <- read_parquet(file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet")) |>
  mutate(IDEO = pop_ideology_direction)
pop_med <- median(bl$populism_overall, na.rm = TRUE); pop_q3 <- quantile(bl$populism_overall, 0.75, na.rm = TRUE, names = FALSE)
logf("baseline populism median %.3f, upper quartile %.3f (populist = above the median; populist_q3 = above the upper quartile)", pop_med, pop_q3)
bl <- bl |> mutate(right = IDEO > 0, populist = populism_overall > pop_med, populist_q3 = populism_overall > pop_q3)
regions <- list(World = unique(bl$countryname), `Latin America` = LATAM, Argentina = "Argentina")
subgroups <- list(all = function(x) rep(TRUE, nrow(x)), right = function(x) x$right %in% TRUE,
                  populist = function(x) x$populist %in% TRUE, right_populist = function(x) x$right %in% TRUE & x$populist %in% TRUE,
                  populist_q3 = function(x) x$populist_q3 %in% TRUE, right_populist_q3 = function(x) x$right %in% TRUE & x$populist_q3 %in% TRUE,
                  left = function(x) x$IDEO < 0, left_populist = function(x) x$IDEO < 0 & x$populist %in% TRUE)
gm <- imap_dfr(regions, function(cs, rn) imap_dfr(subgroups, function(fn, sn) {
  x <- bl |> filter(countryname %in% cs); x <- x[fn(x), ]
  map_dfr(OUTCOMES, ~ tibble(region = rn, subgroup = sn, outcome = .x, n = sum(!is.na(x[[.x]])),
                             mean = mean(x[[.x]], na.rm = TRUE), sd = sd(x[[.x]], na.rm = TRUE),
                             q25 = quantile(x[[.x]], 0.25, na.rm = TRUE, names = FALSE),
                             q75 = quantile(x[[.x]], 0.75, na.rm = TRUE, names = FALSE)))
})) |> mutate(across(c(mean, sd, q25, q75), ~ ifelse(is.finite(.x), .x, NA_real_)))
write_csv_det(gm, file.path(CSV, "53_group_means.csv"))

# ---- 4. Predicted liberalism from the baseline models (Tables 4-8, columns 4 and 5) -----
LIB_DVS <- c("lib_political", "lib_social", "lib_economic", "lib_financial_market", "liberalism_overall")
nd <- scores |> transmute(doc_id, countryname, year, populism_overall, IDEO = pop_ideology_direction) |>
  filter(!is.na(populism_overall), !is.na(IDEO))
pred <- map_dfr(LIB_DVS, function(dv) {
  m4 <- feols(as.formula(paste(dv, "~ populism_overall + IDEO | countryname + year")), data = bl)
  m5 <- feols(as.formula(paste(dv, "~ populism_overall * IDEO | countryname + year")), data = bl)
  # year fixed effects: 2021, 2023 and 2025 are in the baseline (other countries' elections)
  p4 <- predict(m4, newdata = nd); p5 <- predict(m5, newdata = nd)
  fe4 <- fixef(m4); fe5 <- fixef(m5)
  tibble(doc_id = nd$doc_id, outcome = dv,
         actual = scores[[dv]][match(nd$doc_id, scores$doc_id)],
         pred_additive = p4, pred_interaction = p5,
         fe_country_additive = fe4$countryname[["Argentina"]], fe_year_additive = fe4$year[as.character(nd$year)],
         fe_country_interaction = fe5$countryname[["Argentina"]], fe_year_interaction = fe5$year[as.character(nd$year)],
         b_pop_additive = coef(m4)[["populism_overall"]], b_ideo_additive = coef(m4)[["IDEO"]],
         b_pop_interaction = coef(m5)[["populism_overall"]], b_ideo_interaction = coef(m5)[["IDEO"]],
         b_popxideo_interaction = coef(m5)[["populism_overall:IDEO"]],
         resid_sd_additive = sd(resid(m4)), resid_sd_interaction = sd(resid(m5))) |>
    mutate(resid_additive = actual - pred_additive, resid_interaction = actual - pred_interaction,
           resid_additive_sd = resid_additive / resid_sd_additive, resid_interaction_sd = resid_interaction / resid_sd_interaction)
}) |> left_join(scores |> select(doc_id, abbrev, alliance, date, district, milei), by = "doc_id") |>
  arrange(outcome, date, doc_id)

# ---- 4b. Prediction intervals for column (5): party-clustered bootstrap (PLLM-0241) ------------
# Resample CMP party codes with replacement (the clustering unit of the paper's SEs), refit column (5)
# (LIB ~ POP * IDEO | country + year) on every draw and predict the same rows. The prediction
# interval adds the residual variance of the full-sample fit to the bootstrap variance of the
# prediction: pred +- 1.96 sqrt(Var_boot + sigma^2), sigma^2 = RSS / (N - K - FE levels). A
# percentile version (bootstrap prediction + one residual drawn from the full-sample residuals)
# is kept as a check: the residuals are bounded and not normal. Bounds are not truncated to 0-10.
# The Republican platforms are baseline manifestos: their prediction is in sample.
B_BOOT <- 500L; SEED_BOOT <- 20261111L
US_REP <- c("61620_201611", "61620_202011", "61620_202411")      # Republican Party, CMP 61620 (PLLM-0252)
us <- bl |> filter(doc_id %in% US_REP) |> arrange(date)
stopifnot(nrow(us) == length(US_REP))
nd_all <- bind_rows(nd |> mutate(set = "argentina_oos"),
                    us |> transmute(doc_id, countryname, year, populism_overall, IDEO, set = "us_republican_in_sample"))
set.seed(SEED_BOOT)
cl_rows <- split(seq_len(nrow(bl)), bl$party)                   # 995 clusters, sorted by party code
draws <- lapply(seq_len(B_BOOT), function(b) unlist(cl_rows[sample.int(length(cl_rows), length(cl_rows), replace = TRUE)], use.names = FALSE))
U <- matrix(runif(B_BOOT * nrow(nd_all)), B_BOOT)                # residual draws for the percentile interval
boot <- map_dfr(LIB_DVS, function(dv) {
  fml <- as.formula(paste(dv, "~ populism_overall * IDEO | countryname + year"))
  m5 <- feols(fml, data = bl)
  e  <- resid(m5); s2 <- sum(e^2) / degrees_freedom(m5, "resid")
  p0 <- predict(m5, newdata = nd_all)
  pb <- vapply(draws, function(ix) predict(feols(fml, data = bl[ix, ], warn = FALSE, notes = FALSE), newdata = nd_all),
               numeric(nrow(nd_all)))                            # rows = platforms, cols = draws
  se <- apply(pb, 1, sd, na.rm = TRUE)
  pct <- t(vapply(seq_len(nrow(nd_all)), function(i) {
    v <- pb[i, ] + e[pmax(1L, ceiling(U[, i] * length(e)))]
    quantile(v, c(0.025, 0.975), na.rm = TRUE, names = FALSE) }, numeric(2)))
  tibble(doc_id = nd_all$doc_id, set = nd_all$set, outcome = dv, n_obs = nobs(m5),
         pred = p0, se_pred_boot = se, sigma_resid = sqrt(s2), se_pi = sqrt(se^2 + s2),
         ci95_lo = p0 - 1.96 * se, ci95_hi = p0 + 1.96 * se,
         pi95_lo = p0 - 1.96 * sqrt(se^2 + s2), pi95_hi = p0 + 1.96 * sqrt(se^2 + s2),
         pi95pct_lo = pct[, 1], pi95pct_hi = pct[, 2], n_boot_valid = rowSums(!is.na(pb)))
})
actual_all <- bind_rows(scores |> select(doc_id, all_of(LIB_DVS)), us |> select(doc_id, all_of(LIB_DVS))) |>
  pivot_longer(-doc_id, names_to = "outcome", values_to = "actual")
boot <- boot |> left_join(actual_all, by = c("doc_id", "outcome")) |>
  mutate(z_pi = (actual - pred) / se_pi, outside_pi95 = actual < pi95_lo | actual > pi95_hi) |>
  relocate(actual, .after = outcome) |> arrange(set, outcome, doc_id)
# a draw cannot predict a row when it contains no manifesto with that row's country or year level (or that
# level only as a removed singleton); such draws are dropped for that row and counted in n_boot_valid
logf("bootstrap draws that predict the row: min %d of %d (rows with fewer: %s)", min(boot$n_boot_valid), B_BOOT,
     paste(unique(boot$doc_id[boot$n_boot_valid < B_BOOT]), collapse = ", "))
# US: only two party codes (Democrats, Republicans), so about exp(-2) = 13.5% of the draws hold no US manifesto;
# the Republican intervals are conditional on the US being drawn
stopifnot(min(boot$n_boot_valid[boot$set == "argentina_oos"]) >= 0.95 * B_BOOT)
write_csv_det(boot, file.path(CSV, "53_prediction_intervals.csv"))
pred <- pred |> left_join(boot |> filter(set == "argentina_oos") |>
                            select(doc_id, outcome, pred_interaction_se_boot = se_pred_boot, pi95_lo_interaction = pi95_lo,
                                   pi95_hi_interaction = pi95_hi, pi95pct_lo_interaction = pi95pct_lo,
                                   pi95pct_hi_interaction = pi95pct_hi, outside_pi95_interaction = outside_pi95),
                          by = c("doc_id", "outcome"))
stopifnot(isTRUE(all.equal(boot$pred[match(paste(pred$doc_id, pred$outcome), paste(boot$doc_id, boot$outcome))], pred$pred_interaction)))
write_csv_det(pred, file.path(CSV, "53_predicted_liberalism.csv"))
logf("bootstrap: %d party-clustered draws (seed %d) of column (5)", B_BOOT, SEED_BOOT)

# ---- 4c. US Republican platforms 2016/2020/2024 from the baseline (PLLM-0252) --------------------
us_pred <- map_dfr(LIB_DVS, function(dv) {
  m4 <- feols(as.formula(paste(dv, "~ populism_overall + IDEO | countryname + year")), data = bl)
  m5 <- feols(as.formula(paste(dv, "~ populism_overall * IDEO | countryname + year")), data = bl)
  tibble(doc_id = us$doc_id, outcome = dv, pred_additive = predict(m4, newdata = us), pred_interaction = predict(m5, newdata = us))
})
us_out <- us |> select(doc_id, countryname, party, partyname, partyabbrev, date, year, token_count,
                       populism_overall, pop_anti_elitism, pop_people_centrism, pop_manichean, ideology = IDEO,
                       pop_ideology_left, pop_ideology_right, pop_ideology_centrist, all_of(LIB_DVS),
                       n_models_populism_overall, n_models_liberalism_overall, n_models_lib_economic,
                       n_models_lib_financial_market, n_models_lib_political, n_models_lib_social) |>
  left_join(us_pred |> pivot_wider(names_from = outcome, values_from = c(pred_additive, pred_interaction)), by = "doc_id") |>
  mutate(note = case_when(year == 2020 ~ "2020 convention re-adopted the 2016 platform; identical text length in the corpus (token_count equal to 2016)",
                          year == 2024 ~ "short platform; liberalism dims from fewer model legs (see n_models_*)",
                          TRUE ~ ""),
         prediction = "in sample (baseline manifesto; country FE United States, year FE of its election)")
write_csv_det(us_out, file.path(CSV, "53_us_republican.csv"))
logf("US Republican platforms in the baseline: %s", paste(sprintf("%s (%s, %.0f tokens, pop %.2f, ideo %.2f, lib %.2f, econ %.2f)",
     us$doc_id, us$date, us$token_count, us$populism_overall, us$IDEO, us$liberalism_overall, us$lib_economic), collapse = "; "))

# ---- 4d. What the Argentina and the year fixed effects rest on (PLLM-0241) -------------------------
bl_m <- bl |> filter(!is.na(populism_overall), !is.na(IDEO), !is.na(liberalism_overall))   # estimation sample of column (5), overall liberalism
ar_m <- bl_m |> filter(countryname == "Argentina")
fe_year <- function(y) { x <- bl_m |> filter(year == y)
  tibble(fe = paste0("year ", y), n_manifestos = nrow(x), n_parties = n_distinct(x$party),
         countries = paste(sort(unique(x$countryname)), collapse = "; "),
         elections = paste(sort(unique(paste(x$countryname, format(x$date, "%Y-%m")))), collapse = "; "),
         ideology_min = min(x$IDEO), ideology_max = max(x$IDEO), n_ideology_gt0 = sum(x$IDEO > 0)) }
fe_sup <- bind_rows(
  tibble(fe = "country Argentina", n_manifestos = nrow(ar_m), n_parties = n_distinct(ar_m$party), countries = "Argentina",
         elections = paste(sort(unique(format(ar_m$date, "%Y-%m"))), collapse = "; "),
         ideology_min = min(ar_m$IDEO), ideology_max = max(ar_m$IDEO), n_ideology_gt0 = sum(ar_m$IDEO > 0)),
  map_dfr(c(2021L, 2023L, 2025L), fe_year)) |>
  mutate(statement = case_when(
    fe == "country Argentina" ~ sprintf("The Argentina fixed effect rests on %d baseline manifestos of %d parties (%d elections, %s-%s), all left of centre: ideology %.2f to %.2f, none above zero.",
                                        n_manifestos, n_parties, length(unique(ar_m$date)), min(ar_m$year), max(ar_m$year), ideology_min, ideology_max),
    !grepl(";", countries) ~ sprintf("The %s fixed effect rests on one election: %s (%d manifestos).", fe, elections, n_manifestos),
    TRUE ~ sprintf("The %s fixed effect rests on %d manifestos from %d countries.", fe, n_manifestos, lengths(strsplit(countries, "; ")))),
    sample = "estimation sample of column (5), liberalism_overall")
write_csv_det(fe_sup, file.path(CSV, "53_fe_support.csv"))
walk(fe_sup$statement, ~ logf("%s", .x))

# ---- 5. La Libertad Avanza table ----------------------------------------------------------
lla <- scores |> filter(milei == 1) |>
  transmute(doc_id, election = format(date, "%Y-%m"), stage, district, words,
            populism_overall, pop_anti_elitism, pop_people_centrism, pop_manichean, ideology = pop_ideology_direction,
            pop_ideology_left, pop_ideology_right, pop_ideology_centrist,
            lib_political, lib_social, lib_economic, lib_financial_market, liberalism_overall,
            n_model_legs, model_legs, across(starts_with("resolved_")))
write_csv_det(lla, file.path(CSV, "53_lla_table.csv"))
ref <- gm |> filter(subgroup %in% c("all", "right", "right_populist"), outcome %in% c("populism_overall", "IDEO", LIB_DVS)) |>
  mutate(region = factor(region, names(regions)), subgroup = factor(subgroup, c("all", "right", "right_populist"))) |>
  arrange(region, subgroup) |>
  mutate(label = paste0(region, ": ", recode(as.character(subgroup), all = "all parties", right = "right-wing", right_populist = "right-wing populist"))) |>
  select(label, outcome, mean, n) |>
  mutate(n = if_else(outcome == "populism_overall", n, NA_integer_)) |>
  group_by(label) |> mutate(N = max(n, na.rm = TRUE)) |> ungroup() |> select(-n) |>
  pivot_wider(names_from = outcome, values_from = mean) |>
  filter(!is.na(populism_overall))          # groups without any baseline manifesto (e.g. Argentina right-wing) are dropped
write_csv_det(ref, file.path(CSV, "53_lla_reference_rows.csv"))
fmt <- function(x) ifelse(is.na(x), "--", formatC(x, digits = 2, format = "f"))
fmtn <- function(x) formatC(x, format = "d", big.mark = ",")
plat_label <- function(abbrev, date, district, stage)
  paste0(abbrev, " ", format(date, "%Y"), " ", if_else(district == "ON", "national", district), if_else(stage == "PASO", " (PASO)", ""))
lla_rows <- map_chr(seq_len(nrow(lla)), function(i) with(lla[i, ], paste0(
  "LLA ", substr(election, 1, 4), " ", if (district == "ON") "national" else district, if (stage == "PASO") " (PASO)" else "", " & ",
  fmt(populism_overall), " & ", fmt(ideology), " & ",
  fmt(lib_political), " & ", fmt(lib_social), " & ", fmt(lib_economic), " & ", fmt(lib_financial_market), " & ", fmt(liberalism_overall), " & \\\\")))
ref_rows <- map_chr(seq_len(nrow(ref)), function(i) with(ref[i, ], paste0(
  label, " & ", fmt(populism_overall), " & ", fmt(IDEO), " & ", fmt(lib_political), " & ", fmt(lib_social), " & ",
  fmt(lib_economic), " & ", fmt(lib_financial_market), " & ", fmt(liberalism_overall), " & ", fmtn(N), " \\\\")))
us_rows <- unlist(map(seq_len(nrow(us_out)), function(i) with(us_out[i, ], c(
  paste0("US Republicans ", year, if (year == 2020) " (2016 re-adopted)" else "", " & ", fmt(populism_overall), " & ", fmt(ideology), " & ",
         fmt(lib_political), " & ", fmt(lib_social), " & ", fmt(lib_economic), " & ", fmt(lib_financial_market), " & ", fmt(liberalism_overall), " & 1 \\\\"),
  paste0("\\quad predicted, col.\\ (5), in sample & & & ", fmt(pred_interaction_lib_political), " & ", fmt(pred_interaction_lib_social), " & ",
         fmt(pred_interaction_lib_economic), " & ", fmt(pred_interaction_lib_financial_market), " & ", fmt(pred_interaction_liberalism_overall), " & \\\\")))))
note_lla <- paste0("\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}\\scriptsize\\textit{Notes:} Platforms filed with the C\\'amara Nacional Electoral ",
  "(CF = Capital Federal, BA = Buenos Aires province; 2023 = national platform, filed for the primaries (PASO) and the general election), ",
  "scored out of sample with the identical pipeline and aggregation rule as the baseline; not part of the baseline. Reference rows are ",
  "means over the baseline manifestos; $N$ = number of baseline manifestos with a populism score; right-wing = ideology $> 0$; right-wing populist = ideology $> 0$ and populism above the baseline ",
  "median (", formatC(pop_med, digits = 2, format = "f"), "). No Argentinian baseline manifesto has an ideology score above zero.")
note_us <- " US Republicans: platforms of the Republican Party (Manifesto Project code 61620) in the baseline; the 2020 convention re-adopted the 2016 platform. Their predictions come from column (5) of Tables 4--8 with the United States country and the election-year fixed effects and are in sample."
lla_table <- function(rows, note, label) c("\\begin{table}[htbp]\\centering\\scriptsize\\setlength{\\tabcolsep}{4pt}",
  paste0("\\caption{La Libertad Avanza 2021--2025 against the Baseline Averages}\\label{", label, "}"),   # Title Case (PLLM-0256)
  "\\begin{tabular}{@{}lcccccccr@{}}\\toprule",
  " & & & \\multicolumn{5}{c}{Liberalism} & \\\\ \\cmidrule(lr){4-8}",
  " & Populism & Ideology & Political & Social & Economic & Financial & Overall & $N$ \\\\ \\midrule",
  rows, "\\bottomrule\\end{tabular}", paste0(note, "\\end{minipage}"), "\\end{table}")
write_tex(lla_table(c(lla_rows, "\\midrule", ref_rows), note_lla, "tab:argentina_lla"), file.path(OUT, "tables", "tab_argentina_lla.tex"))
write_tex(lla_table(c(lla_rows, "\\midrule", ref_rows, "\\midrule", us_rows), paste0(note_lla, note_us), "tab:argentina_lla"),
          file.path(OUT, "tables", "tab_argentina_lla_US.tex"))

# ---- 5b. Prediction-interval table (PLLM-0241; _US variant with the Republicans, PLLM-0252) -------
DV5 <- c("lib_political", "lib_social", "lib_economic", "lib_financial_market", "liberalism_overall")
bw <- boot |> select(doc_id, outcome, actual, pred, pi95_lo, pi95_hi, outside_pi95)
cell <- function(id, what) map_chr(DV5, function(dv) { r <- bw |> filter(doc_id == id, outcome == dv)
  if (nrow(r) == 0) return("--")
  switch(what, actual = if (isTRUE(r$outside_pi95)) paste0("\\textbf{", fmt(r$actual), "}") else fmt(r$actual),
         pred = fmt(r$pred), pi = paste0("[", fmt(r$pi95_lo), ", ", fmt(r$pi95_hi), "]")) })
pi_block <- function(id, label) c(paste0(label, " & ", paste(cell(id, "actual"), collapse = " & "), " \\\\"),
                                  paste0("\\quad predicted & ", paste(cell(id, "pred"), collapse = " & "), " \\\\"),
                                  paste0("\\quad 95\\% PI & ", paste(cell(id, "pi"), collapse = " & "), " \\\\[2pt]"))
lla_ids <- scores |> filter(milei == 1) |> arrange(date, district, stage)
lla_pi <- unlist(map(seq_len(nrow(lla_ids)), function(i) with(lla_ids[i, ], pi_block(doc_id, paste0("LLA ", plat_label("", date, district, stage) |> trimws())))))
us_pi  <- unlist(map(seq_len(nrow(us)), function(i) pi_block(us$doc_id[i], paste0("US Republicans ", us$year[i], if (us$year[i] == 2020) " (2016 re-adopted)" else ""))))
sig <- boot |> filter(set == "argentina_oos") |> distinct(outcome, sigma_resid, n_obs)
note_pi <- paste0("\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}\\scriptsize\\textit{Notes:} First line: scored liberalism (bold if outside the 95\\% prediction interval). ",
  "Predicted: column (5) of Tables 4--8 (populism, ideology and their interaction; country and year fixed effects; estimated on the baseline manifestos, $N$ = ",
  paste(unique(fmtn(sig$n_obs)), collapse = "--"), "). 95\\% PI: prediction $\\pm 1.96\\sqrt{\\widehat{\\mathrm{Var}}_{\\mathrm{boot}} + \\hat\\sigma^2}$, ",
  "where $\\widehat{\\mathrm{Var}}_{\\mathrm{boot}}$ is the variance of the prediction over ", B_BOOT, " bootstrap draws that resample parties (Manifesto Project party codes) with replacement ",
  "and $\\hat\\sigma$ is the residual standard deviation of the baseline fit (", paste(sprintf("%s %.2f", c("political", "social", "economic", "financial", "overall"), sig$sigma_resid[match(DV5, sig$outcome)]), collapse = ", "),
  "); bounds are not truncated to 0--10. The Argentina fixed effect rests on ", fe_sup$n_manifestos[1], " baseline manifestos, all with ideology below zero; the 2025 year fixed effect on the ",
  "German federal election of February 2025.")
pi_table <- function(rows, note, label) c("\\begin{table}[htbp]\\centering\\scriptsize\\setlength{\\tabcolsep}{4pt}",
  paste0("\\caption{Predicted Liberalism of \\emph{La Libertad Avanza} with 95\\% Prediction Intervals}\\label{", label, "}"),
  "\\begin{tabular}{@{}lccccc@{}}\\toprule", " & \\multicolumn{5}{c}{Liberalism} \\\\ \\cmidrule(lr){2-6}",
  " & Political & Social & Economic & Financial & Overall \\\\ \\midrule", rows, "\\bottomrule\\end{tabular}", paste0(note, "\\end{minipage}"), "\\end{table}")
write_tex(pi_table(lla_pi, note_pi, "tab:argentina_lla_intervals"), file.path(OUT, "tables", "tab_argentina_lla_intervals.tex"))
write_tex(pi_table(c("\\multicolumn{6}{@{}l}{\\textit{Panel A: La Libertad Avanza (out of sample)}} \\\\", lla_pi, "\\midrule",
                     "\\multicolumn{6}{@{}l}{\\textit{Panel B: US Republicans (in sample)}} \\\\", us_pi),
                   paste0(note_pi, " Panel B: Republican Party platforms in the baseline (the 2020 convention re-adopted the 2016 platform); their predictions include their own residual in the fit and are in sample."),
                   "tab:argentina_lla_intervals"), file.path(OUT, "tables", "tab_argentina_lla_intervals_US.tex"))

# ---- 5c. All 38 Argentine platforms 2021-2025: predicted vs scored (online appendix; PLLM-0241) --
DV3 <- c(liberalism_overall = "Overall", lib_economic = "Economic", lib_financial_market = "Financial markets")
a38 <- scores |> select(doc_id, abbrev, alliance, date, stage, district, milei, populism_overall, ideology = pop_ideology_direction) |>
  mutate(platform = plat_label(abbrev, date, district, stage)) |>
  crossing(outcome = names(DV3)) |>
  left_join(pred |> select(doc_id, outcome, pred = pred_interaction, resid = resid_interaction, resid_sd_units = resid_interaction_sd,
                           resid_sd_model = resid_sd_interaction, pi95_lo = pi95_lo_interaction, pi95_hi = pi95_hi_interaction),
            by = c("doc_id", "outcome")) |>
  mutate(actual = map2_dbl(doc_id, outcome, ~ scores[[.y]][scores$doc_id == .x])) |>
  relocate(actual, .before = pred) |> arrange(date, stage != "PASO", district, doc_id, match(outcome, names(DV3)))
write_csv_det(a38, file.path(CSV, "53_argentina_all38.csv"))
summ_grp <- function(d, grp) {
  oth <- d |> filter(milei == 0, !is.na(resid_sd_units)); l <- d |> filter(milei == 1, !is.na(resid_sd_units))
  m <- mean(oth$resid_sd_units); s <- sd(oth$resid_sd_units)
  bind_rows(
    tibble(group = grp, row = "others", platform = "non-LLA platforms", n = nrow(oth), resid_sd_units = m, sd_others = s,
           z_vs_others = NA_real_, rank_desc = NA_integer_, n_ranked = NA_integer_),
    tibble(group = grp, row = "LLA", platform = l$platform, n = 1L, resid_sd_units = l$resid_sd_units, sd_others = s,
           z_vs_others = (l$resid_sd_units - m) / s,
           rank_desc = map_int(l$resid_sd_units, ~ 1L + sum(oth$resid_sd_units > .x)), n_ranked = nrow(oth) + 1L),
    tibble(group = grp, row = "LLA mean", platform = "LLA, mean of platforms", n = nrow(l), resid_sd_units = mean(l$resid_sd_units),
           sd_others = s, z_vs_others = (mean(l$resid_sd_units) - m) / s, rank_desc = NA_integer_, n_ranked = NA_integer_))
}
a38_sum <- map_dfr(names(DV3), function(dv) {
  d <- a38 |> filter(outcome == dv)
  bind_rows(summ_grp(d, "all platforms"),
            summ_grp(d |> filter(milei == 1 | ideology > 0), "right of centre (ideology > 0)"),
            summ_grp(d |> filter(milei == 1 | ideology >= 0), "ideology >= 0")) |> mutate(outcome = dv, .before = 1)
}) |> mutate(across(where(is.double), ~ ifelse(is.finite(.x), .x, NA_real_)))
write_csv_det(a38_sum, file.path(CSV, "53_argentina_all38_summary.csv"))
ids38 <- a38 |> distinct(doc_id, platform, milei, ideology)
row38 <- map_chr(seq_len(nrow(ids38)), function(i) {
  d <- a38 |> filter(doc_id == ids38$doc_id[i])
  cells <- unlist(map(names(DV3), function(dv) { r <- d |> filter(outcome == dv); c(fmt(r$pred), fmt(r$actual), fmt(r$resid_sd_units)) }))
  lab <- if (ids38$milei[i] == 1) paste0("\\textbf{", ids38$platform[i], "}") else ids38$platform[i]
  paste0(gsub("&", "\\\\&", lab), " & ", fmt(ids38$ideology[i]), " & ", paste(cells, collapse = " & "), " \\\\") })
srow <- function(grp, lab) {
  x <- a38_sum |> filter(group == grp)
  oth <- map_chr(names(DV3), ~ { r <- x |> filter(outcome == .x, row == "others"); paste0(fmt(r$resid_sd_units), " (", fmt(r$sd_others), ")") })
  lm_ <- map_chr(names(DV3), ~ { r <- x |> filter(outcome == .x, row == "LLA mean"); paste0(fmt(r$resid_sd_units), " [", fmt(r$z_vs_others), "]") })
  nn <- x |> filter(row == "others") |> pull(n) |> unique()
  c(paste0("\\multicolumn{2}{@{}l}{", lab, ", others: mean (SD), $n$ = ", paste(nn, collapse = "/"), "} & ", paste0("& & ", oth, collapse = " & "), " \\\\"),
    paste0("\\multicolumn{2}{@{}l}{", lab, ", LLA: mean [$z$ vs others]} & ", paste0("& & ", lm_, collapse = " & "), " \\\\"))
}
tex38 <- c("\\begin{table}[htbp]\\centering\\scriptsize\\setlength{\\tabcolsep}{3pt}",
  "\\caption{All Argentine Platforms 2021--2025: Predicted and Scored Liberalism}\\label{tab:argentina_all38}",
  "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%", "\\begin{tabular}{@{}lrrrrrrrrrr@{}}\\toprule",
  paste0(" & & ", paste0("\\multicolumn{3}{c}{", DV3, "}", collapse = " & "), " \\\\ \\cmidrule(lr){3-5}\\cmidrule(lr){6-8}\\cmidrule(lr){9-11}"),
  paste0("Platform & Ideology & ", paste(rep("Pred. & Scored & Resid.", 3), collapse = " & "), " \\\\ \\midrule"),
  row38, "\\midrule", srow("all platforms", "All"), srow("right of centre (ideology > 0)", "Ideology $> 0$"), srow("ideology >= 0", "Ideology $\\geq 0$"),
  "\\bottomrule\\end{tabular}", "}",
  paste0("\\begin{minipage}{0.97\\textwidth}\\vspace{2pt}\\scriptsize\\textit{Notes:} All ", nrow(ids38), " platforms filed with the C\\'amara Nacional Electoral for the 2021, 2023 and 2025 elections ",
         "(CF = Capital Federal, BA = Buenos Aires province, national = 2023 national platform; PASO = primaries), scored out of sample; La Libertad Avanza in bold. ",
         "Pred.: column (5) of Tables 4--8 (populism $\\times$ ideology, country and year fixed effects, estimated on the baseline); -- where the platform has no ideology score. ",
         "Resid.: scored minus predicted in residual standard deviations of the baseline fit. Bottom rows: mean and standard deviation of the residuals of the non-LLA platforms ",
         "with a prediction ($n$ per outcome), and the mean LLA residual with its distance from the non-LLA mean in non-LLA standard deviations; ",
         "rows restricted by ideology keep LLA and the other platforms with ideology above (at or above) zero.\\end{minipage}"),
  "\\end{table}")
write_tex(tex38, file.path(OUT, "tables", "tab_argentina_all38.tex"))
log_lines <- c(log_lines, "", capture.output(print(as.data.frame(a38_sum |> mutate(across(where(is.double), ~ round(.x, 3)))), width = 200)))
log_lines <- c(log_lines, "", capture.output(print(as.data.frame(boot |> filter(set != "argentina_oos" | doc_id %in% lla$doc_id) |>
  select(doc_id, outcome, actual, pred, se_pred_boot, sigma_resid, pi95_lo, pi95_hi, pi95pct_lo, pi95pct_hi) |>
  mutate(across(where(is.double), ~ round(.x, 3)))), width = 200)))
log_lines <- c(log_lines, "", capture.output(print(lla |> select(doc_id, populism_overall, ideology, lib_economic, liberalism_overall), width = 200)))
write_tex(log_lines, file.path(OUT, "logs", "53_argentina.txt"))
message("R/53 done -> ", OUT)
