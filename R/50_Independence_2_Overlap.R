# ============================================================
# 50_Independence_2_Overlap.R
#
# TEST 2 — Evidence-overlap and shared-method (MTMM) diagnostics on the
# EXISTING per-chunk annotations (zero API cost).
#
# 2a  Evidence overlap: within the same chunk and model, how often do a
#     populism dimension and a liberalism dimension cite the SAME or
#     overlapping text? Benchmarked against overlap among liberalism
#     sub-dimensions themselves and a positional null.
# 2b  Multitrait-multimethod (Campbell & Fiske 1959): traits = populism,
#     liberalism; methods = the three LLMs. A model-specific halo implies
#     heterotrait-MONOmethod r(pop_A, lib_A) more negative than
#     heterotrait-HETEROmethod r(pop_A, lib_B). Computed at chunk level
#     (identical chunk text across models) and manifesto level, raw and
#     country-year demeaned.
# 2c  Cross-rater regressions: headline spec with populism from model A
#     and liberalism from model B (A != B), the standard remedy for
#     common-method variance (Podsakoff et al. 2003).
# 2d  Within- vs between-manifesto correlation of chunk scores.
#
# Inputs (read-only): website/data/raw_{sonnet,gpt41mini,gemini}.parquet
#   (= HF raw_*.parquet), data/CMP/full_data.parquet (text, for positions);
#   IND_BASE=v2026a: data/CMP/v2026a/raw_*.parquet and the corpus 2026-1 text
# ============================================================

source("R/50_Independence_0_Setup.R")
suppressPackageStartupMessages(library(stringi))

MODELS <- if (IND_BASE == "v2026a") c(sonnet = v26_file("raw", "claude"),
                                       gpt41mini = v26_file("raw", "gpt41mini"),
                                       gemini = v26_file("raw", "gemini")) else
  c(sonnet = "website/data/raw_sonnet.parquet",
    gpt41mini = "website/data/raw_gpt41mini.parquet",
    gemini = "website/data/raw_gemini.parquet")
POP_D <- c("pop_anti_elitism", "pop_people_centrism", "pop_manichean", "populism_overall")
LIB_D <- c("lib_political", "lib_social", "lib_economic", "lib_financial_market", "liberalism_overall")

raw <- imap_dfr(MODELS, function(p, m) {
  read_parquet(p) |>
    filter(is.na(scoring_error) | !nzchar(scoring_error %||% "")) |>
    distinct(doc_id, .keep_all = TRUE) |>
    mutate(model = m, .before = 1)
})
message("chunk rows (all models): ", nrow(raw))

# ------------------------------------------------------------
# 2a. Evidence overlap
# ------------------------------------------------------------
norm_q <- function(x) {
  x <- stri_trans_tolower(coalesce(x, ""))
  x <- stri_replace_all_regex(x, "[\\p{P}\\p{S}]+", " ")
  stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))
}
word_jaccard <- function(a, b) {
  map2_dbl(stri_split_fixed(a, " "), stri_split_fixed(b, " "), function(x, y) {
    x <- unique(x[nzchar(x)]); y <- unique(y[nzchar(y)])
    if (!length(x) || !length(y)) return(NA_real_)
    length(intersect(x, y)) / length(union(x, y))
  })
}

# Long table of (chunk, model, dim) quotes with verified/mixed status
qlong <- map_dfr(c(POP_D, LIB_D), function(d) {
  raw |>
    transmute(model, doc_id, doc_id_root, dim = d,
              status = .data[[paste0(d, "_status")]],
              quote = .data[[paste0(d, "_quote")]],
              context = .data[[paste0(d, "_context")]])
}) |>
  filter(status %in% c("verified", "mixed"), !is.na(quote), nzchar(quote)) |>
  mutate(qn = norm_q(quote), cn = norm_q(context))

# Positions: locate each quote in the full manifesto text (first exact or
# case-insensitive occurrence) to measure positional overlap.
txt <- if (IND_BASE == "v2026a") {
  # corpus 2026-1 text (local only, never written out), cleaned as in R/30
  source("R/2_Helpers.R")
  read_parquet("data/CMP/update_2026a/corpus_texts_20260904101355.parquet", col_select = c("doc_id", "text_raw")) |>
    filter(!is.na(doc_id)) |> distinct(doc_id, .keep_all = TRUE) |>
    transmute(doc_id, text = map_chr(text_raw, clean_manifesto_text))
} else read_parquet("data/CMP/full_data.parquet", col_select = c("doc_id", "text")) |>
  distinct(doc_id, .keep_all = TRUE)
txt_lookup <- setNames(txt$text, txt$doc_id)
loc <- stri_locate_first_fixed(txt_lookup[qlong$doc_id_root], qlong$quote,
                               opts_fixed = stri_opts_fixed(case_insensitive = TRUE))
qlong <- qlong |>
  mutate(start = loc[, 1], end = loc[, 2],
         doc_len = stri_length(txt_lookup[doc_id_root])) |>
  # effective chunk length for the positional null (chunks ~ equal length)
  left_join(raw |> count(model, doc_id_root, name = "n_chunks"), by = c("model", "doc_id_root")) |>
  mutate(chunk_len = doc_len / n_chunks)
message(sprintf("quotes located in manifesto text: %.1f%%", 100 * mean(!is.na(qlong$start))))

pair_overlap <- function(set_a, set_b, label) {
  a <- qlong |> filter(dim %in% set_a)
  b <- qlong |> filter(dim %in% set_b)
  inner_join(a, b, by = c("model", "doc_id", "doc_id_root"), suffix = c("_a", "_b"),
             relationship = "many-to-many") |>
    filter(dim_a != dim_b) |>
    # unordered pairs within the same family
    filter(!(set_a[1] == set_b[1] & dim_a > dim_b)) |>
    mutate(
      identical      = qn_a == qn_b,
      nested         = !identical & (stri_detect_fixed(qn_a, qn_b) | stri_detect_fixed(qn_b, qn_a)),
      jaccard        = word_jaccard(qn_a, qn_b),
      quote_in_ctx   = stri_detect_fixed(cn_b, qn_a) | stri_detect_fixed(cn_a, qn_b),
      both_located   = !is.na(start_a) & !is.na(start_b),
      span_overlap   = both_located & start_a <= end_b & start_b <= end_a,
      near_300       = both_located & abs(start_a - start_b) <= 300,
      # positional null: P(|U1-U2| <= 300) for independent uniform positions
      near_300_null  = pmin(1, 2 * 300 / chunk_len_a - (300 / chunk_len_a)^2),
      pair_type = label
    )
}

ov <- bind_rows(
  pair_overlap(POP_D[1:3], LIB_D[1:4], "populism sub-dim x liberalism sub-dim"),
  pair_overlap(LIB_D[1:4], LIB_D[1:4], "liberalism sub-dim x liberalism sub-dim (benchmark)"),
  pair_overlap(POP_D[1:3], POP_D[1:3], "populism sub-dim x populism sub-dim (benchmark)")
)

ov_sum <- ov |>
  group_by(pair_type, model) |>
  summarise(n_pairs = n(),
            pct_identical = 100 * mean(identical),
            pct_nested = 100 * mean(nested),
            pct_jaccard_ge_.5 = 100 * mean(jaccard >= 0.5, na.rm = TRUE),
            pct_quote_in_other_ctx = 100 * mean(quote_in_ctx),
            pct_span_overlap = 100 * mean(span_overlap[both_located]),
            pct_within_300ch = 100 * mean(near_300[both_located]),
            pct_within_300ch_null = 100 * mean(near_300_null[both_located]),
            .groups = "drop")
write_tab(ov_sum, "t2a_evidence_overlap", digits = 2, label = "tab:indep:overlap",
          caption = "Evidence overlap between dimension pairs within the same chunk and model",
          note = "Pairs of evidence quotes (status verified or mixed) given for two dimensions in the same chunk by the same model. Percent of pairs that are identical, nested, have Jaccard word overlap of at least 0.5, where one quote lies in the other's context, whose spans overlap in the text, and that lie within 300 characters of each other (random pairs from different chunks of the same manifesto as the null). Benchmarks: pairs within the same construct.")
print(ov_sum, width = 250)

ov_dim <- ov |>
  filter(pair_type == "populism sub-dim x liberalism sub-dim") |>
  group_by(dim_a, dim_b) |>
  summarise(n_pairs = n(), pct_identical_or_nested = 100 * mean(identical | nested),
            pct_span_overlap = 100 * mean(span_overlap[both_located]),
            pct_within_300ch = 100 * mean(near_300[both_located]), .groups = "drop")
write_tab(ov_dim, "t2a_evidence_overlap_by_dim", digits = 2,
          caption = "Populism x liberalism evidence overlap by dimension pair (pooled over models).")
print(ov_dim)

# ------------------------------------------------------------
# 2b. MTMM: chunk level (same chunk text across models)
# ------------------------------------------------------------
chunk_w <- raw |>
  transmute(model, doc_id, doc_id_root,
            pop = if_else(populism_overall_status == "verified", populism_overall_score, NA_real_),
            lib = if_else(liberalism_overall_status == "verified", liberalism_overall_score, NA_real_)) |>
  pivot_wider(names_from = model, values_from = c(pop, lib))

mtmm <- function(d, level, demean_by = NULL) {
  ms <- names(MODELS)
  if (!is.null(demean_by)) {
    cols <- c(paste0("pop_", ms), paste0("lib_", ms))
    d <- d |> filter(if_all(all_of(cols), is.finite))
    for (v in cols) d[[v]] <- as.numeric(fixest::demean(d[[v]], d[, demean_by]))
  }
  expand_grid(pop_model = ms, lib_model = ms) |>
    pmap_dfr(function(pop_model, lib_model) {
      a <- d[[paste0("pop_", pop_model)]]; b <- d[[paste0("lib_", lib_model)]]
      ok <- is.finite(a) & is.finite(b)
      tibble(level = level, pop_model, lib_model, same_model = pop_model == lib_model,
             n = sum(ok), r = cor(a[ok], b[ok]))
    })
}

# Manifesto level: per-model cwm scores from the baseline file
base <- load_baseline()
man_w <- base |>
  transmute(doc_id, countryname, year,
            pop_sonnet = populism_overall_sonnet, pop_gpt41mini = populism_overall_gpt41mini,
            pop_gemini = populism_overall_gemini,
            lib_sonnet = liberalism_overall_sonnet, lib_gpt41mini = liberalism_overall_gpt41mini,
            lib_gemini = liberalism_overall_gemini)

mt <- bind_rows(
  mtmm(chunk_w, "chunk"),
  mtmm(chunk_w, "chunk, within-manifesto", demean_by = "doc_id_root"),
  mtmm(man_w, "manifesto"),
  mtmm(man_w, "manifesto, within country-year", demean_by = c("countryname", "year"))
)
# Reliability benchmarks: same-trait cross-model correlations (validity diagonal)
rel <- bind_rows(
  map_dfr(list(list(chunk_w, "chunk"), list(man_w, "manifesto")), function(z) {
    d <- z[[1]]; ms <- names(MODELS)
    combn(ms, 2, simplify = FALSE) |> map_dfr(function(p) tibble(
      level = z[[2]], pair = paste(p, collapse = "-"),
      r_pop = cor(d[[paste0("pop_", p[1])]], d[[paste0("pop_", p[2])]], use = "complete.obs"),
      r_lib = cor(d[[paste0("lib_", p[1])]], d[[paste0("lib_", p[2])]], use = "complete.obs")))
  }))
write_tab(rel, "t2b_cross_model_reliability", digits = 3,
          caption = "Cross-model agreement (same trait, different LLM).")

mt_sum <- mt |>
  group_by(level, same_model) |>
  summarise(mean_r = mean(r), min_r = min(r), max_r = max(r), .groups = "drop") |>
  pivot_wider(names_from = same_model, values_from = c(mean_r, min_r, max_r)) |>
  rename_with(~ sub("_TRUE$", "_monomethod", sub("_FALSE$", "_heteromethod", .x)))
# Disattenuated comparison: hetero-method r / sqrt(rel_pop * rel_lib) is the
# trait correlation implied without shared method; compare to mono-method r.
rel_mean <- rel |> group_by(level) |> summarise(rel_pop = mean(r_pop), rel_lib = mean(r_lib))
mt_sum <- mt_sum |>
  left_join(rel_mean |> rename(lvl = level), by = c("level" = "lvl"))
write_tab(mt, "t2b_mtmm_full", digits = 3, caption = "Multitrait-multimethod correlations of populism and liberalism, all model pairs.")
write_tab(mt_sum, "t2b_mtmm_summary", digits = 3, label = "tab:indep:mtmm",
          caption = "Multitrait-multimethod: populism-liberalism correlation within the same LLM and across LLMs",
          note = "Same LLM (mono-method): populism and liberalism both scored by one model; across LLMs (hetero-method): populism from one model, liberalism from another. A model-specific halo would make the same-LLM correlation more negative. Reliability: cross-model agreement of each trait.")
print(mt_sum, width = 250)

# ------------------------------------------------------------
# 2c. Cross-rater regressions (headline spec, z-scored)
# ------------------------------------------------------------
xr <- expand_grid(pop_model = names(MODELS), lib_model = names(MODELS)) |>
  pmap_dfr(function(pop_model, lib_model) {
    d <- base |> mutate(pop = .data[[paste0("populism_overall_", pop_model)]],
                        lib = .data[[paste0("liberalism_overall_", lib_model)]])
    m  <- std_fe_reg(d, "lib", "pop", cluster = ~party)
    mr <- std_fe_reg(d, "lib", "pop", cluster = ~party, standardize = FALSE)
    tidy_coef(m, "pop") |>
      mutate(b_raw = coef(mr)[["pop"]], se_raw = se(mr)[["pop"]]) |>
      mutate(pop_model, lib_model, same_model = pop_model == lib_model, .before = 1)
  })
# Leave-one-out: populism = mean of the two OTHER models, liberalism = held-out model
loo <- map_dfr(names(MODELS), function(m) {
  oth <- setdiff(names(MODELS), m)
  d <- base |> mutate(pop = rowMeans(across(all_of(paste0("populism_overall_", oth))), na.rm = TRUE),
                      lib = .data[[paste0("liberalism_overall_", m)]])
  mm <- std_fe_reg(d, "lib", "pop", cluster = ~party)
  mr <- std_fe_reg(d, "lib", "pop", cluster = ~party, standardize = FALSE)
  tidy_coef(mm, "pop") |> mutate(b_raw = coef(mr)[["pop"]], se_raw = se(mr)[["pop"]]) |>
    mutate(pop_model = paste(oth, collapse = "+"), lib_model = m, same_model = FALSE, .before = 1)
})
xr_all <- bind_rows(xr, loo) |> mutate(sig = stars(p))
write_tab(xr_all, "t2c_cross_rater_regressions", digits = 3, label = "tab:indep:crossrater",
          caption = "Liberalism (overall) on populism, each scored by a different LLM",
          note = "Both variables z-scored; country and year fixed effects; SE clustered by party. Raw: unstandardised estimate on the 0-10 scales.")
print(xr_all |> select(pop_model, lib_model, estimate, se, b_raw, n), n = 20)

# ------------------------------------------------------------
# 2d. Within- vs between-manifesto correlation (chunk scores)
# ------------------------------------------------------------
wb <- raw |>
  transmute(model, doc_id_root,
            pop = if_else(populism_overall_status == "verified", populism_overall_score, NA_real_),
            lib = if_else(liberalism_overall_status == "verified", liberalism_overall_score, NA_real_)) |>
  filter(is.finite(pop), is.finite(lib)) |>
  group_by(model, doc_id_root) |>
  mutate(n_ch = n(), pop_w = pop - mean(pop), lib_w = lib - mean(lib)) |>
  ungroup()
wb_sum <- wb |>
  group_by(model) |>
  summarise(
    r_within_manifesto = cor(pop_w[n_ch > 1], lib_w[n_ch > 1]),
    n_chunks_multi = sum(n_ch > 1),
    sd_pop_within = sd(pop_w[n_ch > 1]), sd_lib_within = sd(lib_w[n_ch > 1]),
    .groups = "drop") |>
  left_join(wb |> group_by(model, doc_id_root) |> summarise(pop = mean(pop), lib = mean(lib), .groups = "drop") |>
              group_by(model) |> summarise(r_between_manifesto = cor(pop, lib), n_manifestos = n()),
            by = "model")
write_tab(wb_sum, "t2d_within_between", digits = 3,
          caption = "Correlation of chunk-level populism and liberalism scores within manifestos (chunk deviations from manifesto mean) vs between manifestos.")
print(wb_sum)

saveRDS(list(ov_sum = ov_sum, ov_dim = ov_dim, mt = mt, mt_sum = mt_sum, rel = rel,
             xr = xr_all, wb = wb_sum), file.path(IND_CACHE, "t2_results.rds"))
message("Test 2 done.")
