# ============================================================
# 50_Independence_3c_Compare.R
#
# TEST 3, step c — compare separate-call vs joint-call scores and the
# populism -> liberalism slope on the rescored subsample.
#
# NOT A CHAIN STAGE. Archived analysis of the 2026-10-07/08 separate-call rescoring (API module,
# outputs in output/independence/). It DELIBERATELY uses the June-2026 baseline rule below (the
# production rule at the time; = AGG_RULES$june in R/1_Aggregation.R, applied to the arms' own
# chunk files) and its own span-validated variant, so that the Oct-8 results stay reproducible.
# It does not use R/1_Aggregation.R.
#
# Aggregation (baseline rule, website/data/_build_baseline.R):
#   chunk score enters iff evidence_status == "verified";
#   within-manifesto confidence-weighted mean, w = pmax(conf, 0.1).
#   Weight column: populism_confidence (production) for joint and pop
#   arms; liberalism_confidence for the lib arm (no populism_confidence
#   exists there). 85% of the sample is single-chunk, where weights are
#   irrelevant; a sensitivity uses liberalism_confidence for joint too.
# Span-validated variant: additionally require the quote to be verbatim
#   in the chunk and not a substring of the arm's own system prompt
#   (R/12 deterministic checks; no LLM judge).
#
# Outputs: t3_agreement, t3_halo_shift, t3_slopes, t3_slopes_subdims,
#          t3_slope_bootstrap (tables), f3_scatter_sep_vs_joint.pdf
# ============================================================

source("R/50_Independence_0_Setup.R")
source("R/50_Independence_3_Prompts.R")
source("R/12_Validate_Span_Quality.R")

RAW_DIR <- file.path(IND_CACHE, "t3_raw")
samp <- read_csv(file.path(IND_CACHE, "t3_sample.csv"), show_col_types = FALSE)
base <- load_baseline()
DIMS_POP <- c("populism_overall", "pop_anti_elitism", "pop_people_centrism", "pop_manichean")
DIMS_LIB <- c("liberalism_overall", "lib_political", "lib_social", "lib_economic", "lib_financial_market")
DIMS_IDE <- c("pop_ideology_left", "pop_ideology_right", "pop_ideology_centrist")

load_arm <- function(arm_id) {
  fs <- list.files(file.path(RAW_DIR, arm_id), pattern = "\\.rds$", full.names = TRUE)
  map_dfr(fs, ~ readRDS(.x)$raw)
}

span_ok <- function(raw, dims, sysp) {
  for (d in dims) {
    q <- raw[[paste0(d, "_quote")]]
    ok <- map2_lgl(q, raw$chunk_text, function(qq, ct)
      !hallucination_check(qq, sysp) && isTRUE(verbatim_check(qq, ct)$found))
    raw[[paste0(d, "_spanok")]] <- ok
  }
  raw
}

aggregate_arm <- function(raw, dims, wcol, span_validated = FALSE) {
  rows <- raw |> distinct(doc_id_root) |> rename(doc_id = doc_id_root)
  for (d in intersect(dims, sub("_score$", "", grep("_score$", names(raw), value = TRUE)))) {
    sc <- paste0(d, "_score"); st <- paste0(d, "_status")
    r <- raw |> filter(.data[[st]] == "verified", !is.na(.data[[sc]]))
    if (span_validated) r <- r |> filter(.data[[paste0(d, "_spanok")]])
    agg <- r |> group_by(doc_id_root) |>
      summarise(!!d := weighted.mean(.data[[sc]], w = pmax(coalesce(.data[[wcol]], 0), 0.1)), .groups = "drop") |>
      rename(doc_id = doc_id_root)
    rows <- left_join(rows, agg, by = "doc_id")
  }
  rows
}

# Per-model score sets: production (from baseline file), joint rerun, separate
build_sets <- function(model, span_validated = FALSE, joint_w = "populism_confidence") {
  sfx <- c(gpt41mini = "gpt41mini", sonnet = "sonnet")[[model]]
  j <- load_arm(paste0(model, "_joint")); p <- load_arm(paste0(model, "_pop")); l <- load_arm(paste0(model, "_lib"))
  if (span_validated) {
    j <- span_ok(j, c(DIMS_POP, DIMS_LIB), system_prompt_v4_safe("X", 2000))
    p <- span_ok(p, DIMS_POP, system_prompt_pop_only("X", 2000))
    l <- span_ok(l, DIMS_LIB, system_prompt_lib_only("X", 2000))
  }
  prod <- base |> select(doc_id, all_of(paste0(c(DIMS_POP, DIMS_LIB), "_", sfx))) |>
    rename_with(~ sub(paste0("_", sfx, "$"), "", .x))
  jp <- aggregate_arm(j, DIMS_POP, "populism_confidence", span_validated)
  jl <- aggregate_arm(j, DIMS_LIB, joint_w, span_validated)
  list(production = prod,
       joint = full_join(jp, jl, by = "doc_id"),
       separate = full_join(aggregate_arm(p, DIMS_POP, "populism_confidence", span_validated),
                            aggregate_arm(l, DIMS_LIB, "liberalism_confidence", span_validated), by = "doc_id"))
}

# ------------------------------------------------------------
# Agreement: separate vs joint-rerun vs production
# ------------------------------------------------------------
agree <- function(sets, model, variant) {
  pairs <- list(c("separate", "joint"), c("joint", "production"), c("separate", "production"))
  map_dfr(pairs, function(pr) map_dfr(c(DIMS_POP, DIMS_LIB), function(d) {
    a <- sets[[pr[1]]] |> select(doc_id, a = all_of(d))
    b <- sets[[pr[2]]] |> select(doc_id, b = all_of(d))
    x <- inner_join(a, b, by = "doc_id") |> filter(is.finite(a), is.finite(b))
    tibble(model, variant, comparison = paste(pr, collapse = " vs "), dim = d, n = nrow(x),
           r = cor(x$a, x$b), MAD = mean(abs(x$a - x$b)), mean_diff = mean(x$a - x$b),
           p_mean_diff = tryCatch(t.test(x$a - x$b)$p.value, error = function(e) NA_real_))
  }))
}

# ------------------------------------------------------------
# Slopes on the subsample: (pop source, lib source) combinations
# ------------------------------------------------------------
make_combo <- function(sets, pop_src, lib_src, lib_dim = "liberalism_overall") {
  sets[[pop_src]] |> select(doc_id, pop = populism_overall) |>
    inner_join(sets[[lib_src]] |> select(doc_id, lib = all_of(lib_dim)), by = "doc_id") |>
    inner_join(samp |> select(doc_id, countryname, decade, stratum), by = "doc_id") |>
    filter(is.finite(pop), is.finite(lib))
}
slope <- function(d, fe = TRUE, standardize = FALSE) {
  if (standardize) d <- d |> mutate(across(c(pop, lib), ~ as.numeric(scale(.x))))
  m <- if (fe) feols(lib ~ pop | countryname + decade, d, vcov = "hetero")
       else    feols(lib ~ pop, d, vcov = "hetero")
  tibble(b = coef(m)[["pop"]], se = se(m)[["pop"]], n = nobs(m))
}
COMBOS <- tribble(~pop_src, ~lib_src,
  "production", "production", "joint", "joint", "separate", "separate",
  "joint", "separate", "separate", "joint", "production", "separate", "separate", "production")

slopes_for <- function(sets, model, variant, docs = NULL) {
  pmap_dfr(COMBOS, function(pop_src, lib_src) {
    d <- make_combo(sets, pop_src, lib_src)
    if (!is.null(docs)) d <- d |> filter(doc_id %in% docs)
    bind_rows(slope(d, fe = FALSE) |> mutate(spec = "OLS, raw 0-10"),
              slope(d, fe = TRUE) |> mutate(spec = "country + decade FE, raw 0-10"),
              slope(d, fe = TRUE, standardize = TRUE) |> mutate(spec = "country + decade FE, z-scored")) |>
      mutate(model, variant, pop_src, lib_src, .before = 1)
  })
}

# Common doc set per model: manifestos with finite pop & lib in all three sources,
# so every combo is estimated on the SAME manifestos.
common_docs <- function(sets) {
  reduce(map(sets, ~ .x |> filter(is.finite(populism_overall), is.finite(liberalism_overall)) |> pull(doc_id)),
         intersect)
}

boot_diff <- function(sets, docs, B = 2000, seed = 1L, fe = TRUE) {
  d <- make_combo(sets, "joint", "joint") |> rename(pop_j = pop, lib_j = lib) |>
    inner_join(make_combo(sets, "separate", "separate") |> select(doc_id, pop_s = pop, lib_s = lib), by = "doc_id") |>
    filter(doc_id %in% docs)
  est <- function(dd) {
    f <- if (fe) "| countryname + decade" else ""
    bj <- coef(feols(as.formula(paste("lib_j ~ pop_j", f)), dd))[["pop_j"]]
    bs <- coef(feols(as.formula(paste("lib_s ~ pop_s", f)), dd))[["pop_s"]]
    c(bj = bj, bs = bs, diff = bs - bj, ratio = bs / bj)
  }
  set.seed(seed)
  bt <- map_dfr(seq_len(B), function(i) {
    dd <- d[sample.int(nrow(d), replace = TRUE), ]
    tryCatch(as_tibble_row(suppressMessages(est(dd))), error = function(e) NULL)
  })
  pt <- est(d)
  tibble(stat = names(pt), estimate = pt,
         ci_lo = map_dbl(names(pt), ~ quantile(bt[[.x]], 0.025, na.rm = TRUE)),
         ci_hi = map_dbl(names(pt), ~ quantile(bt[[.x]], 0.975, na.rm = TRUE)),
         n = nrow(d), B = nrow(bt))
}

# Direct halo test: does the joint call pull liberalism DOWN where the
# (separately measured) populism is high?  (lib_joint - lib_sep) ~ pop_sep
halo_shift <- function(sets, model, variant) {
  bind_rows(
    map_dfr(DIMS_LIB, function(d) {
      x <- sets$separate |> select(doc_id, pop = populism_overall, sep = all_of(d)) |>
        inner_join(sets$joint |> select(doc_id, jnt = all_of(d)), by = "doc_id") |>
        filter(is.finite(pop), is.finite(sep), is.finite(jnt))
      m <- feols(I(jnt - sep) ~ pop, x, vcov = "hetero")
      tibble(model, variant, outcome = paste0(d, ": joint - separate"), regressor = "populism (separate)",
             b = coef(m)[["pop"]], se = se(m)[["pop"]], p = pvalue(m)[["pop"]], n = nobs(m))
    }),
    map_dfr(DIMS_POP[1], function(d) {
      x <- sets$separate |> select(doc_id, lib = liberalism_overall, sep = all_of(d)) |>
        inner_join(sets$joint |> select(doc_id, jnt = all_of(d)), by = "doc_id") |>
        filter(is.finite(lib), is.finite(sep), is.finite(jnt))
      m <- feols(I(jnt - sep) ~ lib, x, vcov = "hetero")
      tibble(model, variant, outcome = paste0(d, ": joint - separate"), regressor = "liberalism (separate)",
             b = coef(m)[["lib"]], se = se(m)[["lib"]], p = pvalue(m)[["lib"]], n = nobs(m))
    }))
}

models_avail <- c("gpt41mini", "sonnet")[map_lgl(c("gpt41mini", "sonnet"), function(m)
  all(dir.exists(file.path(RAW_DIR, paste0(m, c("_joint", "_pop", "_lib"))))))]

out <- list()
for (m in models_avail) {
  for (variant in c("baseline rule", "span-validated", "joint lib weighted by liberalism_confidence")) {
    sets <- switch(variant,
      "baseline rule" = build_sets(m),
      "span-validated" = build_sets(m, span_validated = TRUE),
      "joint lib weighted by liberalism_confidence" = build_sets(m, joint_w = "liberalism_confidence"))
    docs <- common_docs(sets)
    message(sprintf("%s | %s | common manifestos: %d", m, variant, length(docs)))
    out$agree  <- bind_rows(out$agree, agree(sets, m, variant))
    out$slopes <- bind_rows(out$slopes, slopes_for(sets, m, variant, docs) |> mutate(sample = "all"),
                            slopes_for(sets, m, variant, intersect(docs, samp$doc_id[samp$stratum == "single"])) |>
                              mutate(sample = "single-chunk"))
    out$halo   <- bind_rows(out$halo, halo_shift(sets, m, variant))
    if (variant == "baseline rule") {
      out$boot <- bind_rows(out$boot,
        boot_diff(sets, docs, fe = TRUE) |> mutate(model = m, spec = "country + decade FE", .before = 1),
        boot_diff(sets, docs, fe = FALSE) |> mutate(model = m, spec = "OLS", .before = 1))
      out$sub <- bind_rows(out$sub, map_dfr(DIMS_LIB, function(ld) map_dfr(c("joint", "separate", "production"), function(src) {
        d <- make_combo(sets, src, src, ld) |> filter(doc_id %in% docs)
        bind_rows(slope(d, fe = FALSE) |> mutate(spec = "OLS"),
                  slope(d, fe = TRUE) |> mutate(spec = "country + decade FE")) |>
          mutate(model = m, lib_dim = ld, source = src, .before = 1)
      })))
      out$sets[[m]] <- sets
    }
  }
}

write_tab(out$agree,  "t3_agreement", digits = 3,
          caption = "Agreement of separate-call vs joint-call (rerun) vs production scores, per dimension.")
write_tab(out$slopes, "t3_slopes", digits = 3,
          caption = "Liberalism (overall) on populism (overall) on the rescored subsample, by measurement source.")
write_tab(out$sub,    "t3_slopes_subdims", digits = 3,
          caption = "Sub-dimension slopes on the rescored subsample (raw 0-10; HC1 SE).")
write_tab(out$halo,   "t3_halo_shift", digits = 3,
          caption = "Direct halo test: joint-minus-separate score difference regressed on the other construct measured separately.")
write_tab(out$boot,   "t3_slope_bootstrap", digits = 3,
          caption = "Pairs bootstrap (B = 2000) of the joint vs separate slope: difference and ratio.")

# Compact paper table: slope by source (baseline rule, all docs, FE raw + z)
paper <- out$slopes |>
  filter(variant == "baseline rule", sample == "all") |>
  mutate(cell = sprintf("%.3f%s (%.3f)", b, stars(2 * pnorm(-abs(b / se))), se)) |>
  select(model, pop_src, lib_src, spec, cell, n) |>
  pivot_wider(names_from = spec, values_from = cell)
write_tab(paper, "t3_slopes_paper",
          caption = "Populism -> liberalism slope by measurement source on the rescored subsample. HC1 SE in parentheses.")
print(paper, width = 200)
print(out$boot)
print(out$halo |> filter(variant == "baseline rule"))
print(out$agree |> filter(variant == "baseline rule", dim %in% c("populism_overall", "liberalism_overall")))

# Scatter: separate vs joint, per model, populism and liberalism overall
sc <- map_dfr(names(out$sets), function(m) {
  s <- out$sets[[m]]
  bind_rows(
    inner_join(s$separate |> select(doc_id, sep = populism_overall), s$joint |> select(doc_id, jnt = populism_overall), by = "doc_id") |>
      mutate(dim = "Populism (overall)"),
    inner_join(s$separate |> select(doc_id, sep = liberalism_overall), s$joint |> select(doc_id, jnt = liberalism_overall), by = "doc_id") |>
      mutate(dim = "Liberalism (overall)")) |> mutate(model = m)
})
g <- ggplot(sc, aes(jnt, sep)) + geom_abline(linetype = 2, colour = "grey50") +
  geom_jitter(width = .08, height = .08, alpha = .5, size = 1) +
  facet_grid(model ~ dim) + coord_equal(xlim = c(0, 10), ylim = c(0, 10)) +
  labs(x = "Joint call (production prompt, rerun)", y = "Separate call") + theme_bw(base_size = 10)
ggsave(file.path(IND_FIG, "f3_scatter_sep_vs_joint.pdf"), g, width = 7, height = 6.5)

sl <- out$slopes |> filter(variant == "baseline rule", sample == "all", spec == "OLS, raw 0-10") |>
  mutate(combo = paste0("pop: ", pop_src, " / lib: ", lib_src))
g2 <- ggplot(sl, aes(b, fct_rev(combo))) + geom_vline(xintercept = 0, colour = "grey60") +
  geom_pointrange(aes(xmin = b - 1.96 * se, xmax = b + 1.96 * se)) + facet_wrap(~ model) +
  labs(x = "Slope of liberalism on populism, 0-10 scales, OLS (95% CI, HC1)", y = NULL) + theme_bw(base_size = 10)
ggsave(file.path(IND_FIG, "f3_slopes_by_source.pdf"), g2, width = 8, height = 3.5)

saveRDS(out, file.path(IND_CACHE, "t3_results.rds"))
message("Test 3 comparison done.")
