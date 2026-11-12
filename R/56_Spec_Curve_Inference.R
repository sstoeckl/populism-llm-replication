# ============================================================
# 56_Spec_Curve_Inference.R
#
# PLLM-0206 (2026-11-11): joint inference for the specification curve of R/42 and a
# decomposition of its model fork. The share of significant specifications describes the
# curve; it is not a test, because the specifications are highly correlated. This script adds
# the joint test of Simonsohn, Simmons & Nelson (2020, Nature Human Behaviour 4, 1208-1214,
# "Specification curve analysis", section "Inference").
#
# Part A, joint inference under the null (estimands pop and inter of R/42)
#   Specifications: the distinct specifications of R/42 (specs_distinct_v2026a.parquet, party
#   SE), one regression per beta_key = data_key x outcome x sample x fe x estimand. The SE fork
#   is not a separate regression; p-values use the party cluster throughout.
#   Null data: for specification k with estimated coefficient b_k on the term of interest x_k
#   (x = POP for pop, x = POP x IDEO for inter), y*_k = y - b_k * x_k. The null holds exactly in
#   every y*_k (the coefficient on x_k in the original sample is zero), and all other features
#   of the data (FE structure, residuals, the dependence between specifications) are kept.
#   Bootstrap unit: the CMP party code (the clustering level). Draw b samples parties with
#   replacement from all parties in the corpus (with text), once per draw, and the SAME draw is
#   applied to every specification, every dataset and every sample restriction: each
#   specification is then estimated on its own rows of one common bootstrap world, so the
#   joint dependence across specifications is preserved (this is stronger than "same draw for
#   specifications sharing a dataset"). A party drawn m times enters m times; each copy is a
#   separate cluster for the clustered SE (and, being identical, shares the party FE without
#   changing any estimate). Sample restrictions (post2000, post2010, europe_only) are applied
#   after the draw, so their number of clusters varies across draws.
#   Curve statistics per draw: the median coefficient, the share of specifications with a
#   negative coefficient, the share significantly negative (p < 0.05, party-clustered), and,
#   supplementary, the share significantly positive. Shares are over the specifications that
#   could be estimated in the draw (same rules as R/42: >= 100 complete cases, >= 100 obs
#   after fixest, term identified).
#   p-value = share of draws at least as extreme as the observed curve (median* <= observed,
#   share* >= observed); p_plus1 = (1 + count) / (1 + B) is the conservative version.
#   Implementation: for a given dataset x sample x FE x estimand, the outcomes share the
#   regressors and the FE; they are estimated together by a direct reimplementation of feols
#   (section 2: fixest::demean, OLS, fixest's clustered small-sample correction). Draw 0 is
#   the original sample with the original outcomes and must reproduce every R/42 coefficient
#   and party p-value (asserted), and with the null outcomes must give coefficients of zero.
#   Determinism: all party draws are generated up front from one seed (SEED); workers draw no
#   random numbers, so results do not depend on the number of workers (furrr seed = TRUE kept
#   as a guard).
#
# Part B, model-fork decomposition (pop, distinct specifications, party SE; no new regressions)
#   which model is included (Sonnet / GPT-4.1-mini / Gemini in the set vs not), how many models
#   (1, 2, 3), each of the seven model sets, and the overall median under three weightings:
#   cells (each distinct specification once, as in R/42), equal weight per model set (each of
#   the 7 sets 1/7), and per model (a set of k models counts as 1/k of a model, i.e. a single
#   model 1, a pair 1/2, the trio 1/3; within a set every specification has equal weight).
#   Weighted quantiles: smallest coefficient at which the cumulative weight reaches the level.
#
# Inputs:  PIPE_OUT/spec_curve/specs_distinct_v2026a.parquet, datasets_v2026a.parquet (R/42),
#          PIPE_META (party universe)
# Outputs: PIPE_OUT/csv/56_joint_inference.csv, 56_bootstrap_draws.csv,
#          56_model_fork_decomposition.csv,
#          PIPE_OUT/tables/tab_speccurve_joint_inference.tex, tab_speccurve_model_decomposition.tex,
#          PIPE_OUT/figures/spec_curve_null_distribution.pdf,
#          PIPE_OUT/logs/56_spec_curve_inference.txt
# Runtime: see the log (32 workers).
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(fixest); library(furrr); library(scales)
})
source("R/0_Config.R")
SC <- file.path(PIPE_OUT, "spec_curve")
t_start <- Sys.time()

B       <- as.integer(Sys.getenv("SC56_B", "500"))   # bootstrap draws (env override only for benchmarking)
SEED    <- 20261111L
WORKERS <- 32L
EUROPE  <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark",
             "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy",
             "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal",
             "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Iceland", "Norway", "Switzerland",
             "United Kingdom")                        # identical to R/42

# ---- 1. Inputs -------------------------------------------------------------------------------
ds <- read_parquet(file.path(SC, "specs_distinct_v2026a.parquet")) |>
  select(estimand, beta_key, data_key, outcome, sample_key, fe_key, model_set, coef, p, n)
stopifnot(all(ds$data_key == as.integer(sub("\\|.*", "", ds$beta_key))), !anyDuplicated(ds$beta_key))
dat_all <- read_parquet(file.path(SC, "datasets_v2026a.parquet"))
stopifnot(all(unique(ds$data_key) %in% dat_all$data_key))
dat_list <- split(select(dat_all, -data_key), dat_all$data_key)
dat_list <- dat_list[as.character(sort(unique(ds$data_key)))]
rm(dat_all)

# party universe: every CMP party code with a manifesto text
meta <- read_parquet(PIPE_META) |> filter(has_text) |> distinct(doc_id, .keep_all = TRUE)
PARTIES <- sort(unique(meta$party))
stopifnot(all(unlist(map(dat_list, ~ unique(.x$party))) %in% PARTIES))

# estimation groups: dataset x sample x FE x estimand, with the outcomes and their b_k
groups <- ds |> arrange(estimand, data_key, sample_key, fe_key, outcome) |>
  group_by(estimand, data_key, sample_key, fe_key) |>
  summarise(outcomes = list(outcome), b = list(coef), keys = list(beta_key), .groups = "drop")
spec_order <- unlist(groups$keys)                     # order of the per-specification vectors
est_of     <- ds$estimand[match(spec_order, ds$beta_key)]

# party multiplicities per draw: column 0 = identity (original sample), columns 1..B = draws
set.seed(SEED)
draws <- c(list(setNames(rep(1L, length(PARTIES)), PARTIES)),
           map(seq_len(B), function(b) {
             m <- tabulate(sample.int(length(PARTIES), length(PARTIES), replace = TRUE), nbins = length(PARTIES))
             setNames(m, PARTIES)
           }))

# ---- 2. One draw: every specification on one bootstrap world ---------------------------------
# Estimation core. feols for 38,804 specifications x B draws is too slow (about 3 h on 32
# workers), so each design is estimated directly, reproducing fixest 0.14 exactly (asserted on
# draw 0 against every R/42 coefficient and party p-value):
#   - observations with a missing outcome or regressor dropped per outcome; FE groups with a
#     single observation removed iteratively (fixest fixef.rm = "perfect_fit" for OLS);
#   - fixest::demean() of regressors and outcomes (tol 1e-6, as feols), OLS on the demeaned data;
#   - cluster-robust variance with the fixest small-sample correction
#     G/(G-1) * (n-1)/(n-K), K = slopes + sum of the sizes of the FE not nested in the cluster
#     - (number of such FE - 1) (ssc K.fixef = "nonnested", K.exact = FALSE);
#   - p from the t distribution with G-1 degrees of freedom (t.df = "min").
#   Specifications with < 100 complete cases or < 100 obs after singleton removal, or a
#   rank-deficient design (collin.tol 1e-9) are NA ("failed"), as in R/42.
# Y: outcome columns (all sharing X, FE and cluster); j: column of X holding the term of interest
fast_group <- function(Y, X, fe, cl, j) {
  k <- ncol(Y); out <- matrix(NA_real_, k, 2)
  okX <- stats::complete.cases(X)
  samp <- lapply(seq_len(k), function(i) {
    s <- okX & !is.na(Y[, i])
    if (sum(s) < 100) return(NULL)
    repeat {
      drop <- rep(FALSE, length(s))
      for (f in fe) { cnt <- tabulate(f[s], nbins = max(f)); drop <- drop | (s & cnt[f] == 1L) }
      if (!any(drop)) break
      s <- s & !drop
    }
    if (sum(s) < 100) NULL else s
  })
  grp <- integer(k)                                  # outcomes with an identical sample share one demeaning
  for (i in seq_len(k)) if (!is.null(samp[[i]])) {
    hit <- which(vapply(seq_len(i - 1), function(h) grp[h] == h && identical(samp[[h]], samp[[i]]), TRUE))
    grp[i] <- if (length(hit)) hit[1] else i
  }
  maxcl <- max(cl)
  for (g in unique(grp[grp > 0])) {
    ii <- which(grp == g); s <- samp[[g]]; n <- sum(s)
    fl <- lapply(fe, function(f) f[s])
    Z <- fixest::demean(cbind(X[s, , drop = FALSE], Y[s, ii, drop = FALSE]), fl, nthreads = 1, tol = 1e-6, iter = 10000)
    Xd <- Z[, seq_len(ncol(X)), drop = FALSE]
    if (qr(Xd, tol = 1e-9)$rank < ncol(X)) next
    XtXi <- solve(crossprod(Xd))
    clc <- cl[s]; G <- length(unique(clc))
    nested <- vapply(fl, function(f) length(unique(f * (maxcl + 1) + clc)) == length(unique(f)), TRUE)
    sizes  <- vapply(fl, function(f) length(unique(f)), 1L)
    K <- ncol(X) + if (any(!nested)) sum(sizes[!nested]) - (sum(!nested) - 1L) else 0L
    adj <- G / (G - 1) * (n - 1) / (n - K)
    for (q in seq_along(ii)) {
      yd <- Z[, ncol(X) + q]
      b <- XtXi %*% crossprod(Xd, yd); e <- as.vector(yd - Xd %*% b)
      S <- rowsum(Xd * e, clc, reorder = FALSE)
      V <- adj * (XtXi %*% crossprod(S) %*% XtXi)
      out[ii[q], ] <- c(b[j], 2 * stats::pt(-abs(b[j] / sqrt(V[j, j])), G - 1))
    }
  }
  out
}

# mode "null": outcomes y* = y - b_k x (the test); mode "orig": original outcomes (draw-0 check)
run_draw <- function(mult, mode = "null") {
  coef_out <- p_out <- rep(NA_real_, length(spec_order)); pos <- 0L
  cache_key <- ""; d <- NULL
  for (g in seq_len(nrow(groups))) {
    gr <- groups[g, ]; ocs <- gr$outcomes[[1]]; bk <- gr$b[[1]]; k <- length(ocs)
    idx <- pos + seq_len(k); pos <- pos + k
    ck <- paste(gr$data_key, gr$sample_key)
    if (ck != cache_key) {                            # bootstrap rows of this dataset x sample
      d <- dat_list[[as.character(gr$data_key)]]
      d <- switch(gr$sample_key, full = d, post2000 = d[d$year >= 2000, ], post2010 = d[d$year >= 2010, ],
                  europe_only = d[d$countryname %in% EUROPE, ])
      m <- unname(mult[as.character(d$party)])
      d <- d[rep(seq_len(nrow(d)), times = m), ]
      bcl <- paste(d$party, sequence(m), sep = "_")   # each copy of a party = own cluster and own party FE
      d$cl  <- match(bcl, unique(bcl))
      d$cc  <- match(d$countryname, unique(d$countryname))
      d$yy  <- match(d$year, unique(d$year))
      d$cxy <- match(paste(d$countryname, d$year), unique(paste(d$countryname, d$year)))
      cache_key <- ck
    }
    fe <- switch(gr$fe_key, ctry_year = list(d$cc, d$yy), ctry_year_p = list(d$cc, d$yy, d$cl), ctry_x_year = list(d$cxy))
    X <- if (gr$estimand == "pop") cbind(d$populism_overall) else cbind(d$populism_overall, d$IDEO, d$populism_overall * d$IDEO)
    j <- ncol(X); x <- X[, j]
    Y <- vapply(seq_len(k), function(q) if (mode == "null") d[[ocs[q]]] - bk[q] * x else d[[ocs[q]]], numeric(nrow(d)))
    r <- fast_group(Y, X, fe, d$cl, j)
    coef_out[idx] <- r[, 1]; p_out[idx] <- r[, 2]
  }
  list(coef = coef_out, p = p_out)
}
curve_stats <- function(coef, p, est) {
  map_dfr(c("pop", "inter"), function(e) {
    s <- est == e & !is.na(coef); cf <- coef[s]; pp <- p[s]
    tibble(estimand = e, n_specs_ok = sum(s), n_specs_failed = sum(est == e) - sum(s),
           median = median(cf), share_neg = mean(cf < 0), share_sig_neg = mean(cf < 0 & pp < .05),
           share_sig_pos = mean(cf > 0 & pp < .05))
  })
}

# ---- 3. Run: draw 0 (checks) and the B bootstrap draws ---------------------------------------
# Single-threaded BLAS / OpenMP in the main process and in every worker (the system OpenBLAS is
# the pthread build and otherwise spawns up to 64 threads per worker): avoids oversubscription
# and keeps the floating-point summation order, and hence the output bytes, fixed.
Sys.setenv(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1")   # inherited by the workers at launch
RhpcBLASctl::blas_set_num_threads(1); RhpcBLASctl::omp_set_num_threads(1)
plan(multisession, workers = WORKERS)
opts <- furrr_options(seed = TRUE, scheduling = 1,
                      globals = c("draws", "groups", "dat_list", "spec_order", "est_of", "EUROPE", "fast_group", "run_draw",
                                  "curve_stats"),
                      packages = c("fixest", "dplyr", "tibble", "purrr", "stats"))
t_run <- Sys.time()
res <- future_map(seq_along(draws), function(i) {
  RhpcBLASctl::blas_set_num_threads(1); RhpcBLASctl::omp_set_num_threads(1)
  if (i == 1L) {
    o <- run_draw(draws[[1]], "orig"); n0 <- run_draw(draws[[1]], "null")
    list(orig = o, null0 = n0)
  } else {
    r <- run_draw(draws[[i]], "null")
    curve_stats(r$coef, r$p, est_of) |> mutate(draw = i - 1L, .before = 1)
  }
}, .options = opts)
plan(sequential)
runtime_min <- as.numeric(difftime(Sys.time(), t_run, units = "mins"))

# draw-0 checks: R/42 is reproduced exactly; the null outcomes have zero coefficients
chk <- res[[1]]; obs <- ds[match(spec_order, ds$beta_key), ]
dev_coef <- max(abs(chk$orig$coef - obs$coef)); dev_p <- max(abs(chk$orig$p - obs$p))
dev_null <- max(abs(chk$null0$coef))
stopifnot("draw 0 must reproduce every R/42 specification" = !anyNA(chk$orig$coef),
          dev_coef < 1e-8, dev_p < 1e-8, dev_null < 1e-8)

boot <- bind_rows(res[-1]) |> arrange(estimand != "pop", draw)
observed <- curve_stats(obs$coef, obs$p, est_of)
stopifnot(all(observed$median < 0))                   # the tests below are one-sided in the negative direction

# ---- 4. Joint inference table ----------------------------------------------------------------
STATS <- c(median = "Median coefficient", share_neg = "Share negative", share_sig_neg = "Share significantly negative",
           share_sig_pos = "Share significantly positive (supplementary)")
joint <- map_dfr(c("pop", "inter"), function(e) map_dfr(names(STATS), function(s) {
  o <- observed[[s]][observed$estimand == e]; bb <- boot[[s]][boot$estimand == e]
  extreme <- if (s == "median") bb <= o else bb >= o
  tibble(estimand = e, statistic = s, observed = o, boot_mean = mean(bb),
         boot_p2.5 = quantile(bb, .025, names = FALSE), boot_p97.5 = quantile(bb, .975, names = FALSE),
         n_extreme = sum(extreme), p_value = mean(extreme), p_plus1 = (1 + sum(extreme)) / (1 + B),
         B = B, n_specs = sum(est_of == e),
         mean_specs_failed_per_draw = mean(boot$n_specs_failed[boot$estimand == e]))
}))
write_csv_det(joint, file.path(PIPE_OUT, "csv", "56_joint_inference.csv"))
write_csv_det(boot |> select(draw, estimand, n_specs_ok, n_specs_failed, median, share_neg, share_sig_neg, share_sig_pos),
              file.path(PIPE_OUT, "csv", "56_bootstrap_draws.csv"))

# ---- 5. Model-fork decomposition (pop) -------------------------------------------------------
MS_MODELS <- list(sonnet_only = "sonnet", gpt41_only = "gpt41mini", gemini_only = "gemini",
                  sonnet_gpt41 = c("sonnet", "gpt41mini"), sonnet_gemini = c("sonnet", "gemini"),
                  gpt41_gemini = c("gpt41mini", "gemini"), all_three = c("sonnet", "gpt41mini", "gemini"))
MLBL <- c(sonnet = "Claude Sonnet 4.6", gpt41mini = "GPT-4.1-mini", gemini = "Gemini 3.5 Flash")
wq <- function(x, w, q) { o <- order(x); cw <- cumsum(w[o]) / sum(w); x[o][which(cw >= q - 1e-12)[1]] }
# unweighted rows use median() / quantile() exactly as R/42 (csv/42_per_fork.csv); weighted rows use wq()
dsum <- function(x, w = NULL) {
  qf <- if (is.null(w)) function(q) quantile(x$coef, q, names = FALSE) else function(q) wq(x$coef, w, q)
  if (is.null(w)) w <- rep(1, nrow(x))
  tibble(n = nrow(x), median = if (all(w == 1)) median(x$coef) else qf(.5), p25 = qf(.25), p75 = qf(.75),
         share_sig_neg = sum(w * (x$coef < 0 & x$p < .05)) / sum(w), share_pos = sum(w * (x$coef > 0)) / sum(w))
}
pop <- ds |> filter(estimand == "pop") |>
  mutate(k_models = lengths(MS_MODELS[model_set]),
         has_sonnet = map_lgl(MS_MODELS[model_set], ~ "sonnet" %in% .x),
         has_gpt41mini = map_lgl(MS_MODELS[model_set], ~ "gpt41mini" %in% .x),
         has_gemini = map_lgl(MS_MODELS[model_set], ~ "gemini" %in% .x))
stopifnot(!anyNA(pop$k_models))
n_set <- table(pop$model_set)
w_set   <- 1 / as.numeric(n_set[pop$model_set])                     # each model set 1/7
w_model <- (1 / pop$k_models) / as.numeric(n_set[pop$model_set])     # a set of k models counts 1/k
decomp <- bind_rows(
  map_dfr(names(MLBL), function(m) bind_rows(
    dsum(pop[pop[[paste0("has_", m)]], ]) |> mutate(panel = "which_model", group = m, option = "included"),
    dsum(pop[!pop[[paste0("has_", m)]], ]) |> mutate(panel = "which_model", group = m, option = "not_included"))),
  map_dfr(1:3, function(k) dsum(pop[pop$k_models == k, ]) |> mutate(panel = "how_many", group = "k_models", option = as.character(k))),
  map_dfr(names(MS_MODELS), function(s) dsum(pop[pop$model_set == s, ]) |> mutate(panel = "model_set", group = "model_set", option = s)),
  dsum(pop) |> mutate(panel = "overall", group = "weighting", option = "cells"),
  dsum(pop, w_set) |> mutate(panel = "overall", group = "weighting", option = "equal_per_model_set"),
  dsum(pop, w_model) |> mutate(panel = "overall", group = "weighting", option = "per_model_pair_half")) |>
  relocate(panel, group, option)
write_csv_det(decomp, file.path(PIPE_OUT, "csv", "56_model_fork_decomposition.csv"))

# ---- 6. Tables -------------------------------------------------------------------------------
f3 <- function(x) sprintf("$%.3f$", x)
fpv <- function(n_ext, B) if (n_ext == 0) sprintf("$<%.3f$", 1 / B) else sprintf("$%.3f$", n_ext / B)
fval <- function(s, x) if (s == "median") f3(x) else sprintf("%.1f\\%%", 100 * x)
jrows <- unlist(map(c("pop", "inter"), function(e) {
  x <- joint |> filter(estimand == e)
  hdr <- if (e == "pop") sprintf("\\multicolumn{5}{l}{\\textit{Populism slope, %s specifications}} \\\\", comma(x$n_specs[1]))
         else sprintf("\\multicolumn{5}{l}{\\textit{Populism $\\times$ ideology, %s specifications}} \\\\", comma(x$n_specs[1]))
  c(hdr, pmap_chr(x, function(statistic, observed, boot_mean, boot_p2.5, boot_p97.5, n_extreme, ...)
    sprintf("\\quad %s & %s & %s & [%s,\\,%s] & %s \\\\", STATS[[statistic]], fval(statistic, observed),
            fval(statistic, boot_mean), fval(statistic, boot_p2.5), fval(statistic, boot_p97.5), fpv(n_extreme, B))),
    if (e == "pop") "\\midrule")
}))
write_atomic(c("% v2026a; distinct specifications of R/42 (party-clustered SE); generated by R/56_Spec_Curve_Inference.R",
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Joint inference for the specification curve under the null of no effect}",
  "\\label{tab:speccurve:joint}", "\\footnotesize", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%", "\\begin{tabular}{lrrcr}", "\\toprule",
  "Curve statistic & Observed & Mean under $H_0$ & 2.5/97.5\\% under $H_0$ & $p$ \\\\", "\\midrule",
  jrows, "\\bottomrule", "\\end{tabular}", "}",
  "\\begin{minipage}{0.95\\linewidth}\\scriptsize\\vspace{0.5ex}",
  sprintf(paste0("\\textit{Notes:} Joint test of Simonsohn, Simmons and Nelson (2020). For every specification $k$ the outcome is replaced by ",
    "$y^*_k = y - \\hat\\beta_k x_k$, where $x_k$ is the term of interest ($x=\\mathrm{POP}$, or $\\mathrm{POP}\\times\\mathrm{IDEO}$ ",
    "for the interaction), so that the null of a zero coefficient holds exactly in every specification. In each of $B=%d$ draws, parties ",
    "(CMP party codes) are resampled with replacement, the same draw is used for all specifications, and every specification is ",
    "re-estimated on its null outcome; $p<0.05$ uses standard errors clustered by party (each resampled copy a separate cluster). ",
    "$p$ is the share of draws with a median at least as negative, or a share at least as large, as observed; $<%.3f$ means that no draw ",
    "was as extreme. Shares are over the specifications estimable in a draw."), B, 1 / B),
  "\\end{minipage}", "\\end{table}"),
  file.path(PIPE_OUT, "tables", "tab_speccurve_joint_inference.tex"), writer = function(x, p) writeLines(x, p))

SLBL <- c(sonnet_only = "Sonnet only", gpt41_only = "GPT-4.1-mini only", gemini_only = "Gemini only",
          sonnet_gpt41 = "Sonnet + GPT-4.1-mini", sonnet_gemini = "Sonnet + Gemini", gpt41_gemini = "GPT-4.1-mini + Gemini",
          all_three = "All three")
WLBL <- c(cells = "Each specification once (as reported)", equal_per_model_set = "Each model set equally (1/7)",
          per_model_pair_half = "Per model (pair = 1/2, trio = 1/3)")
drow <- function(lbl, x) sprintf("\\quad %s & %s & %s & $[%.3f,\\,%.3f]$ & %.1f & %.1f \\\\", lbl, comma(x$n), f3(x$median),
                                 x$p25, x$p75, 100 * x$share_sig_neg, 100 * x$share_pos)
dd <- decomp
tab_rows <- c("\\multicolumn{6}{l}{\\textit{Which model is in the set}} \\\\",
  unlist(map(names(MLBL), function(m) c(
    drow(paste(MLBL[[m]], "included"), dd |> filter(panel == "which_model", group == m, option == "included")),
    drow(paste(MLBL[[m]], "not included"), dd |> filter(panel == "which_model", group == m, option == "not_included"))))),
  "\\midrule", "\\multicolumn{6}{l}{\\textit{How many models}} \\\\",
  map_chr(1:3, function(k) drow(sprintf("%d model%s", k, if (k > 1) "s" else ""), dd |> filter(panel == "how_many", option == as.character(k)))),
  "\\midrule", "\\multicolumn{6}{l}{\\textit{Model set}} \\\\",
  map_chr(names(SLBL), function(s) drow(SLBL[[s]], dd |> filter(panel == "model_set", option == s))),
  "\\midrule", "\\multicolumn{6}{l}{\\textit{All specifications, weighted}} \\\\",
  map_chr(names(WLBL), function(w) drow(WLBL[[w]], dd |> filter(panel == "overall", option == w))))
write_atomic(c("% v2026a; distinct specifications of R/42, populism slope (party-clustered SE); generated by R/56_Spec_Curve_Inference.R",
  "\\begin{table}[ht]", "\\centering", "\\caption{Decomposition of the model fork: which model and how many models}",
  "\\label{tab:speccurve:models}", "\\footnotesize", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%", "\\begin{tabular}{lrrcrr}", "\\toprule",
  "Group & $n$ & Median $\\beta$ & IQR & \\% sig.\\ neg. & \\% $\\hat\\beta>0$ \\\\", "\\midrule",
  tab_rows, "\\bottomrule", "\\end{tabular}", "}",
  "\\begin{minipage}{0.95\\linewidth}\\scriptsize\\vspace{0.5ex}",
  paste0("\\textit{Notes:} Distinct specifications of the populism slope (Appendix C). Each model enters the model-set fork four times ",
    "(alone, in two pairs and in the trio), and the sets differ in their number of specifications, so the reported distribution weights ",
    "the models implicitly. The last panel reweights it: each of the seven model sets with equal total weight, or each set of $k$ models ",
    "with weight $1/k$ (a pair counts as half a model); within a set all specifications have equal weight. Weighted quantiles are the ",
    "smallest coefficient at which the cumulative weight reaches the quantile."),
  "\\end{minipage}", "\\end{table}"),
  file.path(PIPE_OUT, "tables", "tab_speccurve_model_decomposition.tex"), writer = function(x, p) writeLines(x, p))

# ---- 7. Figure: null distributions with the observed values ------------------------------------
ELBL <- c(pop = "Populism slope", inter = "Populism x ideology")
fig_d <- boot |> select(estimand, median, share_sig_neg) |>
  pivot_longer(-estimand, names_to = "statistic") |>
  mutate(panel = factor(paste0(ELBL[estimand], ": ", if_else(statistic == "median", "median coefficient", "share significantly negative")),
                        levels = paste0(rep(ELBL, each = 2), ": ", c("median coefficient", "share significantly negative"))))
fig_o <- observed |> select(estimand, median, share_sig_neg) |> pivot_longer(-estimand, names_to = "statistic") |>
  left_join(joint |> select(estimand, statistic, n_extreme), by = c("estimand", "statistic")) |>
  mutate(panel = factor(paste0(ELBL[estimand], ": ", if_else(statistic == "median", "median coefficient", "share significantly negative")),
                        levels = levels(fig_d$panel)),
         lab = sprintf("observed %s\np %s", if_else(statistic == "median", sprintf("%.3f", value), sprintf("%.1f%%", 100 * value)),
                       if_else(n_extreme == 0, sprintf("< %.3f", 1 / B), sprintf("= %.3f", n_extreme / B))))
p_null <- ggplot(fig_d, aes(value)) +
  geom_histogram(bins = 40, fill = "grey60", colour = "white", linewidth = 0.2) +
  geom_vline(data = fig_o, aes(xintercept = value), colour = "firebrick", linewidth = 0.6) +
  # label to the right of a line in the left part of the panel (median), to the left otherwise (shares)
  geom_text(data = fig_o, aes(x = value, y = Inf, label = lab, hjust = if_else(statistic == "median", -0.05, 1.05)),
            vjust = 1.3, size = 2.8, colour = "firebrick") +
  facet_wrap(~ panel, scales = "free", ncol = 2) +
  scale_x_continuous(name = "Curve statistic under the null (bootstrap draws) and observed value",
                     expand = expansion(mult = c(0.08, 0.08))) +
  scale_y_continuous(name = "Bootstrap draws") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), strip.text = element_text(hjust = 0))
save_pdf(p_null, file.path(PIPE_OUT, "figures", "spec_curve_null_distribution.pdf"), 7.5, 5.2)

# ---- 8. Log ----------------------------------------------------------------------------------
fmt_tab <- function(x) capture.output(print(as.data.frame(x), digits = 6, row.names = FALSE))
wm <- decomp |> filter(panel == "overall")
log_lines <- c(
  "R/56 spec-curve joint inference (PLLM-0206)",
  sprintf("B = %d draws (+ draw 0 check), seed %d, %d workers, %d parties in the bootstrap universe", B, SEED, WORKERS, length(PARTIES)),
  sprintf("specifications: pop %d, inter %d; estimation groups (designs per draw): %d",
          sum(est_of == "pop"), sum(est_of == "inter"), nrow(groups)),
  sprintf("bootstrap runtime: %.1f min (whole script %.1f min)", runtime_min, as.numeric(difftime(Sys.time(), t_start, units = "mins"))),
  sprintf("draw-0 check: max |coef - R/42| = %.2e, max |p - R/42| = %.2e, max |null coef| = %.2e", dev_coef, dev_p, dev_null),
  "", "Joint inference:", fmt_tab(joint),
  "", "Model-fork decomposition (pop):", fmt_tab(decomp),
  "", sprintf("overall median: cells %.4f | equal per model set %.4f | per model (pair 1/2, trio 1/3) %.4f",
              wm$median[wm$option == "cells"], wm$median[wm$option == "equal_per_model_set"], wm$median[wm$option == "per_model_pair_half"]))
writeLines(log_lines, file.path(PIPE_OUT, "logs", "56_spec_curve_inference.txt"))
cat(log_lines, sep = "\n")
