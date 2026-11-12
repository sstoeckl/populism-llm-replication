# ============================================================
# 1_Aggregation.R
#
# THE aggregation code path of the v2026a chain: per-chunk scores -> per-model manifesto
# scores -> cross-model manifesto scores (+ IDEO). Used by the baseline (R/40; R/41, R/43, R/44,
# R/49, R/50 and R/60 read the baseline file), the robustness variants (R/41), the
# specification curve (R/42), the ideology diagnosis (R/44) and the A.13 analysis in v2026a mode
# (R/51). No chain stage aggregates chunk scores any other way.
#
# Deliberate exceptions outside the chain (named, documented):
#   - R/51 in its default `june` mode and R/50_Independence_3c reproduce analyses of 2026-10-07/08
#     on the June data with the June rule (AGG_RULES$june below, there evaluated from the archived
#     R/20 code); they are API-module provenance, not chain stages.
#   - R/32 finalize no longer writes *_agg files (2026-10-26); the existing *_agg.parquet files
#     are deprecated and read by nothing.
#
# Replaces (Phase 2, 2026-10-24):
#   - website/data/_build_baseline.R model_cwm()       (verified, populism_confidence for all dims)
#   - R/_archive/20_Spec_Curve_Compute.R:24-48          (same rule, + plain mean / median)
#   - the *_agg blocks of R/15b, R/16, R/18, R/19       (verified|mixed & quality_verified, plain
#     mean; empty for the *_overall dims because those never pass the R/12 topic check)
#
# Canonical rule, Phase 0 rule (a') (decided 2026-10-24, option B):
#   within model:  chunks with evidence_status == "verified", a non-missing score and a span
#                  that passes the quote check (VALIDATION_RULES$quote: verbatim in the source
#                  chunk and not copied from the prompt) enter; confidence-weighted mean
#                  with the dimension's OWN confidence block:
#                    pop_* and populism_overall          -> populism_confidence
#                    pop_ideology_*                      -> pop_ideology_confidence
#                    lib_* and liberalism_overall        -> liberalism_confidence
#                  weight = pmax(confidence, 0.1) on the 0-100 scale (as before; no confidence
#                  is missing or zero in v2026a, so the floor never binds).
#   between models: plain mean over the models with a non-missing manifesto score (gap legs
#                  and dimensions without evidence are NA and drop out).
#   IDEO = (R - L) / (R + L) on the cross-model means of the right and left ideology subscores
#          (rev3, decided by Sebastian 2026-10-27, PLLM-0132 / PLLM-0048; NA if R + L = 0).
#   IDEO_c = (R - L) / (R + L + C), the construction until rev2 (centrist score in the
#          denominator), kept as a documented alternative (baseline column pop_ideology_direction_c).
#
# Named rules (within statistic + span validation):
#   AGG_RULES$canonical = cwm with own-block weights, validation "quote"   (the baseline)
#   AGG_RULES$june      = cwm_pop (populism confidence for every dim), validation "none"
#                         (the June 2026 baseline; spec-curve fork and old-vs-new comparison)
#
# IDEO selection forks (chain-review #5): IDEO is NA if R or L is missing, and R and L can rest on
# different model subsets. combine_models() therefore also returns (same logic as before rev3, on
# the parts of the new formula, R and L):
#   IDEO_zero : missing R/L set to 0 where POP is non-missing (NA only if R+L = 0)
#   IDEO_ge2  : IDEO only where at least 2 models contribute to each of R and L
#   IDEO_all3 : IDEO only where all 3 models contribute to each of R and L
# IDEO_c (alternative construction) is NA if any of R, L, C is missing, as before rev3.
#
# Parse-lost chunk-dims (status NA without scoring_error) carry no score and are excluded like
# no_evidence (none remain in v2026a rev 2). Dims decoded by the lenient parser of R/15b
# (raw column parse_repaired) can be excluded with exclude_repaired = TRUE (spec-curve fork).
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(arrow); library(stringr)
})

AGG_DIMS <- c("pop_anti_elitism", "pop_people_centrism", "pop_manichean", "populism_overall",
              "pop_ideology_overall", "pop_ideology_left", "pop_ideology_right", "pop_ideology_centrist",
              "lib_political", "lib_social", "lib_economic", "lib_financial_market", "liberalism_overall")
AGG_MODELS <- c("sonnet", "gpt41mini", "gemini")
# file stem of each model in data/CMP/v2026a/
AGG_FILE_STEM <- c(sonnet = "claude", gpt41mini = "gpt41mini", gemini = "gemini")

AGG_RULES <- list(canonical = list(validation = "quote", stat = "cwm"),
                  june      = list(validation = "none",  stat = "cwm_pop"))

dim_conf_block <- function(dim) case_when(
  str_starts(dim, "pop_ideology") ~ "pop_ideology_confidence",
  str_starts(dim, "pop_") | dim == "populism_overall" ~ "populism_confidence",
  TRUE ~ "liberalism_confidence")

# ---- 1. Chunk x dimension long table ----------------------------------------
#
# raw    : one model's raw chunk file (data/CMP/v2026a/raw_<m>.parquet layout)
# checks : that model's span checks (R/34 output: doc_id, dim_name, quote_ok, topic_ok)
# Returns one row per (chunk, dim) with score, status, both weights and span flags.
chunk_dim_long <- function(raw, checks, model) {
  stopifnot("duplicate chunk ids in a raw file" = !anyDuplicated(raw$doc_id))
  rep_dims <- if ("parse_repaired" %in% names(raw)) strsplit(coalesce(raw$parse_repaired, ""), ",", fixed = TRUE) else
    rep(list(character()), nrow(raw))
  long <- map_dfr(AGG_DIMS, function(d) {
    tibble(doc_id = raw$doc_id, doc_id_root = raw$doc_id_root, chunk_id = raw$chunk_id,
           dim = d,
           score  = as.numeric(raw[[paste0(d, "_score")]]),
           status = raw[[paste0(d, "_status")]],
           w_block = pmax(raw[[dim_conf_block(d)]], 0.1),
           w_pop   = pmax(raw$populism_confidence, 0.1),
           repaired = vapply(rep_dims, function(v) d %in% v, logical(1)))
  })
  # a scored chunk-dim must have both weights (pmax(NA, 0.1) is NA, which would poison sums)
  stopifnot("missing confidence on a scored chunk-dim" =
              !any(!is.na(long$score) & (is.na(long$w_block) | is.na(long$w_pop))))
  ck <- checks |> select(doc_id, dim = dim_name, quote_ok, topic_ok)
  stopifnot(!anyDuplicated(ck[c("doc_id", "dim")]))
  long |>
    left_join(ck, by = c("doc_id", "dim")) |>
    mutate(model = model, has_span = !is.na(quote_ok),
           quote_ok = quote_ok %in% TRUE, topic_ok = topic_ok %in% TRUE) |>
    arrange(model, doc_id_root, chunk_id, dim)
}

# ---- 2. Within-model aggregation ---------------------------------------------
#
# Span-validation levels (per chunk x dim):
#   "none"        no span filter (the June 2026 baseline)
#   "quote"       span verbatim in its source chunk and not copied from the prompt
#   "quote_topic" "quote" and on topic (fixed keyword regex, LLM judge backstop; R/34)
# A validation RULE assigns a level to each dimension group:
DIM_GROUPS <- list(
  core     = c("pop_anti_elitism", "pop_people_centrism", "pop_manichean",
               "lib_political", "lib_social", "lib_economic", "lib_financial_market"),
  ideo_lrc = c("pop_ideology_left", "pop_ideology_right", "pop_ideology_centrist"),
  overall  = c("populism_overall", "liberalism_overall", "pop_ideology_overall"))
VALIDATION_RULES <- list(
  # quote = the baseline (rule (a'), decided 2026-10-24); r12, full and none are spec-curve forks.
  # Phase 0 rule (a) read literally: R/12 quality_verified as stored for the core and L/R/C
  # dims (for the core dims the stored topic regex matches everything, so R/12 = quote check),
  # extended quote + topic check for the *_overall dims
  r12      = c(core = "quote", ideo_lrc = "quote_topic", overall = "quote_topic"),
  quote    = c(core = "quote", ideo_lrc = "quote", overall = "quote"),
  full     = c(core = "quote_topic", ideo_lrc = "quote_topic", overall = "quote_topic"),
  none     = c(core = "none", ideo_lrc = "none", overall = "none"))

validation_levels <- function(rule) {
  lv <- VALIDATION_RULES[[rule]]
  if (is.null(lv)) stop("unknown validation rule: ", rule)
  unlist(map(names(DIM_GROUPS), ~ setNames(rep(lv[[.x]], length(DIM_GROUPS[[.x]])), DIM_GROUPS[[.x]])))
}

# statuses: evidence_status values that enter (paper: verified only)
# Returns per (model, doc_id, dim): cwm (block weights), cwm_pop (populism_confidence for
# every dim, the June rule), mean, median, n_chunks.
aggregate_within <- function(cl, validation = "quote", statuses = "verified", exclude_repaired = FALSE) {
  lv <- validation_levels(validation)[cl$dim]
  keep <- cl$status %in% statuses & !is.na(cl$score) &
    (lv == "none" | (lv == "quote" & cl$quote_ok) | (lv == "quote_topic" & cl$quote_ok & cl$topic_ok))
  if (exclude_repaired) keep <- keep & !cl$repaired
  cl[keep, ] |>
    group_by(model, doc_id = doc_id_root, dim) |>
    summarise(cwm     = sum(score * w_block) / sum(w_block),
              cwm_pop = sum(score * w_pop) / sum(w_pop),
              mean    = mean(score),
              median  = median(score),
              n_chunks = n(), .groups = "drop") |>
    arrange(model, doc_id, dim)
}

# ---- 3. Between-model aggregation --------------------------------------------
#
# Leave-worst-out drops the model farthest from the mean of all models. With exactly two
# models both deviations tie by construction, so the rule is undefined and returns the plain
# mean; a tie between two of three models also returns the mean. This fixes the tie bug of the
# old R/20:109-112, where which.max() silently dropped the first model and returned the second
# model's score.
between_combine <- function(x, method = c("mean", "median", "leave_worst_out")) {
  method <- match.arg(method)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  if (length(x) == 1L) return(x)
  switch(method,
         mean   = mean(x),
         median = median(x),
         leave_worst_out = if (length(x) == 2L) mean(x) else {
           dev <- abs(x - mean(x))
           if (sum(dev == max(dev)) > 1L) mean(x) else mean(x[-which.max(dev)])
         })
}

# IDEO (rev3): (R - L) / (R + L)
ideo_direction <- function(R, L) {
  out <- (R - L) / (R + L)
  out[!is.finite(out)] <- NA_real_
  out
}
# IDEO_c (until rev2): (R - L) / (R + L + C)
ideo_direction_c <- function(R, L, C) {
  den <- R + L + C
  out <- (R - L) / den
  out[!is.finite(out)] <- NA_real_
  out
}

# within_long: output of aggregate_within(); stat: which within-model statistic
# Returns one row per manifesto: the 13 dims + IDEO + n_models_<dim>.
combine_models <- function(within_long, models = AGG_MODELS, stat = "cwm",
                           between = "mean", dims = AGG_DIMS) {
  x <- within_long |> filter(model %in% models, dim %in% dims) |>
    select(doc_id, dim, model, value = all_of(stat)) |>
    filter(is.finite(value)) |>
    arrange(doc_id, dim, model)
  agg <- if (between == "mean") {
    x |> group_by(doc_id, dim) |> summarise(score = mean(value), k = n(), .groups = "drop")
  } else {
    x |> group_by(doc_id, dim) |>
      summarise(score = between_combine(value, between), k = n(), .groups = "drop")
  }
  wide <- agg |> select(-k) |> pivot_wider(names_from = dim, values_from = score)
  for (d in setdiff(dims, names(wide))) wide[[d]] <- NA_real_
  wide <- wide |> select(doc_id, all_of(dims))
  k <- agg |> mutate(dim = paste0("n_models_", dim)) |> select(doc_id, dim, k) |>
    pivot_wider(names_from = dim, values_from = k, values_fill = 0L)
  for (d in setdiff(paste0("n_models_", dims), names(k))) k[[d]] <- 0L
  out <- wide |> left_join(k, by = "doc_id") |>
    mutate(across(starts_with("n_models_"), ~ coalesce(.x, 0L)))
  if (all(c("pop_ideology_right", "pop_ideology_left", "pop_ideology_centrist") %in% dims)) {
    z <- function(x) coalesce(x, 0)
    k_min <- pmin(out$n_models_pop_ideology_right, out$n_models_pop_ideology_left)
    pop_ok <- if ("populism_overall" %in% dims) !is.na(out$populism_overall) else rep(FALSE, nrow(out))
    out <- out |> mutate(
      IDEO      = ideo_direction(pop_ideology_right, pop_ideology_left),
      IDEO_c    = ideo_direction_c(pop_ideology_right, pop_ideology_left, pop_ideology_centrist),
      IDEO_zero = ifelse(pop_ok, ideo_direction(z(pop_ideology_right), z(pop_ideology_left)), NA_real_),
      IDEO_ge2  = ifelse(k_min >= 2, IDEO, NA_real_),
      IDEO_all3 = ifelse(k_min >= 3, IDEO, NA_real_))
  }
  out |> arrange(doc_id)
}

# ---- 4. Loading the canonical inputs -----------------------------------------
#
# Builds the chunk-dim table for all models from data/CMP/v2026a. Span checks come from
# R/34 (span_checks_<m>.parquet). Data forks:
#   gemini_norm = "vertex" (canonical) | "june": for the 131 normalisation-only docs use the
#                 June AI-Studio Gemini rows (archive/gemini_june_superseded.parquet) instead
#                 of the Vertex re-score;
#   gap_legs    = "na" (canonical) | "june_fallback": fill a documented gap leg
#                 (coverage.parquet, covered == FALSE) with archived June rows where the June
#                 leg is complete. Only the three Gemini gaps qualify (all three are
#                 normalisation-only docs with complete June legs); the GPT gap 113441_202004
#                 had a failed chunk in June too and stays NA.
load_chunk_dims <- function(dir = "data/CMP/v2026a", checks_dir = dir,
                            gemini_norm = c("vertex", "june"), gap_legs = c("na", "june_fallback")) {
  gemini_norm <- match.arg(gemini_norm); gap_legs <- match.arg(gap_legs)
  cov <- read_parquet(file.path(dir, "coverage.parquet"))
  map_dfr(AGG_MODELS, function(m) {
    stem <- AGG_FILE_STEM[[m]]
    if (!exists("v26_file")) stop("source R/0_Config.R before R/1_Aggregation.R (v26_file selects the current revision)")
    raw <- read_parquet(v26_file("raw", stem))
    ck  <- read_parquet(file.path(checks_dir, paste0("span_checks_", stem, ".parquet")))
    if (m == "gemini" && (gemini_norm == "june" || gap_legs == "june_fallback")) {
      alt    <- read_parquet(file.path(dir, "archive", "gemini_june_superseded.parquet"))
      ck_alt <- read_parquet(file.path(checks_dir, "span_checks_gemini_june_superseded.parquet"))
      gaps <- cov |> filter(model == stem, !covered) |> pull(doc_id)
      swap <- if (gemini_norm == "june") unique(alt$doc_id_root) else intersect(gaps, alt$doc_id_root)
      raw <- bind_rows(raw |> filter(!doc_id_root %in% swap),
                       alt |> filter(doc_id_root %in% swap))
      ck  <- bind_rows(ck |> filter(!doc_id_root %in% swap),
                       ck_alt |> filter(doc_id_root %in% swap))
    }
    chunk_dim_long(raw, ck, m)
  })
}

# ---- 5. Diagnostics ------------------------------------------------------------

# Status distribution per model x dim, with parse losses made explicit
chunk_status_summary <- function(cl) {
  cl |> mutate(status = coalesce(status, "parse_lost")) |>
    count(model, dim, status) |>
    pivot_wider(names_from = status, values_from = n, values_fill = 0L) |>
    arrange(model, dim)
}
