# ============================================================
# 51_A13_Reproducibility.R   (BRANCH: main)
#
# RUN-TO-RUN DETERMINISM OF THE PRODUCTION (v4 safe, cross-LLM) PIPELINE
# + full-corpus cross-family agreement. Replaces R/6_Verify_Reproducibility.R
# (gpt-4o-mini, old zero-coded schema, 5 single-chunk manifestos) as the
# source of paper Appendix A.13.
#
# NO REIMPLEMENTATION: prompts, schemas, API wrappers, chunking, span
# validation and both aggregation rules are taken verbatim from the
# production scripts. Scripts that only define functions are sourced;
# the full-corpus drivers (R/15b, R/16, R/18) and R/19 / R/20 execute
# top-level I/O when sourced, so their top-level ASSIGNMENTS are parsed
# and evaluated selectively (load_production_defs()) -- same code, no
# side effects.
#
#   R/1_Functions.R, R/3c_Populism_v4_safe.R, R/12_Validate_Span_Quality.R  sourced
#   R/15b  anthropic_v4_chat, score_manifesto_v4_claude, ...   (Sonnet 4.6)
#   R/16   score_manifesto_v4_robust, ...                      (gpt-4.1-mini)
#   R/18   gemini_v4_chat, score_manifesto_v4_gemini, ...      (gemini-flash-latest)
#   R/19   qv / aggregate_dim / agg  -> "validated" aggregation
#          (quality_verified spans, verified|mixed, plain mean; = *_agg.parquet)
#   R/20   load_model_long()         -> "baseline" aggregation
#          (status=="verified", cwm w = pmax(populism_confidence, 0.1);
#           = paper headline / manifesto_scores.parquet)
#
# DESIGN
#   - 20 manifestos, stratified by language (10 languages x 2), era
#     (pre-2000 / 2000-09 / 2010+, 2010+ oversampled) and populism tercile
#     (stored cross-model baseline); 6 multi-chunk documents (>18K tokens,
#     2-3 chunks) so chunking + within-manifesto aggregation are exercised.
#   - 3 models x 2 fresh runs (run 2 starts only after run 1 is complete).
#   - Each fresh run is also compared with the stored production chunks
#     (data/CMP/poplib_v4_*_raw.parquet).
#   - Every API response is intercepted (trace on httr::content) to record
#     the RESOLVED model version and token usage -> exact cost accounting.
#   - Fresh spans are validated with the production validator + judge,
#     with the judge cache cleared per (model, run) (= independent session).
#
# STAGES (Rscript R/51_A13_Reproducibility.R <stage>)
#   sample    draw the sample + cost estimate (no API calls)
#   score     sample + scoring (cost-gated, checkpointed, resumable)
#   analyze   validation + aggregation + metrics + tables (needs score)
#   all       (default) score + analyze
#
# OUTPUTS  output/reproducibility_v4/
#   tables/*.tex     LaTeX tables in the style of A.13 (booktabs)
#   csv/*.csv        all metrics, sample, per-call log, model versions
#   checkpoints/     per-job RDS (contain manifesto text: LOCAL ONLY, © WZB)
#   logs/            session info
#
# COST CAP: USD 20 total (estimated before run 1, re-checked on actual
#   spend before run 2). Concurrency: 6 workers across 3 providers.
#
# AGGREGATION. In `june` mode the two production rules of June 2026 are evaluated from the archived
# code (R/_archive/20 "baseline", R/19 "validated") on purpose, to reproduce the 2026-10-07 A.13
# run exactly. In `v2026a` mode (the chain stage) all aggregation goes through R/1_Aggregation.R.
#
# MODES (env A13_BASE)
#   june    (default) reproduces the 2026-10-07 run: stored production = the June 2026
#           raw files, June aggregation rules, outputs in output/reproducibility_v4/
#   v2026a  Phase-2 chain stage (run_all.R): `analyze` only, NO API CALLS. Re-uses the
#           archived fresh runs and their R/12 span validation (checkpoints in
#           output/reproducibility_v4/checkpoints/; stops if they are missing), compares
#           them with the v2026a stored chunks (data/CMP/v2026a), aggregates with
#           R/1_Aggregation.R and writes to PIPE_OUT/validation/a13/.
#           "baseline_cwm" = own-block confidence-weighted mean with the quote check
#           (verbatim, not from the prompt) on all dims; "validated_mean" = the June rule
#           (no span check, populism confidence for all dims). The rule-(a) topic check on
#           the *_overall dims is not applied to the fresh runs because their *_overall
#           quotes were never judged, and judging them would need API calls.
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(lubridate)
  library(jsonlite); library(httr); library(furrr)
})
source("R/1_Functions.R")
source("R/3c_Populism_v4_safe.R")
source("R/12_Validate_Span_Quality.R")

STAGE <- commandArgs(trailingOnly = TRUE)[1] %||% "all"
stopifnot(STAGE %in% c("sample", "score", "analyze", "all"))
message("=== R/25 reproducibility v4 | stage: ", STAGE, " | ", Sys.time(), " ===")

# ============================================================
# Configuration
# ============================================================

PROJECT_DIR   <- normalizePath(".")
OUT_DIR       <- "output/reproducibility_v4/"
CKPT_DIR      <- paste0(OUT_DIR, "checkpoints/")
TAB_DIR       <- paste0(OUT_DIR, "tables/")
CSV_DIR       <- paste0(OUT_DIR, "csv/")
LOG_DIR       <- paste0(OUT_DIR, "logs/")
walk(c(OUT_DIR, CKPT_DIR, TAB_DIR, CSV_DIR, LOG_DIR), ensure_dir)

A13_BASE <- Sys.getenv("A13_BASE", "june")
stopifnot(A13_BASE %in% c("june", "v2026a"))
if (A13_BASE == "v2026a") {
  stopifnot("A13_BASE=v2026a runs the no-API analyze stage only" = STAGE == "analyze")
  source("R/0_Config.R")
  source("R/1_Aggregation.R")
  OUT_DIR <- paste0(file.path(PIPE_OUT, "validation", "a13"), "/")
  TAB_DIR <- paste0(OUT_DIR, "tables/"); CSV_DIR <- paste0(OUT_DIR, "csv/"); LOG_DIR <- paste0(OUT_DIR, "logs/")
  walk(c(OUT_DIR, TAB_DIR, CSV_DIR, LOG_DIR), ensure_dir)
  # chain stage: every CSV deterministic (10 significant digits; numeric certification of R/71)
  write_csv <- function(x, file, ...) write_csv_det(x, file)
}
# PLLM-0240 (2026-11-11): no project version codes in the referee-facing notes
PROD_LABEL <- if (A13_BASE == "v2026a") "the archived production scores" else
  "the stored production scores (June 2026)"

SAMPLE_SEED   <- 20261007L
N_PER_LANG    <- 2L
N_MULTICHUNK  <- 6L
N_RUNS        <- 2L
N_WORKERS     <- 6L      # ~2 concurrent requests per provider (keys are shared)
COST_CAP_USD  <- 20

MODELS <- tribble(
  ~model,      ~label,           ~script,                                          ~score_fun,                   ~requested_id,
  "sonnet",    "Sonnet 4.6",     "R/15b_Claude_Full_Corpus.R",                     "score_manifesto_v4_claude",  "claude-sonnet-4-6",
  "gpt41mini", "GPT-4.1-mini",   "R/16_Score_Full_Corpus_gpt41mini.R",             "score_manifesto_v4_robust",  "gpt-4.1-mini-2025-04-14",
  "gemini",    "Gemini Flash",   "R/18_Score_Full_Corpus_Gemini_flash_latest.R",   "score_manifesto_v4_gemini",  "gemini-flash-latest"
)
STORED_RAW <- c(sonnet    = "data/CMP/poplib_v4_claude_full_raw.parquet",
                gpt41mini = "data/CMP/poplib_v4_gpt41mini_raw.parquet",
                gemini    = "data/CMP/poplib_v4_gemini_flash_latest_raw.parquet")
STORED_VAL <- c(sonnet    = "data/CMP/poplib_v4_claude_full_validated_spans.parquet",
                gpt41mini = "data/CMP/poplib_v4_gpt41mini_validated_spans.parquet",
                gemini    = "data/CMP/poplib_v4_gemini_flash_latest_validated_spans.parquet")
if (A13_BASE == "v2026a") {
  STORED_RAW <- setNames(map_chr(AGG_FILE_STEM, \(s) v26_file("raw", s)), names(AGG_FILE_STEM))
  STORED_VAL <- setNames(map_chr(AGG_FILE_STEM, \(s) v26_file("validated_spans", s)), names(AGG_FILE_STEM))
}

# Top-level objects to lift from each production script (everything the
# per-manifesto scorer needs; NOT the data loading / batch loop / write-out).
PROD_KEEP <- list(
  sonnet    = c("MAX_RETRIES_PER_CHUNK", "RETRY_BASE_SLEEP_SEC", "FULL_TEXT_TOKEN_CAP",
                "CHUNK_TOKENS", "OVERLAP_TOKENS", "MODEL_CLAUDE", "V4_DIMS",
                "anthropic_v4_schema", "anthropic_v4_chat", "score_chunk_v4_claude",
                "decode_dim_string", "flatten_parsed_v4_claude", "score_manifesto_v4_claude"),
  gpt41mini = c("MAX_RETRIES_PER_CHUNK", "RETRY_BASE_SLEEP_SEC", "FULL_TEXT_TOKEN_CAP",
                "CHUNK_TOKENS", "OVERLAP_TOKENS", "MODEL", "V4_DIMS",
                "score_chunk_v4_robust", "flatten_parsed_v4", "score_manifesto_v4_robust"),
  gemini    = c("MAX_RETRIES_PER_CHUNK", "RETRY_BASE_SLEEP_SEC", "FULL_TEXT_TOKEN_CAP",
                "CHUNK_TOKENS", "OVERLAP_TOKENS", "MODEL_GEMINI", "V4_DIMS",
                "to_gemini_schema", "gemini_v4_schema", "gemini_v4_chat",
                "score_chunk_v4_gemini", "flatten_parsed_v4_gemini", "score_manifesto_v4_gemini")
)

# USD per 1M tokens (list prices; Gemini priced conservatively at the
# Gemini-3-Flash rate -- 2.5 Flash is 0.30/2.50). Thinking tokens billed as output.
PRICES <- tribble(
  ~pattern,          ~usd_in, ~usd_out,
  "^claude-sonnet",  3.00,    15.00,
  "^gpt-4\\.1-mini", 0.40,     1.60,
  "^gpt-4o-mini",    0.15,     0.60,
  "^gemini",         0.50,     3.00
)

V4_DIMS <- c("pop_anti_elitism","pop_people_centrism","pop_manichean","populism_overall",
             "pop_ideology_left","pop_ideology_right","pop_ideology_centrist","pop_ideology_overall",
             "lib_political","lib_social","lib_economic","lib_financial_market","liberalism_overall")
CONF_VARS <- c("populism_confidence", "pop_ideology_confidence", "liberalism_confidence")
# Dimensions carried by the production baseline aggregation (R/20)
BASE_DIMS <- c("populism_overall","pop_anti_elitism","pop_people_centrism","pop_manichean",
               "pop_ideology_overall","lib_political","lib_social","lib_economic",
               "lib_financial_market","liberalism_overall")

# ============================================================
# Production-code loaders (exact code, no side effects)
# ============================================================

# Top-level `name <- value` expressions of a script, keyed by name.
production_exprs <- function(path) {
  ex <- as.list(parse(path, keep.source = FALSE))
  is_assign <- map_lgl(ex, \(e) is.call(e) && as.character(e[[1]]) %in% c("<-", "=") &&
                                is.symbol(e[[2]]))
  ex <- ex[is_assign]
  set_names(ex, map_chr(ex, \(e) as.character(e[[2]])))
}

# Evaluate the selected top-level assignments (in file order) in a fresh
# environment whose parent is globalenv() (where R/1, R/3c live).
load_production_defs <- function(path, keep) {
  env <- new.env(parent = globalenv())
  ex  <- production_exprs(path)
  for (nm in names(ex)) if (nm %in% keep) eval(ex[[nm]], envir = env)
  miss <- setdiff(keep, ls(env, all.names = TRUE))
  if (length(miss)) stop(path, ": production objects not found: ", paste(miss, collapse = ", "))
  env
}

# R/20: baseline aggregation (cwm over verified chunks). Takes a parquet path.
R20_ENV <- load_production_defs("R/_archive/20_Spec_Curve_Compute.R", "load_model_long")
agg_baseline <- function(raw, model_id) {
  tmp <- tempfile(fileext = ".parquet"); on.exit(unlink(tmp))
  write_parquet(raw, tmp)
  R20_ENV$load_model_long(model_id, tmp) |>
    select(doc_id, ends_with("_cwm")) |>
    rename_with(\(x) sub("_cwm$", "", x)) |>
    mutate(across(-doc_id, \(x) ifelse(is.nan(x), NA_real_, x)))
}

# R/19: validated aggregation (quality_verified spans, verified|mixed, mean).
R19_EXPR <- production_exprs("R/19_Materialize_Gemini_Partial.R")
agg_validated <- function(raw, validated) {
  env <- new.env(parent = globalenv())
  env$raw_ok <- raw; env$validated <- validated
  for (nm in c("V4_DIMS", "qv", "aggregate_dim", "agg")) eval(R19_EXPR[[nm]], envir = env)
  env$agg |> rename_with(\(x) sub("_score$", "", x))
}

# ============================================================
# API probe: record resolved model version + usage of EVERY response
# (trace on httr::content; production wrappers drop these fields)
# ============================================================

api_probe_record <- function(rv) {
  if (!is.list(rv)) return(invisible())
  u <- rv$usage %||% rv$usageMetadata %||% list()
  if (!length(u) && is.null(rv$model) && is.null(rv$modelVersion)) return(invisible())
  rec <- tibble(
    time          = Sys.time(),
    resolved      = as.character(rv$model %||% rv$modelVersion %||% NA),
    response_id   = as.character(rv$id %||% rv$responseId %||% NA),
    input_tokens  = as.numeric(u$input_tokens %||% u$promptTokenCount %||% NA),
    cached_tokens = as.numeric(u$input_tokens_details$cached_tokens %||%
                               u$cache_read_input_tokens %||% u$cachedContentTokenCount %||% 0),
    output_tokens = as.numeric(u$output_tokens %||% u$candidatesTokenCount %||% NA),
    thought_tokens= as.numeric(u$thoughtsTokenCount %||%
                               u$output_tokens_details$reasoning_tokens %||% 0),
    finish        = as.character(rv$stop_reason %||% rv$status %||%
                                 rv$candidates[[1]]$finishReason %||% NA),
    fingerprint   = as.character(rv$system_fingerprint %||% NA)
  )
  pe <- get(".api_probe", envir = globalenv())
  pe$log[[length(pe$log) + 1L]] <- rec
  invisible()
}

install_api_probe <- function() {
  if (!exists(".api_probe", envir = globalenv()))
    assign(".api_probe", new.env(), envir = globalenv())
  pe <- get(".api_probe", envir = globalenv()); pe$log <- list()
  if (!inherits(get("content", envir = asNamespace("httr")), "functionWithTrace")) {
    suppressMessages(trace("content", where = asNamespace("httr"), print = FALSE,
      exit = quote(get("api_probe_record", envir = globalenv())(returnValue(NULL)))))
  }
  invisible()
}

collect_api_probe <- function() {
  pe <- get(".api_probe", envir = globalenv()); out <- bind_rows(pe$log)
  pe$log <- list()
  out
}

price_calls <- function(calls) {
  if (!nrow(calls)) return(mutate(calls, cost_usd = numeric()))
  pr <- map_int(calls$resolved, \(m) {
    i <- which(map_lgl(PRICES$pattern, \(p) grepl(p, m %||% "")))
    if (length(i)) i[1] else NA_integer_
  })
  calls |>
    mutate(usd_in = PRICES$usd_in[pr], usd_out = PRICES$usd_out[pr],
           # Gemini reports thoughts separately from candidates; OpenAI's
           # reasoning tokens are already inside output_tokens.
           billable_out = output_tokens + if_else(grepl("^gemini", resolved), thought_tokens, 0),
           cost_usd = (input_tokens * usd_in + billable_out * usd_out) / 1e6) |>
    select(-usd_in, -usd_out, -billable_out)
}

# ============================================================
# Sample (deterministic, stratified)
# ============================================================

# Country -> language proxy. Only countries with one dominant manifesto
# language; multilingual systems (BE, CH, CA, LU, BA, MK, ME, UA, MD, MT,
# FI, ES regional parties) are excluded, as are ethnic/regional parties
# (parfam 90), so the label is reliable.
LANG_MAP <- c(
  "United Kingdom" = "English", "Ireland" = "English", "Australia" = "English",
  "New Zealand" = "English", "United States" = "English",
  "Germany" = "German", "Austria" = "German", "France" = "French",
  "Spain" = "Spanish", "Mexico" = "Spanish", "Argentina" = "Spanish", "Chile" = "Spanish",
  "Colombia" = "Spanish", "Peru" = "Spanish", "Costa Rica" = "Spanish",
  "Italy" = "Italian", "Netherlands" = "Dutch", "Sweden" = "Swedish",
  "Poland" = "Polish", "Hungary" = "Hungarian", "Greece" = "Greek", "Cyprus" = "Greek",
  "Denmark" = "Danish", "Portugal" = "Portuguese", "Brazil" = "Portuguese",
  "Czech Republic" = "Czech", "Norway" = "Norwegian"
)
SAMPLE_LANGS <- c("English", "German", "French", "Spanish", "Italian",
                  "Dutch", "Swedish", "Polish", "Hungarian", "Greek")

era_of <- function(y) case_when(y < 2000 ~ "pre-2000", y < 2010 ~ "2000-09", TRUE ~ "2010+")

build_sample <- function() {
  # Exactly the production corpus filter (R/15b / R/16 / R/18)
  dat <- read_parquet("data/CMP/full_data.parquet") |>
    filter(!is.na(manifesto_id), !is.na(text)) |>
    mutate(date = as.Date(date)) |>
    distinct(doc_id, .keep_all = TRUE)

  stored <- imap(STORED_RAW, \(p, m) read_parquet(p) |>
                   filter(is.na(scoring_error), !is.na(schema_version)))
  n_chunks <- stored$gpt41mini |> count(doc_id_root, name = "n_chunks_prod")
  ok_all   <- reduce(map(stored, \(x) unique(x$doc_id_root)), intersect)
  base_pop <- imap(stored, \(x, m) agg_baseline(x, m) |> select(doc_id, populism_overall)) |>
    bind_rows() |> group_by(doc_id) |>
    summarise(pop_base = mean(populism_overall, na.rm = TRUE), .groups = "drop") |>
    filter(is.finite(pop_base))

  univ <- dat |>
    filter(doc_id %in% ok_all) |>
    mutate(year = year(date), era = era_of(year),
           language = unname(LANG_MAP[countryname])) |>
    filter(!is.na(language), parfam != 90 | is.na(parfam)) |>
    inner_join(n_chunks, by = c("doc_id" = "doc_id_root")) |>
    inner_join(base_pop, by = "doc_id") |>
    mutate(pop_tercile = ntile(pop_base, 3L))

  # Slot design: 10 languages x 2; slot 1 of every language is 2010+ (the
  # analysis era, oversampled), slot 2 is 2000-09 or pre-2000 (5 each);
  # terciles 7/7/6; 6 multi-chunk slots (2-3 production chunks) spread over
  # distinct languages.
  set.seed(SAMPLE_SEED)
  slots <- tibble(language = rep(SAMPLE_LANGS, each = N_PER_LANG),
                  slot = rep(seq_len(N_PER_LANG), length(SAMPLE_LANGS)))
  slots$era <- "2010+"
  slots$era[slots$slot == 2] <- sample(rep(c("2000-09", "pre-2000"), length.out = sum(slots$slot == 2)))
  slots$pop_tercile <- sample(rep(1:3, length.out = nrow(slots)))
  slots$multi <- FALSE
  slots$multi[sample(which(slots$slot == 1), N_MULTICHUNK)] <- TRUE

  picked <- character()
  rows <- vector("list", nrow(slots))
  for (i in seq_len(nrow(slots))) {
    s <- slots[i, ]
    pool <- univ |>
      filter(language == s$language, !doc_id %in% picked,
             if (s$multi) n_chunks_prod %in% 2:3 else n_chunks_prod == 1L)
    # Relax tercile, then era, if the cell is empty
    cand <- filter(pool, era == s$era, pop_tercile == s$pop_tercile)
    if (!nrow(cand)) cand <- filter(pool, era == s$era)
    if (!nrow(cand)) cand <- pool
    if (!nrow(cand)) stop("No candidate for slot ", i)
    pick <- cand[sample.int(nrow(cand), 1L), ]
    picked <- c(picked, pick$doc_id)
    rows[[i]] <- pick |> mutate(design_era = s$era, design_tercile = s$pop_tercile,
                                design_multi = s$multi)
  }
  smp <- bind_rows(rows) |>
    mutate(tokens_tiktoken = map_dbl(text, estimate_tokens))
  smp
}

# Pre-run cost estimate from actual chunk token counts (o200k tokenizer;
# Claude tokenizer inflated by 1.35, Gemini by 1.10; output tokens assumed).
estimate_cost <- function(smp) {
  sys_tok <- estimate_tokens(system_prompt_v4_safe("Germany", 2020L))
  per_doc <- smp |>
    mutate(chunk_tok = map(text, \(t) {
      est <- estimate_tokens(t)
      ch <- if (est <= 18000L) list(t) else as.list(chunk_text(t, 20000L, 2000L))
      map_dbl(ch, estimate_tokens)
    }),
    n_chunks_fresh = lengths(chunk_tok),
    in_tok = map_dbl(chunk_tok, \(x) sum(x + sys_tok + 30)))
  calls <- sum(per_doc$n_chunks_fresh)
  in_tok <- sum(per_doc$in_tok)
  est <- tribble(
    ~model,      ~in_mult, ~out_per_call, ~usd_in, ~usd_out,
    "sonnet",    1.35,     2500,          3.00,    15.00,
    "gpt41mini", 1.00,     2000,          0.40,     1.60,
    "gemini",    1.10,     2000,          0.50,     3.00
  ) |>
    mutate(calls = calls * N_RUNS,
           cost_usd = N_RUNS * (in_tok * in_mult * usd_in + calls / N_RUNS * out_per_call * usd_out) / 1e6)
  list(per_doc = per_doc |> select(doc_id, n_chunks_fresh, in_tok),
       by_model = est, total = sum(est$cost_usd) + 0.25)  # + judge allowance
}

# ============================================================
# Scoring job (runs inside a furrr worker; self-contained)
# ============================================================

score_job <- function(job, project_dir) {
  setwd(project_dir)
  # The httr::content trace resolves the recorder in globalenv()
  assign("api_probe_record", api_probe_record, envir = globalenv())
  suppressMessages({
    source("R/1_Functions.R")
    source("R/3c_Populism_v4_safe.R")
  })
  install_api_probe()
  env <- load_production_defs(job$script, job$keep[[1]])
  t0 <- Sys.time()
  res <- tryCatch(
    env[[job$score_fun]](job$doc_id, job$text, job$countryname, lubridate::year(job$date)),
    error = function(e) list(raw = tibble(), spans = tibble(),
                             errors = paste("job_error:", conditionMessage(e))))
  list(model = job$model, run = job$run, doc_id = job$doc_id,
       raw = res$raw, spans = res$spans, errors = res$errors,
       calls = collect_api_probe(),
       elapsed_s = as.numeric(difftime(Sys.time(), t0, units = "secs")),
       finished = Sys.time())
}

ckpt_path <- function(model, run, doc_id) sprintf("%s%s_run%d_%s.rds", CKPT_DIR, model, run, doc_id)

run_jobs <- function(jobs) {
  todo <- jobs |> filter(!file.exists(ckpt_path(model, run, doc_id)))
  if (!nrow(todo)) return(invisible())
  message(sprintf("  %d jobs to score (%d already checkpointed)", nrow(todo), nrow(jobs) - nrow(todo)))
  plan(multisession, workers = min(N_WORKERS, nrow(todo)))
  on.exit(plan(sequential), add = TRUE)
  res <- future_map(
    split(todo, seq_len(nrow(todo))),
    function(job) {
      out <- score_job(job, PROJECT_DIR)
      saveRDS(out, ckpt_path(out$model, out$run, out$doc_id))
      sprintf("%s run%d %s: %d chunks, %d errors, %.0fs", out$model, out$run, out$doc_id,
              nrow(out$raw), length(out$errors), out$elapsed_s)
    },
    .options = furrr_options(
      globals = c("score_job", "load_production_defs", "production_exprs",
                  "install_api_probe", "collect_api_probe", "api_probe_record",
                  "ckpt_path", "CKPT_DIR", "PROJECT_DIR", "%||%"),
      packages = c("tibble", "dplyr", "purrr", "tidyr", "httr", "jsonlite",
                   "rtiktoken", "lubridate", "stringr"),
      seed = TRUE
    )
  )
  walk(res, message)
}

load_checkpoints <- function() {
  fs <- list.files(CKPT_DIR, pattern = "_run[0-9]+_.*\\.rds$", full.names = TRUE)
  map(fs, readRDS)
}

# ============================================================
# STAGE: sample (+ cost estimate)
# ============================================================

smp_file <- paste0(CKPT_DIR, "sample.rds")
if (file.exists(smp_file)) {
  smp <- readRDS(smp_file)
  message("Loaded existing sample (", nrow(smp), " manifestos)")
} else {
  message("Drawing stratified sample ...")
  smp <- build_sample()
  saveRDS(smp, smp_file)
}
cost_est <- estimate_cost(smp)
smp <- smp |> left_join(cost_est$per_doc |> select(doc_id, n_chunks_fresh), by = "doc_id")

sample_out <- smp |>
  transmute(doc_id, countryname, language, partyname, partyabbrev, year, era,
            pop_base = round(pop_base, 2), pop_tercile, tokens_tiktoken,
            n_chunks_prod, n_chunks_fresh, design_multi)
write_csv(sample_out, paste0(CSV_DIR, "sample.csv"))
message(sprintf("Sample: %d manifestos | %d languages | %d countries | eras: %s | multi-chunk: %d (%d chunks total)",
                nrow(smp), n_distinct(smp$language), n_distinct(smp$countryname),
                paste(names(table(smp$era)), table(smp$era), sep = "=", collapse = ", "),
                sum(smp$n_chunks_fresh > 1), sum(smp$n_chunks_fresh)))
if (any(smp$n_chunks_fresh != smp$n_chunks_prod))
  warning("Fresh chunking differs from production chunk count for: ",
          paste(smp$doc_id[smp$n_chunks_fresh != smp$n_chunks_prod], collapse = ", "))
print(cost_est$by_model)
message(sprintf("ESTIMATED total cost: USD %.2f (cap %.2f)", cost_est$total, COST_CAP_USD))
write_csv(cost_est$by_model, paste0(CSV_DIR, "cost_estimate.csv"))
if (cost_est$total > COST_CAP_USD) stop("Estimated cost exceeds cap -- not scoring.")
if (STAGE == "sample") quit(save = "no")

# ============================================================
# STAGE: score
# ============================================================

if (STAGE %in% c("score", "all")) {
  for (k in c("ANTHROPIC_API_KEY", "OPENAI_API_KEY", "GEMINI_API_KEY"))
    if (!nzchar(Sys.getenv(k))) stop(k, " missing")

  make_jobs <- function(run, docs) {
    crossing(model = MODELS$model, doc_id = docs) |>
      left_join(MODELS |> select(model, script, score_fun), by = "model") |>
      mutate(run = run, keep = PROD_KEEP[model]) |>
      left_join(smp |> select(doc_id, text, countryname, date), by = "doc_id") |>
      arrange(match(doc_id, docs), model)
  }
  spent <- function() {
    cps <- load_checkpoints()
    if (!length(cps)) return(0)
    sum(price_calls(bind_rows(map(cps, "calls")))$cost_usd, na.rm = TRUE)
  }

  # Pre-flight: shortest manifesto, run 1, all three models. Stops on errors
  # (e.g., a changed Gemini alias rejecting thinkingBudget = 0).
  first_doc <- smp |> arrange(tokens_tiktoken) |> slice(1) |> pull(doc_id)
  message("\n--- Pre-flight (", first_doc, ") ---")
  run_jobs(make_jobs(1L, first_doc))
  pf <- map(MODELS$model, \(m) readRDS(ckpt_path(m, 1L, first_doc)))
  for (p in pf) {
    message(sprintf("  %-9s resolved=%s | errors=%d | thoughts=%s", p$model,
                    paste(unique(p$calls$resolved), collapse = ","), length(p$errors),
                    paste(unique(p$calls$thought_tokens), collapse = ",")))
    if (length(p$errors)) stop("Pre-flight failed for ", p$model, ": ", paste(p$errors, collapse = " | "))
  }

  for (r in seq_len(N_RUNS)) {
    message(sprintf("\n--- Run %d --- (spent so far: USD %.2f)", r, spent()))
    if (r > 1L) {
      # Project run r from the actual spend of run 1
      cps <- keep(load_checkpoints(), \(x) x$run == 1L)
      run1_cost <- sum(price_calls(bind_rows(map(cps, "calls")))$cost_usd, na.rm = TRUE)
      projected <- spent() + run1_cost * (N_RUNS - r + 1L)
      message(sprintf("  projected total after all runs: USD %.2f", projected))
      if (projected > COST_CAP_USD) stop("Projected cost exceeds cap -- stopping before run ", r)
    }
    run_jobs(make_jobs(r, smp$doc_id))
  }
  message(sprintf("\nScoring done. Actual spend (scoring): USD %.2f", spent()))
}

if (!STAGE %in% c("analyze", "all")) quit(save = "no")

# ============================================================
# STAGE: analyze -- assemble fresh + stored chunk tables
# ============================================================

message("\n=== Analysis ===")
cps <- load_checkpoints()
calls_score <- bind_rows(map(cps, \(x) mutate(x$calls, model = x$model, run = x$run, doc_id = x$doc_id))) |>
  price_calls()

fresh <- map(cps, \(x) list(model = x$model, run = x$run,
                            raw = x$raw, spans = x$spans, errors = x$errors))
fresh_raw <- map_dfr(fresh, \(x) mutate(x$raw, model = x$model, run = x$run))
fresh_err <- tibble(model = map_chr(fresh, "model"), run = map_int(fresh, "run"),
                    n_err = map_int(fresh, \(x) length(x$errors)))
if (!"scoring_error" %in% names(fresh_raw)) fresh_raw$scoring_error <- NA_character_
message(sprintf("Fresh chunk rows: %d | persistent chunk errors: %d",
                nrow(fresh_raw), sum(!is.na(fresh_raw$scoring_error))))

stored_raw <- imap_dfr(STORED_RAW, \(p, m) read_parquet(p) |>
                         filter(doc_id_root %in% smp$doc_id) |>
                         mutate(model = m, run = 0L))
stored_val <- imap_dfr(STORED_VAL, \(p, m) read_parquet(p) |>
                         filter(doc_id_root %in% smp$doc_id) |> mutate(model = m, run = 0L))

ok_rows <- function(x) x |> filter(is.na(scoring_error), !is.na(schema_version))

# Fresh outputs without manifesto text (quotes <= 8 words are fine)
write_parquet(fresh_raw, paste0(OUT_DIR, "fresh_raw_chunks.parquet"))

# ---- Span validation of fresh runs (production validator + judge) ----
val_file <- paste0(CKPT_DIR, "fresh_validated_spans.rds")
if (A13_BASE == "v2026a" && !file.exists(val_file)) stop("archived fresh span validation missing: ", val_file)
if (file.exists(val_file)) {
  fresh_val <- readRDS(val_file)
  calls_judge <- readRDS(paste0(CKPT_DIR, "judge_calls.rds"))
} else {
  install_api_probe()
  sp_for_check <- system_prompt_v4_safe("X", 2020L)
  fresh_val <- list(); calls_judge <- list()
  for (x in split(fresh, paste(map_chr(fresh, "model"), map_int(fresh, "run")))) {
    m <- x[[1]]$model; r <- x[[1]]$run
    spans <- bind_rows(map(x, "spans"))
    rm(list = ls(envir = .judge_cache, all.names = TRUE), envir = .judge_cache)  # fresh session
    chunk_text_lookup <- setNames(spans$chunk_text, spans$doc_id)
    v <- validate_spans_table_with_judge(
      spans |> select(doc_id, doc_id_root, dim_name, quote, context),
      chunk_text_lookup, sp_for_check, TOPIC_KEYWORDS_V4,
      use_judge = TRUE, verbose = FALSE)
    fresh_val[[paste(m, r)]] <- mutate(v, model = m, run = r)
    calls_judge[[paste(m, r)]] <- collect_api_probe() |> mutate(model = m, run = r)
    message(sprintf("  validated %s run%d: %d spans, %.1f%% quality-verified, %d judge calls",
                    m, r, nrow(v), 100 * mean(v$quality_verified), sum(v$judge_used)))
  }
  fresh_val <- bind_rows(fresh_val); calls_judge <- bind_rows(calls_judge) |> price_calls()
  saveRDS(fresh_val, val_file); saveRDS(calls_judge, paste0(CKPT_DIR, "judge_calls.rds"))
}
write_parquet(fresh_val, paste0(OUT_DIR, "fresh_validated_spans.parquet"))

calls_all <- bind_rows(calls_score |> mutate(stage = "scoring"),
                       calls_judge |> mutate(stage = "judge"))
write_csv(calls_all, paste0(CSV_DIR, "api_calls.csv"))
total_cost <- sum(calls_all$cost_usd, na.rm = TRUE)
message(sprintf("ACTUAL total cost: USD %.2f", total_cost))

# ---- Model versions: requested vs resolved ----
versions <- calls_score |>
  group_by(model) |>
  summarise(resolved = paste(sort(unique(resolved)), collapse = "; "),
            n_calls = n(),
            mean_in = round(mean(input_tokens, na.rm = TRUE)),
            mean_out = round(mean(output_tokens, na.rm = TRUE)),
            max_thought_tokens = max(thought_tokens, na.rm = TRUE),
            finish = paste(sort(unique(finish)), collapse = "; "),
            cost_usd = round(sum(cost_usd, na.rm = TRUE), 2), .groups = "drop") |>
  left_join(MODELS |> select(model, requested_id), by = "model") |>
  mutate(production_recorded = case_when(
    model == "gpt41mini" ~ "gpt-4.1-mini-2025-04-14 (pinned snapshot; resolved id not logged)",
    model == "sonnet"    ~ "claude-sonnet-4-6 (requested id; resolved id not logged)",
    model == "gemini"    ~ "gemini-flash-latest (ALIAS; resolved modelVersion not logged, run 2026-06-05/06)"))
write_csv(versions, paste0(CSV_DIR, "model_versions.csv"))
print(versions)

# ============================================================
# Chunk-level determinism
# ============================================================

to_long_chunks <- function(raw) {
  raw <- ok_rows(raw)
  map_dfr(V4_DIMS, \(d) raw |>
    transmute(model, run, doc_id, doc_id_root, dim = d,
              score  = .data[[paste0(d, "_score")]],
              status = .data[[paste0(d, "_status")]],
              quote  = .data[[paste0(d, "_quote")]]))
}
chunks_long <- bind_rows(to_long_chunks(fresh_raw), to_long_chunks(stored_raw))
conf_long <- bind_rows(ok_rows(fresh_raw), ok_rows(stored_raw)) |>
  select(model, run, doc_id, all_of(CONF_VARS)) |>
  pivot_longer(all_of(CONF_VARS), names_to = "dim", values_to = "score")

COMPARISONS <- tribble(~comparison, ~ra, ~rb, ~kind,
                       "run1_vs_run2", 1L, 2L, "Run-to-run",
                       "run1_vs_prod", 1L, 0L, "Fresh vs production",
                       "run2_vs_prod", 2L, 0L, "Fresh vs production")

pair_up <- function(long, ra, rb, by = c("model", "doc_id", "dim")) {
  inner_join(long |> filter(run == ra) |> select(-run),
             long |> filter(run == rb) |> select(-run),
             by = by, suffix = c("_a", "_b"))
}

chunk_metrics <- function(p) {
  both <- !is.na(p$score_a) & !is.na(p$score_b)
  qa <- coalesce(p$quote_a, ""); qb <- coalesce(p$quote_b, "")
  tibble(
    n_pairs        = nrow(p),
    n_both_scored  = sum(both),
    exact_score    = mean(p$score_a[both] == p$score_b[both]),
    within_1       = mean(abs(p$score_a[both] - p$score_b[both]) <= 1),
    mad            = mean(abs(p$score_a[both] - p$score_b[both])),
    r              = if (sum(both) > 2) cor(p$score_a[both], p$score_b[both]) else NA_real_,
    na_agree       = mean(is.na(p$score_a) == is.na(p$score_b)),
    status_agree   = mean(coalesce(p$status_a, "<NA>") == coalesce(p$status_b, "<NA>")),
    noevid_agree   = mean((coalesce(p$status_a, "") == "no_evidence") ==
                          (coalesce(p$status_b, "") == "no_evidence")),
    quote_identical= mean(qa[nzchar(qa) & nzchar(qb)] == qb[nzchar(qa) & nzchar(qb)]),
    all_identical  = mean(coalesce(p$score_a == p$score_b, is.na(p$score_a) & is.na(p$score_b)) &
                          coalesce(p$status_a, "") == coalesce(p$status_b, "") & qa == qb)
  )
}

chunk_res_dim <- pmap_dfr(COMPARISONS, function(comparison, ra, rb, kind) {
  pair_up(chunks_long, ra, rb) |> group_by(model, dim) |>
    group_modify(\(d, k) chunk_metrics(d)) |> ungroup() |>
    mutate(comparison = comparison, kind = kind, .before = 1)
})
# Pooled over dimensions (and models). Correlation = mean of per-dimension r
# (pooling raw pairs across dimensions would inflate r via level differences).
chunk_res <- pmap_dfr(COMPARISONS, function(comparison, ra, rb, kind) {
  p <- pair_up(chunks_long, ra, rb)
  bind_rows(
    p |> group_by(model) |> group_modify(\(d, k) chunk_metrics(d)) |> ungroup(),
    chunk_metrics(p) |> mutate(model = "pooled")
  ) |> mutate(comparison = comparison, kind = kind, .before = 1)
}) |>
  select(-r) |>
  left_join(bind_rows(
    chunk_res_dim |> group_by(comparison, model) |> summarise(r = mean(r, na.rm = TRUE), .groups = "drop"),
    chunk_res_dim |> group_by(comparison) |> summarise(r = mean(r, na.rm = TRUE), .groups = "drop") |>
      mutate(model = "pooled")),
    by = c("comparison", "model"))
conf_res <- pmap_dfr(COMPARISONS, function(comparison, ra, rb, kind) {
  p <- pair_up(conf_long, ra, rb)
  both <- !is.na(p$score_a) & !is.na(p$score_b)
  p[both, ] |> group_by(model, dim) |>
    summarise(n = n(), exact = mean(score_a == score_b), mad = mean(abs(score_a - score_b)),
              r = suppressWarnings(cor(score_a, score_b)), .groups = "drop") |>
    mutate(comparison = comparison, .before = 1)
})
# Chunk structure: identical chunk ids?
chunk_struct <- bind_rows(ok_rows(fresh_raw), ok_rows(stored_raw)) |>
  count(model, run, doc_id_root, name = "n_chunks") |>
  pivot_wider(names_from = run, values_from = n_chunks, names_prefix = "run")
write_csv(chunk_res, paste0(CSV_DIR, "chunk_level_determinism.csv"))
write_csv(chunk_res_dim, paste0(CSV_DIR, "chunk_level_determinism_by_dim.csv"))
write_csv(conf_res, paste0(CSV_DIR, "chunk_level_confidence.csv"))
write_csv(chunk_struct, paste0(CSV_DIR, "chunk_structure.csv"))

# ============================================================
# Manifesto-level determinism (both production aggregations)
# ============================================================

agg_all <- function() {
  srcs <- distinct(bind_rows(fresh_raw, stored_raw), model, run)
  pmap_dfr(srcs, function(model, run) {
    m <- model; r <- run
    raw <- bind_rows(fresh_raw, stored_raw) |> filter(model == m, run == r)
    val <- bind_rows(fresh_val, stored_val) |> filter(model == m, run == r)
    bind_rows(
      agg_baseline(raw |> select(-model, -run), m) |>
        pivot_longer(-doc_id, names_to = "dim", values_to = "score") |>
        mutate(aggregation = "baseline_cwm"),
      agg_validated(raw |> select(-model, -run), val) |>
        pivot_longer(-doc_id, names_to = "dim", values_to = "score") |>
        filter(dim %in% BASE_DIMS) |>
        mutate(aggregation = "validated_mean")
    ) |> mutate(model = m, run = r)
  }) |> mutate(score = ifelse(is.finite(score), score, NA_real_))
}
if (A13_BASE == "v2026a") {
  # Canonical aggregation (R/1_Aggregation.R) on fresh and stored chunks alike; span checks
  # from the R/12 flags of each run's own spans.
  agg_all <- function() {
    srcs <- distinct(bind_rows(fresh_raw, stored_raw), model, run)
    pmap_dfr(srcs, function(model, run) {
      m <- model; r <- run
      raw <- bind_rows(fresh_raw, stored_raw) |> filter(model == m, run == r) |> ok_rows()
      val <- bind_rows(fresh_val, stored_val) |> filter(model == m, run == r) |>
        transmute(doc_id, dim_name, quote_ok = verbatim_found %in% TRUE & !(hallucination_flag %in% TRUE),
                  topic_ok = quality_verified %in% TRUE) |>
        distinct(doc_id, dim_name, .keep_all = TRUE)
      cl <- chunk_dim_long(raw, val, m)
      bind_rows(
        aggregate_within(cl, validation = "quote") |> transmute(doc_id, dim, score = cwm, aggregation = "baseline_cwm"),
        aggregate_within(cl, validation = "none") |> transmute(doc_id, dim, score = cwm_pop, aggregation = "validated_mean")
      ) |> filter(dim %in% BASE_DIMS) |> mutate(model = m, run = r)
    }) |> mutate(score = ifelse(is.finite(score), score, NA_real_))
  }
}
man <- agg_all()
# Cross-model mean (= the paper's between-model step, plain mean)
man_xm <- man |> group_by(aggregation, run, doc_id, dim) |>
  summarise(score = if (all(is.na(score))) NA_real_ else mean(score, na.rm = TRUE),
            .groups = "drop") |>
  mutate(model = "cross_model_mean")
man <- bind_rows(man, man_xm)
write_csv(man, paste0(CSV_DIR, "manifesto_scores_runs.csv"))

man_metrics <- function(p) {
  both <- !is.na(p$score_a) & !is.na(p$score_b)
  d <- abs(p$score_a[both] - p$score_b[both])
  tibble(n = sum(both),
         na_mismatch = sum(is.na(p$score_a) != is.na(p$score_b)),
         identical = mean(d < 1e-9),
         mad = mean(d), max_dev = if (length(d)) max(d) else NA_real_,
         r = if (sum(both) > 2) cor(p$score_a[both], p$score_b[both]) else NA_real_)
}
# "ALL" rows summarise the per-dimension metrics (mean MAD / r / identical,
# max of max_dev, sum of n and NA mismatches) -- not raw pairs pooled across dims.
summ_dims <- function(x) x |> summarise(
  n = sum(n), na_mismatch = sum(na_mismatch), identical = mean(identical, na.rm = TRUE),
  mad = mean(mad, na.rm = TRUE), max_dev = suppressWarnings(max(max_dev, na.rm = TRUE)),
  r = mean(r, na.rm = TRUE), .groups = "drop")
man_res <- pmap_dfr(COMPARISONS, function(comparison, ra, rb, kind) {
  p <- pair_up(man, ra, rb, by = c("aggregation", "model", "doc_id", "dim"))
  by_dim <- p |> group_by(aggregation, model, dim) |> group_modify(\(d, k) man_metrics(d)) |> ungroup()
  bind_rows(
    by_dim,
    by_dim |> group_by(aggregation, model) |> summ_dims() |> mutate(dim = "ALL"),
    by_dim |> filter(model != "cross_model_mean") |> group_by(aggregation) |> summ_dims() |>
      mutate(model = "pooled", dim = "ALL")
  ) |> mutate(comparison = comparison, kind = kind, .before = 1)
})
write_csv(man_res, paste0(CSV_DIR, "manifesto_level_determinism.csv"))

# ---- Drift test: is |fresh - production| larger than |run1 - run2|? ----
# Under a stationary serving model both are draws of the same run-to-run
# noise; a significantly larger fresh-vs-production gap (paired over
# manifesto x dimension) plus a signed shift indicates the model behind the
# endpoint changed since the production run.
drift <- man |> filter(aggregation == "baseline_cwm") |>
  pivot_wider(names_from = run, values_from = score, names_prefix = "r") |>
  filter(!is.na(r0), !is.na(r1), !is.na(r2)) |>
  mutate(gap = (abs(r1 - r0) + abs(r2 - r0)) / 2 - abs(r1 - r2),
         shift = (r1 + r2) / 2 - r0, is_pop = str_starts(dim, "pop")) |>
  group_by(model) |>
  summarise(n_cells = n(), n_docs = n_distinct(doc_id),
            mad_run1_run2   = mean(abs(r1 - r2)),
            mad_fresh_prod  = mean(abs(c(r1, r2) - c(r0, r0))),
            signed_fresh_minus_prod = mean(shift),
            signed_pop_fresh_minus_prod = mean(shift[is_pop]),
            # Inference clustered by manifesto: average within document, t-test across documents
            p_gap_larger   = t.test(tapply(gap, doc_id, mean))$p.value,
            p_signed_shift = t.test(tapply(shift, doc_id, mean))$p.value,
            p_pop_shift    = t.test(tapply(shift[is_pop], doc_id[is_pop], mean))$p.value,
            .groups = "drop")
write_csv(drift, paste0(CSV_DIR, "drift_test.csv"))
print(drift)

# ============================================================
# Full-corpus cross-family agreement (stored production, no API cost)
# ============================================================

message("\n=== Full-corpus cross-family agreement ===")
corpus <- if (A13_BASE == "v2026a") {
  # per-model manifesto scores of the v2026a baseline (its span-validation rule)
  b <- read_parquet(file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet"))
  map_dfr(names(AGG_FILE_STEM), \(m) b |> select(doc_id, all_of(paste0(BASE_DIMS, "_", m))) |>
            rename_with(\(x) sub(paste0("_", m, "$"), "", x)) |> mutate(model = m)) |>
    pivot_longer(all_of(BASE_DIMS), names_to = "dim", values_to = "score") |>
    mutate(score = ifelse(is.finite(score), score, NA_real_))
} else {
  imap_dfr(STORED_RAW, \(p, m) agg_baseline(read_parquet(p), m) |> mutate(model = m)) |>
    pivot_longer(all_of(BASE_DIMS), names_to = "dim", values_to = "score") |>
    mutate(score = ifelse(is.finite(score), score, NA_real_))
}

# Sanity: recomputed cross-model baseline == published manifesto_scores.parquet
if (A13_BASE == "june" && file.exists("website/data/manifesto_scores.parquet")) {
  pub <- read_parquet("website/data/manifesto_scores.parquet") |>
    select(doc_id, all_of(BASE_DIMS)) |>
    pivot_longer(-doc_id, names_to = "dim", values_to = "pub")
  chk <- corpus |> group_by(doc_id, dim) |>
    summarise(mine = if (all(is.na(score))) NA_real_ else mean(score, na.rm = TRUE), .groups = "drop") |>
    inner_join(pub, by = c("doc_id", "dim"))
  message(sprintf("  baseline check vs manifesto_scores.parquet: n=%d, max |diff| = %.4f, NA mismatches = %d",
                  nrow(chk), max(abs(chk$mine - chk$pub), na.rm = TRUE),
                  sum(is.na(chk$mine) != is.na(chk$pub))))
}

# ICC from two-way ANOVA mean squares (Shrout & Fleiss 1979; McGraw & Wong 1996)
icc_twoway <- function(mat) {
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  n <- nrow(mat); k <- ncol(mat)
  gm  <- mean(mat)
  msr <- k * sum((rowMeans(mat) - gm)^2) / (n - 1)
  msc <- n * sum((colMeans(mat) - gm)^2) / (k - 1)
  sse <- sum((mat - outer(rowMeans(mat), rep(1, k)) - outer(rep(1, n), colMeans(mat)) + gm)^2)
  mse <- sse / ((n - 1) * (k - 1))
  c(n = n,
    icc_a1 = (msr - mse) / (msr + (k - 1) * mse + k * (msc - mse) / n),   # absolute agreement
    icc_c1 = (msr - mse) / (msr + (k - 1) * mse))                         # consistency
}

wide <- corpus |> pivot_wider(names_from = model, values_from = score)
pairs <- list(c("sonnet", "gpt41mini"), c("sonnet", "gemini"), c("gpt41mini", "gemini"))
xfam <- map_dfr(BASE_DIMS, function(d) {
  w <- wide |> filter(dim == d)
  pr <- map_dfr(pairs, \(p) {
    a <- w[[p[1]]]; b <- w[[p[2]]]; ok <- !is.na(a) & !is.na(b)
    ic <- icc_twoway(cbind(a[ok], b[ok]))
    tibble(pair = paste(p, collapse = "_"), n = sum(ok), r = cor(a[ok], b[ok]),
           rho = cor(a[ok], b[ok], method = "spearman"),
           mad = mean(abs(a[ok] - b[ok])), icc_a1 = ic[["icc_a1"]], icc_c1 = ic[["icc_c1"]])
  })
  ic3 <- icc_twoway(as.matrix(w[, c("sonnet", "gpt41mini", "gemini")]))
  pr <- bind_rows(pr, tibble(pair = "all_three", n = ic3[["n"]], icc_a1 = ic3[["icc_a1"]],
                             icc_c1 = ic3[["icc_c1"]]))
  # Each model vs the cross-model mean (all three) and vs leave-one-out mean
  vs <- map_dfr(c("sonnet", "gpt41mini", "gemini"), \(m) {
    others <- setdiff(c("sonnet", "gpt41mini", "gemini"), m)
    xm  <- rowMeans(as.matrix(w[, c("sonnet", "gpt41mini", "gemini")]), na.rm = TRUE)
    loo <- rowMeans(as.matrix(w[, others]), na.rm = TRUE)
    a <- w[[m]]; ok1 <- !is.na(a) & is.finite(xm); ok2 <- !is.na(a) & is.finite(loo)
    tibble(pair = paste0(m, "_vs_mean"), n = sum(ok1), r = cor(a[ok1], xm[ok1]),
           mad = mean(abs(a[ok1] - xm[ok1])), r_loo = cor(a[ok2], loo[ok2]))
  })
  bind_rows(pr, vs) |> mutate(dim = d, .before = 1)
})
write_csv(xfam, paste0(CSV_DIR, "crossfamily_agreement_fullcorpus.csv"))

# ============================================================
# LaTeX tables (booktabs, style of Appendix A.13)
# ============================================================

tt  <- function(x) paste0("\\texttt{", gsub("_", "\\\\_", x), "}")
f3  <- function(x) ifelse(is.na(x), "--", formatC(x, format = "f", digits = 3))
f2  <- function(x) ifelse(is.na(x), "--", formatC(x, format = "f", digits = 2))
pct <- function(x) ifelse(is.na(x), "--", paste0(formatC(100 * x, format = "f", digits = 1), "\\%"))
big <- function(x) formatC(x, format = "d", big.mark = "{,}")
write_tab <- function(lines, file) writeLines(lines, paste0(TAB_DIR, file))
MOD_LAB <- c(sonnet = "Sonnet 4.6", gpt41mini = "GPT-4.1-mini", gemini = "Gemini 3.5 Flash",
             pooled = "Pooled", cross_model_mean = "Cross-model mean")
# PLLM-0240 / PLLM-0258 (2026-11-11): dimension labels of Table A.1 (tab:evidence_shares, R/41) instead
# of texttt variable names; model name "Gemini 3.5 Flash" (one name per object)
DIM_LAB <- c(populism_overall = "Populism (overall)", pop_anti_elitism = "Anti-elitism",
             pop_people_centrism = "People-centrism", pop_manichean = "Manichaean",
             pop_ideology_overall = "Ideology (overall)", lib_political = "Political liberalism",
             lib_social = "Social liberalism", lib_economic = "Economic liberalism",
             lib_financial_market = "Financial-market liberalism", liberalism_overall = "Liberalism (overall)")
latex_esc <- function(x) x |> str_replace_all("([&%$#_{}])", "\\\\\\1")

# ---- Table A.13-1: sample ----
t1 <- smp |> arrange(language, year) |>
  mutate(line = sprintf("%s & %s & %s & %s & %d & %s & %d & %s \\\\",
                        tt(doc_id), latex_esc(countryname), language,
                        latex_esc(str_trunc(if_else(is.na(partyabbrev) | partyabbrev == "", partyname, partyabbrev), 18)),
                        year, big(round(tokens_tiktoken)), n_chunks_fresh,
                        c("low", "mid", "high")[pop_tercile]))
write_tab(c(
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Manifestos selected for reproducibility verification}",
  "\\label{tab:verify_manifestos}", "\\footnotesize",
  "\\begin{tabular}{lllllrrl}", "\\toprule",
  "\\texttt{doc\\_id} & Country & Language & Party & Year & Tokens & Chunks & Populism \\\\",
  "\\midrule", t1$line, "\\bottomrule", "\\end{tabular}",
  "\\par\\smallskip\\parbox{0.95\\linewidth}{\\scriptsize\\textit{Notes:} Stratified draw (seed 20261007) over 10 languages $\\times$ 2, three election eras, and terciles of the stored cross-model populism baseline. Tokens: o200k tokenizer, as used for chunking; documents above 18{,}000 tokens are split into 20{,}000-token chunks with 2{,}000-token overlap.}",
  "\\end{table}"), "tab_A13_1_sample.tex")

# ---- Table A.13-2: chunk-level determinism ----
cr <- chunk_res |>
  mutate(kind = if_else(comparison == "run1_vs_run2", "rr", "fp")) |>
  group_by(kind, model) |>
  summarise(across(c(n_pairs, n_both_scored), \(x) round(mean(x))),
            across(c(exact_score, within_1, mad, r, na_agree, status_agree, quote_identical, all_identical), mean),
            .groups = "drop")
row_def <- tribble(~var, ~lab, ~fmt,
  "all_identical",   "Chunk-dimension fully identical (score, status, quote)", "pct",
  "exact_score",     "Exact score agreement (both scored)", "pct",
  "within_1",        "Agreement within $\\pm$1 point", "pct",
  "mad",             "Mean absolute difference", "f3",
  "r",               "Pearson correlation", "f3",
  "status_agree",    "\\texttt{evidence\\_status} agreement", "pct",
  "na_agree",        "Score NA/non-NA agreement", "pct",
  "quote_identical", "Identical evidence quote", "pct",
  "n_pairs",         "Chunk $\\times$ dimension pairs", "int")
mk_panel <- function(k) {
  cols <- c("sonnet", "gpt41mini", "gemini", "pooled")
  pmap_chr(row_def, function(var, lab, fmt) {
    v <- map_dbl(cols, \(m) cr[[var]][cr$kind == k & cr$model == m] %||% NA_real_)
    vv <- switch(fmt, pct = pct(v), f3 = f3(v), int = big(as.integer(v)))
    paste0(lab, " & ", paste(vv, collapse = " & "), " \\\\")
  })
}
write_tab(c(
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Chunk-level determinism of the production pipeline (13 dimensions per chunk)}",
  "\\label{tab:verify_determinism}", "\\scriptsize",
  "\\begin{tabular}{lrrrr}", "\\toprule",
  " & Sonnet 4.6 & GPT-4.1-mini & Gemini 3.5 Flash & Pooled \\\\", "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel A: Run 1 vs.\\ Run 2 (fresh, identical settings)}} \\\\",
  mk_panel("rr"), "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel B: Fresh runs vs.\\ stored production scores}} \\\\",
  "\\multicolumn{5}{l}{\\textit{(average of Run 1 and Run 2)}} \\\\",
  mk_panel("fp"), "\\bottomrule", "\\end{tabular}",
  "\\par\\smallskip\\parbox{0.95\\linewidth}{\\scriptsize\\textit{Notes:} \\texttt{temperature}\\,=\\,0 for all models; identical \\texttt{poplib\\_v4} schema, system prompt and chunking as in production. Score metrics use pairs where both runs return a numeric score; status and NA agreement use all pairs. Production scores: ", PROD_LABEL, ".}",
  "\\end{table}"), "tab_A13_2_chunk_determinism.tex")

# ---- Table A.13-3: manifesto-level per dimension (baseline aggregation) ----
mr <- man_res |> filter(aggregation == "baseline_cwm", comparison == "run1_vs_run2")
mcols <- c("sonnet", "gpt41mini", "gemini", "cross_model_mean")
t3 <- map_chr(c(BASE_DIMS, "ALL"), function(d) {
  vals <- map_chr(mcols, \(m) {
    x <- mr |> filter(model == m, dim == d)
    if (!nrow(x)) return("-- & --")
    if (m == "cross_model_mean") sprintf("%s & %s & %s", f3(x$mad), f3(x$r), f2(x$max_dev))
    else sprintf("%s & %s", f3(x$mad), f3(x$r))
  })
  lab <- if (d == "ALL") "All 10 dimensions" else DIM_LAB[[d]]
  paste0(lab, " & ", paste(vals, collapse = " & "), " \\\\")
})
write_tab(c(
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Manifesto-level determinism, Run 1 vs.\\ Run 2 (baseline aggregation, $n=20$)}",
  "\\label{tab:verify_manifesto}", "\\scriptsize",
  "\\begin{tabular}{lrrrrrrrrr}", "\\toprule",
  " & \\multicolumn{2}{c}{Sonnet 4.6} & \\multicolumn{2}{c}{GPT-4.1-mini} & \\multicolumn{2}{c}{Gemini 3.5 Flash} & \\multicolumn{3}{c}{Cross-model mean} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}\\cmidrule(lr){8-10}",
  "Variable & MAD & $r$ & MAD & $r$ & MAD & $r$ & MAD & $r$ & Max \\\\", "\\midrule",
  head(t3, -1), "\\midrule", tail(t3, 1), "\\bottomrule", "\\end{tabular}",
  if (A13_BASE == "v2026a") "\\par\\smallskip\\parbox{0.95\\linewidth}{\\scriptsize\\textit{Notes:} Baseline aggregation as in the paper: within model, confidence-weighted mean (the dimension's own confidence block) over chunks with status \\texttt{verified} whose evidence quote passes the quote check (verbatim in the source chunk and not copied from the prompt); plain mean across the three models. MAD and Max on the 0--10 scale.}" else
    "\\par\\smallskip\\parbox{0.95\\linewidth}{\\scriptsize\\textit{Notes:} Baseline aggregation as in the paper: confidence-weighted mean over \\texttt{verified} chunks within model, plain mean across the three models. MAD and Max on the 0--10 scale.}",
  "\\end{table}"), "tab_A13_3_manifesto_determinism.tex")

# ---- Table A.13-3b: summary by aggregation rule and comparison ----
sr <- man_res |> filter(dim == "ALL") |>
  mutate(cmp = if_else(comparison == "run1_vs_run2", "rr", "fp")) |>
  group_by(aggregation, cmp, model) |>
  summarise(identical = mean(identical), mad = mean(mad), max_dev = max(max_dev),
            r = mean(r), na_mismatch = mean(na_mismatch), .groups = "drop")
t3b <- map(c("rr", "fp"), function(k) {
  map_chr(c("baseline_cwm", "validated_mean"), function(a) {
    vals <- map_chr(c("sonnet", "gpt41mini", "gemini", "cross_model_mean"), \(m) {
      x <- sr |> filter(aggregation == a, cmp == k, model == m)
      sprintf("%s & %s & %s", f3(x$mad), f3(x$r), pct(x$identical))
    })
    paste0(if (k == "rr") "Run 1 vs.\\ 2" else "Fresh vs.\\ prod.", " & ",
           if (a == "baseline_cwm") (if (A13_BASE == "v2026a") "Canonical" else "Baseline") else
             (if (A13_BASE == "v2026a") "No span check" else "Validated"), " & ",
           paste(vals, collapse = " & "), " \\\\")
  })
}) |> unlist()
write_tab(c(
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Manifesto-level determinism by aggregation rule (all 10 dimensions, $n=20$)}",
  "\\label{tab:verify_aggregation}", "\\scriptsize", "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabular}{llrrrrrrrrrrrr}", "\\toprule",
  " & & \\multicolumn{3}{c}{Sonnet 4.6} & \\multicolumn{3}{c}{GPT-4.1-mini} & \\multicolumn{3}{c}{Gemini 3.5 Flash} & \\multicolumn{3}{c}{Cross-model mean} \\\\",
  "\\cmidrule(lr){3-5}\\cmidrule(lr){6-8}\\cmidrule(lr){9-11}\\cmidrule(lr){12-14}",
  "Comparison & Aggr. & MAD & $r$ & Ident. & MAD & $r$ & Ident. & MAD & $r$ & Ident. & MAD & $r$ & Ident. \\\\",
  "\\midrule", t3b, "\\bottomrule", "\\end{tabular}",
  if (A13_BASE == "v2026a") "\\par\\smallskip\\parbox{0.95\\linewidth}{\\scriptsize\\textit{Notes:} Canonical: the paper's aggregation (own-block confidence-weighted mean over \\texttt{verified} chunks whose quote passes the quote check; plain mean across models). No span check: confidence-weighted mean over all \\texttt{verified} chunks with populism confidence as the weight for every dimension. Ident.: share of manifesto $\\times$ dimension scores identical to $10^{-9}$. Fresh vs.\\ prod.\\ averages Run 1 and Run 2.}" else
  "\\par\\smallskip\\parbox{0.95\\linewidth}{\\scriptsize\\textit{Notes:} Baseline: confidence-weighted mean over \\texttt{verified} chunks (paper headline). Validated: mean over \\texttt{verified}/\\texttt{mixed} chunks whose evidence span passes the A.7 checks (anti-prompt, verbatim, on-topic + LLM judge); the judge is re-run independently for each fresh run. Ident.: share of manifesto $\\times$ dimension scores identical to $10^{-9}$. Fresh vs.\\ prod.\\ averages Run 1 and Run 2.}",
  "\\end{table}"), "tab_A13_3b_aggregation_summary.tex")

# ---- Table A.13-4: side-by-side headline aggregates (cross-model mean) ----
sbs <- man |> filter(aggregation == "baseline_cwm", model == "cross_model_mean",
                     dim %in% c("populism_overall", "liberalism_overall")) |>
  select(doc_id, dim, run, score) |>
  pivot_wider(names_from = c(dim, run), values_from = score) |>
  left_join(smp |> select(doc_id, countryname, year), by = "doc_id") |>
  arrange(countryname, year)
t4 <- sbs |> mutate(line = sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\",
  tt(doc_id), latex_esc(countryname),
  f2(populism_overall_1), f2(populism_overall_2), f2(populism_overall_0),
  f2(liberalism_overall_1), f2(liberalism_overall_2), f2(liberalism_overall_0)))
write_tab(c(
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Side-by-side cross-model baseline scores (Run 1, Run 2, production)}",
  "\\label{tab:verify_sidebyside}", "\\footnotesize",
  "\\begin{tabular}{llrrrrrr}", "\\toprule",
  " & & \\multicolumn{3}{c}{Populism (overall)} & \\multicolumn{3}{c}{Liberalism (overall)} \\\\",
  "\\cmidrule(lr){3-5}\\cmidrule(lr){6-8}",
  "\\texttt{doc\\_id} & Country & Run 1 & Run 2 & Prod. & Run 1 & Run 2 & Prod. \\\\",
  "\\midrule", t4$line, "\\bottomrule", "\\end{tabular}", "\\end{table}"),
  "tab_A13_4_sidebyside.tex")

# ---- Table A.13-5: full-corpus cross-family agreement ----
t5 <- map_chr(BASE_DIMS, function(d) {
  x <- xfam |> filter(dim == d)
  g <- \(p, v) x[[v]][x$pair == p] %||% NA_real_
  sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\", DIM_LAB[[d]],
          big(as.integer(g("all_three", "n"))),
          f3(g("sonnet_gpt41mini", "r")), f3(g("sonnet_gemini", "r")), f3(g("gpt41mini_gemini", "r")),
          f3(g("all_three", "icc_a1")), f3(g("all_three", "icc_c1")),
          f3(g("sonnet_vs_mean", "r_loo")), f3(g("gpt41mini_vs_mean", "r_loo")), f3(g("gemini_vs_mean", "r_loo")))
})
write_tab(c(
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Full-corpus cross-family agreement of manifesto-level scores}",
  "\\label{tab:verify_crossmodel}", "\\scriptsize",
  "\\begin{tabular}{lrrrrrrrrr}", "\\toprule",
  " & & \\multicolumn{3}{c}{Pairwise Pearson $r$} & \\multicolumn{2}{c}{ICC (3 models)} & \\multicolumn{3}{c}{$r$ with mean of other two} \\\\",
  "\\cmidrule(lr){3-5}\\cmidrule(lr){6-7}\\cmidrule(lr){8-10}",
  "Variable & $n$ & S--G & S--Gem & G--Gem & ICC(A,1) & ICC(C,1) & Sonnet & GPT & Gemini \\\\",
  "\\midrule", t5, "\\bottomrule", "\\end{tabular}",
  "\\par\\smallskip\\parbox{0.95\\linewidth}{\\scriptsize\\textit{Notes:} ", if (A13_BASE == "v2026a") "Per-model manifesto scores, baseline aggregation." else "Stored production scores, baseline (confidence-weighted) within-model aggregation.", " S = Claude Sonnet 4.6, G = GPT-4.1-mini, Gem = Gemini 3.5 Flash. $n$: manifestos scored by all three models (ICC); pairwise statistics use all available pairs. ICC(A,1): two-way, absolute agreement, single rater; ICC(C,1): consistency.}",
  "\\end{table}"), "tab_A13_5_crossfamily_corpus.tex")

writeLines(capture.output(sessionInfo()), paste0(LOG_DIR, "sessionInfo.txt"))
message(sprintf("\nDone. Tables in %s | CSVs in %s | total cost USD %.2f", TAB_DIR, CSV_DIR, total_cost))
