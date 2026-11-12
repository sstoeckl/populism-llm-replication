# ============================================================
# run_all.R — the canonical v2026a chain (Phase 2)
#
#   Rscript run_all.R                 all default stages (no API calls), in order
#   Rscript run_all.R baseline tables only the named stages (each in a fresh R process)
#   Rscript run_all.R --list          show the stages
#
# Default stages start from the archived per-chunk scores (data/CMP/v2026a); from there a
# rerun reproduces every output byte for byte (R/71_Verify.R checks this against
# output/v2026a/MANIFEST.md5). The API stages (corpus download, scoring, judge) are
# documented and rerunnable, but LLM scoring is not deterministic: the archived raw files
# are the reproducibility object. API stages run only when named explicitly.
#
# Environment: PIPE_OUT / PIPE_DERIVED redirect outputs (used by R/71);
# BASELINE_VALIDATION selects the span-validation rule (default "quote" = rule (a'), see
# R/1_Aggregation.R); PIPE_PUBLIC=1 runs the public stages from the redacted inputs of R/72
# (data/CMP/v2026a_public): checks_public replaces checks, and ind2 / a13 / compare, which need
# manifesto text or the paper source, are not run; `oa` runs without its quote-class table (full
# quotes and contexts), and `numbers` writes only numbers_public.tex / csv/46_numbers_public.csv
# (macros from restricted outputs as placeholders).
# `numbers` must come after every stage whose csv files it reads (tables ... a13).
# ============================================================

STAGES <- list(
  # ---- API stages: documented, never run by default --------------------------------
  prep     = list(api = TRUE,  cmd = "R/30_CMP2026a_Prep.R",            what = "MPDS2026a + corpus 2026-1 download, update classification (Manifesto API)"),
  probe    = list(api = TRUE,  cmd = "R/31_CMP2026a_Model_Probe.R",     what = "check that the production model IDs are served"),
  score    = list(api = TRUE,  cmd = "R/32_CMP2026a_Score.R",           what = "score new/changed manifestos with the three models (see its header for sub-stages)"),
  merge    = list(api = FALSE, cmd = "R/33_CMP2026a_Merge.R",           what = "merge June + 2026-10 chunk rows into data/CMP/v2026a (refuses to overwrite)"),
  judge    = list(api = TRUE,  cmd = "R/34_Span_Checks_v2026a.R judge", what = "LLM topic judge for the *_overall spans (gpt-4o-mini, capped)"),
  # ---- default chain: no API calls ----------------------------------------------------
  checks   = list(api = FALSE, cmd = "R/34_Span_Checks_v2026a.R checks", what = "span checks (quote, fixed topic regex, judge cache)"),
  checks_public = list(api = FALSE, cmd = "R/34b_Span_Checks_Public.R",  what = "public mode: copy the shipped span-check flags"),
  recheck  = list(api = FALSE, cmd = "R/36_Recheck_Spans.R",             what = "recompute the quote check from text; parse-decoder test; quote-length audit (restricted)"),
  freeze_paper = list(api = FALSE, cmd = "R/48_Freeze_Paper.R",          what = "freeze paper/R0/main_v2.tex lines into a fixed input (once per paper version)"),
  baseline = list(api = FALSE, cmd = "R/40_Baseline.R",                  what = "manifesto_scores_v2026a.parquet"),
  tables   = list(api = FALSE, cmd = "R/41_Paper_Tables.R",              what = "main-text and Appendix A tables and figures"),
  speccurve = list(api = FALSE, cmd = "R/42_Spec_Curve.R",               what = "Appendix C specification curve"),
  speccurve_inference = list(api = FALSE, cmd = "R/56_Spec_Curve_Inference.R", what = "PLLM-0206: joint inference of the specification curve (bootstrap under the null, B = 500, 32 workers, about 42 min) and the model-fork decomposition"),
  nse      = list(api = FALSE, cmd = "R/43_NSE.R",                       what = "NSE inputs for the reporting rule"),
  fetch_external = list(api = TRUE, cmd = "R/44a_Fetch_External.R",      what = "download public CHES / ParlGov CSVs to data/external (network, no keys)"),
  ideology = list(api = FALSE, cmd = "R/44_Ideology_Diagnosis.R",        what = "IDEOLOGY_DIAGNOSIS.md: bridge, IDEO quality, external validity"),
  oa       = list(api = FALSE, cmd = "R/45_OA_Ideology.R",               what = "online-appendix tables OA2-OA5: centrist component, R vs economic liberalism, party FE, CMP categories"),
  ideovar  = list(api = FALSE, cmd = "R/58_Ideology_Variant.R",          what = "PLLM-0245: ideology variant (null component as 0 within model) and its regressions"),
  oa_examples = list(api = FALSE, cmd = "R/47_OA_Examples.R",          what = "OA6 quote lengths + validation by model, OA9 worked examples, OA10 model differences (restricted)"),
  ind1     = list(api = FALSE, cmd = "R/50_Independence_1_CMP.R",        what = "independence Test 1 (hand-coded CMP)",  env = c(IND_BASE = "v2026a")),
  ind2     = list(api = FALSE, cmd = "R/50_Independence_2_Overlap.R",    what = "independence Test 2 (evidence overlap)", env = c(IND_BASE = "v2026a")),
  ind4     = list(api = FALSE, cmd = "R/50_Independence_4_External.R",   what = "independence Test 4 (PopuList)",        env = c(IND_BASE = "v2026a")),
  a13      = list(api = FALSE, cmd = "R/51_A13_Reproducibility.R analyze", what = "A.13 determinism + full-corpus ICC (archived fresh runs)", env = c(A13_BASE = "v2026a")),
  numbers  = list(api = FALSE, cmd = "R/46_Numbers.R",                   what = "numbers.tex: every in-text number as a LaTeX macro, from the chain csv files (+ csv/46_numbers.csv)"),
  compare  = list(api = FALSE, cmd = "R/49_Comparison.R",                what = "COMPARISON_v2026a.md (paper ticket list)"),
  website  = list(api = FALSE, cmd = "R/60_Website_Data.R",              what = "website data files"),
  argentina_score = list(api = TRUE, cmd = "R/52_Argentina_Score.R",     what = "PLLM-0185: score the Argentine 2021-2025 CNE platforms out of sample (frozen pipeline; see its header for sub-stages)"),
  argentina = list(api = FALSE, cmd = "R/53_Argentina_Baseline.R",       what = "PLLM-0185: out-of-sample scores of the Argentine platforms, group means, predicted liberalism, LLA table"),
  argentina_fig = list(api = FALSE, cmd = "R/54_Argentina_Figures.R",    what = "PLLM-0185: Milei-vs-averages, scatter, predicted and trajectory figures"),
  manifest = list(api = FALSE, cmd = "R/70_Manifest.R",                  what = "output/v2026a/MANIFEST.md5 + package versions"),
  merge_rev2 = list(api = FALSE, cmd = "R/35_Merge_ParseLoss_v2026a.R",  what = "v2026a rev2: parse-loss re-scores into raw_claude_rev2 (refuses to overwrite)"),
  public_export = list(api = FALSE, cmd = "R/72_Public_Export.R",        what = "redacted public inputs data/CMP/v2026a_public"),
  fetch_mpds    = list(api = TRUE,  cmd = "R/73_Fetch_MPDS.R",            what = "public package: fetch MPDS2026a with your key, rebuild meta_2026a.parquet")
)
DEFAULT <- c("checks", "recheck", "baseline", "tables", "speccurve", "speccurve_inference", "nse", "ideology", "oa", "ideovar", "oa_examples", "ind1", "ind2", "ind4", "a13",
             "argentina", "argentina_fig", "numbers", "compare", "website", "manifest")

PUBLIC <- nzchar(Sys.getenv("PIPE_PUBLIC"))
PUBLIC_STAGES <- c("checks_public", "baseline", "tables", "speccurve", "speccurve_inference", "nse", "oa", "ideovar", "ind1", "ind4", "numbers", "website")
if (PUBLIC) DEFAULT <- PUBLIC_STAGES
args <- commandArgs(trailingOnly = TRUE)
if ("--list" %in% args) {
  for (s in names(STAGES)) cat(sprintf("%-10s %-5s %-38s %s\n", s, if (STAGES[[s]]$api) "API" else "", STAGES[[s]]$cmd, STAGES[[s]]$what))
  quit(save = "no")
}
run <- if (length(args)) args else DEFAULT
bad <- setdiff(run, names(STAGES)); if (length(bad)) stop("unknown stage(s): ", paste(bad, collapse = ", "))
if (PUBLIC && length(setdiff(run, PUBLIC_STAGES))) stop("not available in public mode: ", paste(setdiff(run, PUBLIC_STAGES), collapse = ", "))
stopifnot("run from the project root" = file.exists("R/1_Aggregation.R"))

log_dir <- file.path(Sys.getenv("PIPE_OUT", if (PUBLIC) "output/v2026a_public" else "output/v2026a"), "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
for (s in run) {
  st <- STAGES[[s]]
  parts <- strsplit(st$cmd, " ")[[1]]
  env <- if (is.null(st$env)) character() else paste0(names(st$env), "=", st$env)
  logf <- file.path(log_dir, sprintf("run_%s.log", s))
  message(sprintf("[run_all] %-9s %s%s", s, st$cmd, if (st$api) "  (API)" else ""))
  t0 <- Sys.time()
  rc <- system2("Rscript", parts, env = env, stdout = logf, stderr = logf)
  message(sprintf("[run_all] %-9s exit %d, %.0f s (log: %s)", s, rc, as.numeric(difftime(Sys.time(), t0, units = "secs")), logf))
  if (rc != 0) stop("stage '", s, "' failed; see ", logf)
}
