# CHANGELOG_DATA — every data revision

Counts come from the files named in each entry; costs come from the run logs named. Costs are list
prices × logged tokens. Nothing is ever overwritten: superseded rows go to an archive file with a
`drop_reason`.

## 2025-11 — Nov-2025 paper data (superseded, archived)

- **What:** gpt-4o-mini, schema v2 (zero-coded, no evidence status); scores behind the circulated
  paper's Tables 4–8.
- **Files:** `data/CMP/20251111-poplib_results.RData` (3,327 manifestos), corpus 2025-1
  (`data/CMP/full_data.parquet`).
- **Status:** archived. Used only by the bridge in `R/44_Ideology_Diagnosis.R`, which reproduces
  the printed tables exactly.
- **Cost:** not recorded.

## 2026-06 — June baseline (three models, corpus 2025-1)

- **What:** v4-safe prompt and schema; Claude Sonnet 4.6 (`R/15b`), GPT-4.1-mini (`R/16`), Gemini
  via the alias `gemini-flash-latest` on AI Studio (`R/18`, first materialised partially by `R/19`).
  Resolved model versions were not logged.
- **Files:** `data/CMP/poplib_v4_{claude_full,gpt41mini,gemini_flash_latest}_raw.parquet` and
  `*_validated_spans.parquet`.

| Model | chunk rows | rows without error | manifestos fully parsed |
|---|---|---|---|
| Claude | 6,561 | 6,523 | 3,311 |
| GPT-4.1-mini | 6,561 | 6,557 | 3,326 |
| Gemini | 7,054 | 6,559 | 3,327 |

- **Known defects** (found 2026-10):
  - the resume logic counted failed chunks as done (fixed 2026-10-13 in R/15b, R/16);
  - 492 Gemini quota / API-error rows plus 3 Gemini parse failures (the 495 Gemini `june_error_row`s);
  - 703 Claude chunk-dims parse-lost (see 2026-10-24);
  - the June baseline `website/data/manifesto_scores.parquet` ignores span validation and weights
    every dimension by populism confidence.
- **Cost:** not recorded in the repo.

## 2026-10-07/08 — CMP update scoring (83 manifestos)

- **Cause:** MPDS2026a / corpus 2026-1 (`20260904101355`).
  - `R/30` classification (`data/CMP/update_2026a/update_status.parquet`):
    - 3,138 unchanged;
    - 44 new;
    - 131 text changed by normalisation only;
    - 38 substantively changed;
    - 17 metadata only;
    - 3 party re-codes (2 with identical text, 1 with changed text).
  - The 83 new, substantively changed or re-coded-with-changed-text manifestos were scored with all
    three models; Gemini on AI Studio, resolved `gemini-3.5-flash`.
- **Files:** `data/CMP/update_2026a/poplib_v4_<m>_raw.parquet`.
- **Cost:** about $37 (`PLAN.md`, "Already spent this cycle").

## 2026-10-13/14 — v2026a revision 1 (merge)

- **What:**
  - 131 normalisation-only manifestos re-scored: Claude and GPT, plus Gemini on Vertex AI `global`,
    `gemini-3.5-flash`, after a Vertex anchor test (`data/CMP/v2026a/ANCHOR.md`);
  - failed June chunks retried: Claude 31 chunks in 16 manifestos, GPT 2 manifestos;
  - Gemini error rows dropped;
  - a manifesto × model leg enters only if complete.
- **Script:** `R/33_CMP2026a_Merge.R`. Notes in `data/CMP/v2026a/RUN_NOTES.md`.

| Model | canonical rows (`raw_<m>.parquet`) | manifestos | archived (`archive/dropped_rows_<m>.parquet`) |
|---|---|---|---|
| Claude | 6,699 | 3,371 | 38 `june_error_row`, 612 `june_superseded_by_2026-1_text` |
| GPT-4.1-mini | 6,695 | 3,370 | 4 `june_error_row`, 618 superseded, 3 `incomplete_leg_documented_gap` |
| Gemini | 6,659 | 3,368 | 495 `june_error_row`, 619 superseded |

- **Gap legs** (`coverage.parquet`, `gap_chunks.parquet`):
  - GPT 113441_202004 (truncated JSON at `max_output_tokens`);
  - Gemini 21112_201405, 21111_200706 (Vertex timeouts);
  - Gemini 89221_201612 (timeout + invalid JSON).
- **Fork archive:** the June AI-Studio Gemini rows of the 131 normalisation-only manifestos are
  kept for the Gemini-source fork in `archive/gemini_june_superseded.parquet` (488 rows, 131
  manifestos).
- **Cost** (`RUN_NOTES.md`): Anthropic $65.0 (incl. about $9 for a batch lost to a truncated
  checkpoint), OpenAI $5.4 (incl. 2,586 judge calls), Google Vertex $9.1.

## 2026-10-24 — v2026a revision 2 (Claude parse-loss re-score)

- **Cause:** 363 Claude chunks (703 chunk-dims) had a missing `evidence_status` without a scoring
  error.
  - Claude had returned dimension objects as JSON strings with unescaped quotation marks; the
    flattener mapped them to NA.
  - No raw responses had been stored.
- **What:**
  - R/15b patched to store the raw tool_use JSON and decode stringified dims;
  - re-score with `CMP_TARGET=parseloss Rscript R/32_CMP2026a_Score.R score claude`;
  - offline re-parse and merge with `R/35_Merge_ParseLoss_v2026a.R`;
  - 450 dims in 237 chunks needed the lenient decoding;
  - result: 0 parse losses.
- **Files:**
  - `raw_claude_rev2.parquet` (6,699 rows, 3,371 manifestos);
  - `validated_spans_claude_rev2.parquet`;
  - `archive/dropped_rows_claude_rev2.parquet` (363 rows) and `archive/dropped_spans_claude_rev2.parquet`;
  - `drop_reason = parse_loss_superseded_by_rescore_2026-10-24`;
  - the rev-1 files are unchanged.
- **Cost:** Anthropic $37.59 (363 calls, all resolved `claude-sonnet-4-6`); R/12 judge in finalize
  $0.03 (`data/CMP/update_2026a/usage_logs/`).

## 2026-10-24 — topic judge (gpt-4o-mini, R/34)

- **What:** 84,586 (dimension, quote) pairs in `data/CMP/v2026a/judge_topic_v2026a.parquet`:
  - the `*_overall` dims;
  - the 7 core dims, for the repaired topic check (diagnostic and spec-curve fork);
  - L/R/C spans without a stored R/12 answer.
- **Model:** every call resolved `gpt-4o-mini-2024-07-18`.
- **Cost:** $3.01 (sum over the cache's logged tokens).

## 2026-10-24 — baseline rule (a')

- **What:** `manifesto_scores_v2026a.parquet` is built with the quote check on all 13 dims and
  own-block confidence weights (Sebastian, 2026-10-24).
- **Effect** (`output/v2026a/csv/40_rule_decomposition.csv`): POP N 3,349, LIB N 3,361, IDEO N 3,193.

## 2026-10-24 — public input set

- **What:** `data/CMP/v2026a_public/` (`R/72_Public_Export.R`): redacted copies, no text field over
  8 words.
- **Not public:** the HF dataset `sstoeckl/populism-llm` was set to private on 2026-10-24. Its raw
  files carry long context passages.

## 2026-10-24 — external data (R/44a)

- **What:** CHES 1999–2024 trend file V2 and the ParlGov development party table, retrieved
  2026-10-24 23:57 UTC; URLs and md5 in `data/external/SOURCES.csv`.
- **Archived earlier:** PopuList 4.0 and the Partyfacts external-parties file, from 2026-10-07 in
  `output/independence/cache/external/`.

## 2026-10-26 — code-review fixes (no data change)

- **What:** the independent review's fixes touched code only. No input file was modified or
  re-scored.
- **Evidence that the archived data stand:**
  - The corrected merge (`R/33`: attempts ordered by the stored checkpoint timestamp, expected
    chunks from all three June files) was re-run into a scratch directory. It reproduces every
    score-bearing rev-1 file exactly (raw GPT and Gemini, all validated spans, coverage, archive).
  - The only differences are `scored_at` of 75 Claude rows (the usage logs now also hold the
    2026-10-24 calls for those documents) and helper columns in `gap_chunks`.
  - The quote check recomputed from the manifesto text with the prompt actually sent equals the
    stored flags on every span (`output/v2026a/csv/36_quote_check_recomputed.csv`).
  - The current Claude parser reproduces all 363 rev-2 rows exactly (`36_parse_decoder_test.csv`).
- **New fixed input:** `data/CMP/v2026a/paper_main_v2_1ca04ab71ce6ca111f1c0af62f51228a.csv`, a
  frozen copy of `paper/R0/main_v2.tex` (`R/48`).
- **Public set:** `data/CMP/v2026a_public/` now ships a no-text metadata file and the MPDS2026a
  main dataset. `gap_chunks` is reduced to ids and error class, and `scoring_error` is removed.

## 2026-10-27 — chain additions for the final paper (no data change; PLLM-0065)

- **What:** new chain stages, code and outputs only. No input file, no baseline value and no
  pre-existing output changed (checked by `R/71_Verify.R` against the new manifest, which differs
  from the 2026-10-26 one only by the new files and the md5 of the edited scripts).
- **New stage `oa`** (`R/45_OA_Ideology.R`): online-appendix tables OA2–OA5 (`tables/oa_*.tex`,
  csv `45_*`). Ported from the 2026-10-27 briefing scripts
  (`hqg/share/2026-10-27_briefing/discussion_ideology/`); every number of their csv files is
  reproduced to within 1e-9. Two extensions: the CMP-category table covers per401–per414 and
  per601–per608 (the briefing had 16 of them), and the regression tables carry SEs.
  The quote-class table reads the restricted raw files and is skipped in public mode.
- **New stage `numbers`** (`R/46_Numbers.R`): `numbers.tex`, every in-text number as a LaTeX
  macro, and its register `csv/46_numbers.csv`; reads only chain csv files.
- **`tables`** (`R/41`): a second set of the paper figures on the June 2026 axis limits in
  `figures/june_axes/` (limits hard-coded from the briefing's `axis_limits.csv`, widened where
  v2026a data fall outside; `41_fig_june_axes_limits.csv`). The existing figures are unchanged.

## 2026-10-27 (later) — combined paper figures (no data change; PLLM-0119…0122)

- **What:** `R/41` section 13 writes three figures approved on 2026-10-27 to
  `output/v2026a/figures/paper_v2026a/`: `Trends_by_Ideology.pdf` (five-year means by Left/Right),
  `Marginal_Effects_Panels.pdf` (model (5) curves, five outcomes) and `Marginal_Effects_Dots.pdf`
  (marginal effect at the p10 / median / p90 of IDEO). Ported from
  `hqg/share/2026-10-27_briefing/fig_proposals/fig_proposals.R` (`csv/41_fig_me_points.csv`).
- **Models (decision 2026-10-27, same day):** the two marginal-effect figures use exactly the
  Tables 4–8 models (`models[[dv]]$m5`, reference line `$m1`, party-clustered SE, each outcome on its
  own complete-case sample). Every p-value, N and reference coefficient therefore equals the tables,
  and R/46 checks this. Only the IDEO evaluation points (p10 / median / p90) come from the analysis
  sample. A first version had refitted the models on the analysis sample, as the draft did, so its
  p-values differed slightly (economic 0.028 vs 0.035). The marginal-effect points therefore differ
  from the draft's `C_me_points.csv` by up to 0.004.
- `numbers.tex` gains 48 macros (`\numMe<Outcome><Left|Centre|Right>[Se]`, `\numInterP<Outcome>`,
  `\numMeN…`, `\numMeRefColOne…`, `\numMeIdeoAt…`). Its existing macros are unchanged.

## 2026-10-27 (evening) — |IDEO| table, Table 3, more macros (no data change; PLLM-0127, PLLM-0014, PLLM-0030, PLLM-0040)

- **`oa`:** new online-appendix table `oa_abs_ideo.tex` (`tab:oa2e`). It reports Tables 4–8,
  columns (4) and (5), with |IDEO| added, and the left/right-extremity asymmetry test
  (csv `45_abs_ideo_regs.csv`, `45_abs_ideo_cor.csv`). It reproduces
  `hqg/share/2026-10-27_briefing/discussion_ideology/abs_ideo/` exactly, at the 3 printed decimals.
- **`ideology`:** the paper's Table 3 as `tab_external_validity.tex` (`tab:external`, decision D7).
  Rows are POP and IDEO; columns are rile, rile logit, CHES lrgen / lrecon / galtan and ParlGov,
  plus the PopuList AUCs. The POP AUC uses the Test 4 sample (0.800, equal to Test 4).
- **`numbers`:** 578 macros. New: |IDEO|, POP external validity, halo (Test 3), R vs CMP
  categories, quote-length audit. The right-quote macros were renamed `\numQuote*` →
  `\numRightQuote*`; no manuscript or proposal used them. The archived Test 3 bootstrap file
  `output/independence/tables/t3_slope_bootstrap.csv` is a new manifest input (R/70).
- **`tables`:** the marginal-effect figures of R/41 section 13 now use the t(G−1) critical value for
  the 95% CI, as the section-8 figures do.

## 2026-10-27 — v2026a rev3 (ideology without the centrist score; PLLM-0132, PLLM-0048)

- **What:** the ideology measure is now IDEO = (R − L) / (R + L) on the cross-model means, NA if
  R + L = 0, and is called "ideology". Decided by Sebastian on 2026-10-27. Until rev2 it was
  (R − L) / (R + L + C).
  - The rev2 construction stays in the baseline as `pop_ideology_direction_c`.
  - The selection forks `_zero`, `_ge2` and `_all3` now refer to R and L.
- **Unchanged:** no input file and no per-chunk score. The populism and liberalism scores, the
  span checks and every result that does not use IDEO are byte-identical. That includes the
  headline (β = −0.459, SE 0.018, N 3,340) and the populism spec curve (20,592 specs, 97.9%
  significantly negative, none significantly positive).
- **Changed:** every output that uses IDEO. 138 of 241 derived and output files; the baseline
  parquet and csv in `data/CMP/v2026a/` were rewritten by `R/40`.
  - IDEO N is 3,209 (was 3,193); r(IDEO, IDEO_c) = 0.989.
  - The interaction distinct-spec count is 18,212 (was 17,488).
- **Superseded files:** as with earlier derived files, the previous baseline is not kept in the
  chain. A snapshot of the rev2 derived files, all rev2 outputs and the code that produced them is
  in `output/_superseded/v2026a_rev2_ideo_rlc_20261027/`; no stage reads it.
- **Labels:** "Populist ideology" → "Ideology" in tables and figures. File and csv column names are
  unchanged. `pop_ideology_overall` keeps its own label.

## 2026-10-27 (night) — external data, paper-ready appendix tables, public numbers (no score change)

- **New external inputs (R/44a, network, no keys):** POPPA 2018 party means (Harvard Dataverse
  doi:10.7910/DVN/8NEL7B v2.0) and the Global Party Survey 2019 party file V2.1 (doi:10.7910/DVN/WMGTNS
  v3.0, DOI from globalpartysurvey.org). Both are recorded in `data/external/SOURCES.csv` with URL,
  DOI / version, retrieval time and md5. The Celico et al. ML index has no static file; R/44 picks up
  `data/external/celico_ml_index.<csv|xlsx>` once the authors' file is added (superseded on 2026-10-28: RFPOPI is fetched from GitHub, see below).
- **Table 3:** `tab_external_validity.tex` is transposed (indicators as rows, r and N for POP and IDEO)
  and now includes POPPA and GPS. The matching register is `44_external_matches.csv` /
  `tab_external_matches.tex`.
- **New manifest inputs:** `output/independence/tables/t3_slopes.csv` (archived Test 3), the two new
  `data/external/` files.
- **Independence tables:** paper-ready (`tab:indep:*`), with readable labels, escaped text and notes;
  the Test 3 tables are rebuilt by `ind1` from the archived csv files.
- **No "June" labels:** fork options of R/42, robustness variants of R/41 and the A.13 tables are
  relabelled descriptively. Csv codes and the spec count (20,592) are unchanged.
- **A.13:** the sample table shows the party name where the abbreviation is empty (34340_201501).
- **numbers:** 674 macros. New groups cover A.10, parse loss (from `36_parse_loss_counts.csv`, written
  by R/36), topic pass rates, recheck counts, Appendix B (overlap, CMP benchmark, PopuList dummies),
  POPPA / GPS and matches. 149 macros come from restricted outputs. Public mode writes
  `numbers_public.tex` / `46_numbers_public.csv`, which are byte-identical in both modes.

## 2026-10-27 (late) — online-appendix batch (no score change; PLLM-0064)

- **New tables:**
  - OA1: `tab_external_validity_by_model.tex` (`tab:oa1a`). `44_external_validity_by_model.csv` now
    covers POP and IDEO and all Table 3 indicators.
  - OA2: `oa_interaction_robustness.tex` (`tab:oa2f`).
  - OA6: `oa_quote_lengths.tex` (`tab:oa6a`) and `oa_validation_by_model.tex` (`tab:oa6b`).
  - OA7: `oa_model_provenance.tex` (`tab:oa7a`).
  - OA8: `oa_speccurve_forks.tex` (`tab:oa8a`).
  - OA9 / OA10: worked examples and model differences, from the new restricted stage `oa_examples`
    (R/47).
- **Label and layout fixes:** `pop_ideology_overall` is printed as "Ideology (overall intensity)" in
  the OA tables. The gap-leg fork option is now "filled from the initial scoring pass".
  `tab_speccurve_perfork.tex` and `tab_nse.tex` get the guarded resizebox. The OA5 note prints a
  single N.
- **numbers:** 738 macros, 213 of them from restricted outputs.

## 2026-10-28 — RFPOPI added to Table 3 (no score change; PLLM-0014)

- **New external input:** RFPOPI (Celico, Rode & Rodríguez Carreño 2024), the static copy
  `data/RFPOPI final dataset.csv` of GitHub `acelico/RFPOPI-Populism-indicators` at commit
  2870ebb94df9 (2024-02-16), md5 2ae7fa0b44a0fb384797e4381d8bc104. Fetched by R/44a as
  `data/external/rfpopi_final_dataset.csv`, with licence and citation in SOURCES.csv.
- **Matching:** Partyfacts id → CMP party; same election year, otherwise nearest within ±1 year.
- **Tables:** RFPOPI replaces the Celico placeholder rows in Table 3, `tab:oa1a` and `tab:oa1b`.
  - POP: r 0.24 with the rhetorical and 0.63 with the ideational index (N 1,852).
  - IDEO: r 0.22 and 0.15 (N 1,769).
  - Matched: 1,860 manifestos of 522 parties.

## Cost summary of this cycle (2026-10)

| Item | USD | Source |
|---|---|---|
| A.13 rerun | 6.31 | `output/reproducibility_v4/REPORT.md` |
| Independence Test 3 | 20.42 | `output/independence/REPORT.md` |
| Update scoring Oct 7/8 | ≈ 37 | `PLAN.md` |
| Probes | ≈ 1 | `PLAN.md` |
| Phase 1 (norm + retry + Vertex anchor) | 79.5 | `data/CMP/v2026a/RUN_NOTES.md` |
| Parse-loss re-score | 37.59 | usage logs |
| Judge (R/34 + R/32 finalize) | 3.04 | judge cache, usage logs |
| **Total** | **≈ 185** | |
