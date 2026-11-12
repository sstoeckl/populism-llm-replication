# R/ — the canonical v2026a script chain

Phase 2 (2026-10-24). One numbered chain produces every table, figure and website number from
the archived per-chunk scores. Run it with `Rscript run_all.R` from the project root
(`Rscript run_all.R --list` shows the stages). `Rscript R/71_Verify.R` reruns the chain into a
scratch directory and checks that every output is byte-identical to `output/v2026a/MANIFEST.md5`.

## The chain

| # | Script | Stage | API | Reads → writes |
|---|---|---|---|---|
| 0 | `0_Config.R` | library | – | paths (`PIPE_IN`, `PIPE_DERIVED`, `PIPE_OUT`), atomic writers, deterministic CSV / PDF |
| 1 | `1_Functions.R` | library | – | chunking, OpenAI wrapper, span helpers (unchanged) |
| 1 | `1_Aggregation.R` | library | – | **the only aggregation code path**: chunk × dim table, span-validation rules, own-block confidence weights, between-model combination (leave-worst-out tie fixed), IDEO |
| 2 | `2_Helpers.R` | library | – | reconstructed corpus-prep helpers (provenance in the file header) |
| 2 | `2_Manifesto_Prep.R` | prep (2025-1) | Manifesto | historical: built `data/CMP/full_data.parquet` (Nov 2025) |
| 3 | `3c_Populism_v4_safe.R` | library | – | production prompt, `poplib_v4` schema, `TOPIC_KEYWORDS_V4` (unchanged; see the regex note below) |
| 12 | `12_Validate_Span_Quality.R` | library | (judge) | R/12 validator: verbatim, anti-prompt, topic + judge |
| 15b, 16, 18, 19 | `15b_Claude_Full_Corpus.R`, `16_Score_Full_Corpus_gpt41mini.R`, `18_Score_Full_Corpus_Gemini_flash_latest.R`, `19_Materialize_Gemini_Partial.R` | scoring (June 2026) | yes | production scorers; R/32 evaluates their function definitions |
| 30 | `30_CMP2026a_Prep.R` | `prep` | Manifesto | MPDS2026a + corpus 2026-1, update classification |
| 31 | `31_CMP2026a_Model_Probe.R` | `probe` | yes | which model each production ID resolves to |
| 32 | `32_CMP2026a_Score.R` | `score` | yes | 2026-10 scoring (update, norm, retry, Vertex anchor) |
| 33 | `33_CMP2026a_Merge.R` | `merge` | – | `data/CMP/v2026a/raw_*`, `validated_spans_*`, `coverage`, `gap_chunks`, `archive/` (refuses to overwrite) |
| 34 | `34_Span_Checks_v2026a.R` | `judge` (API), `checks` | judge only | span checks for all 13 dims (quote check; repaired topic check = fixed regex + judge); judge cache `judge_topic_v2026a.parquet` |
| 34b | `34b_Span_Checks_Public.R` | `checks_public` | – | public mode: copies the shipped span-check flags |
| 35 | `35_Merge_ParseLoss_v2026a.R` | `merge_rev2` | – | v2026a rev2: the 363 parse-loss Claude chunks replaced by their re-scores (`R/32` target `parseloss`, patched R/15b flattener) → `raw_claude_rev2`, `validated_spans_claude_rev2`, archive with `drop_reason` |
| 36 | `36_Recheck_Spans.R` | `recheck` | – (restricted) | recomputes the quote check from the manifesto text with the prompt actually sent and compares it with the stored flags; tests the lenient Claude parser against jsonlite; quote-length audit |
| 40 | `40_Baseline.R` | `baseline` | – | `data/CMP/v2026a/manifesto_scores_v2026a.parquet` (+ csv), old-vs-new decomposition |
| 41 | `41_Paper_Tables.R` | `tables` | – | every data-driven main-text / Appendix A table and figure (label map in its header); since 2026-10-27 also `figures/june_axes/`, the paper figures on the axis limits of the June 2026 figures (section 12), and `figures/paper_v2026a/` with the three combined v2026a paper figures `Trends_by_Ideology.pdf`, `Marginal_Effects_Panels.pdf`, `Marginal_Effects_Dots.pdf` (section 13; the marginal-effect figures use the Tables 4–8 models; csv `41_fig_trends_by_ideology.csv`, `41_fig_me_points.csv`); since 2026-11-11 also `Predicted_Liberalism_by_Ideology.pdf` (csv `41_fig_predicted_liberalism.csv`, PLLM-0242), `tab_pop_subdims.tex` (`tab:pop_subdims`, PLLM-0250), three aggregation forks in `tab:robust_measurement` (+ `41_chunk_subset_share.csv`, PLLM-0247), `41_fin_missingness.csv` and country-FE trend columns (PLLM-0249) |
| 42 | `42_Spec_Curve.R` | `speccurve` | – | Appendix C grid, distinct specs, two estimands, measurement forks |
| 43 | `43_NSE.R` | `nse` | – | NSE inputs for the Phase-5 reporting rule |
| 44 | `44_Ideology_Diagnosis.R` | `ideology` | – | `output/v2026a/IDEOLOGY_DIAGNOSIS.md`: bridge from the printed tables, IDEO quality, external validity; since 2026-10-27 also the paper's Table 3 `tables/tab_external_validity.tex` (label `tab:external`, D7; csv `44_external_validity_table.csv`) with CMP, CHES, ParlGov, POPPA 2018, Global Party Survey 2019, RFPOPI (Celico et al. 2024) and PopuList; matching register `44_external_matches.csv` / `tab_external_matches.tex` (`tab:oa1b`); OA1 by model family `tab_external_validity_by_model.tex` (`tab:oa1a`; csv `44_external_validity_by_model.csv`, POP and IDEO); OA2 interaction across five ideology measures `oa_interaction_robustness.tex` (`tab:oa2f`; csv `44_interaction_robustness[_long].csv`); ideology level effects with CHES lrgen / lrecon / galtan, ParlGov and rile `oa_ideo_levels_external.tex` (`tab:oa2g`; csv `44_ideo_levels_external.csv`; PLLM-0250) |
| 45 | `45_OA_Ideology.R` | `oa` | – (quote-class table restricted) | online-appendix tables OA2–OA5 (`tables/oa_*.tex`, labels `tab:oa2a` … `tab:oa5a`, csv `45_*`): centrist component and alternative IDEO constructions, ideological extremity \|IDEO\| (`oa_abs_ideo.tex`, `tab:oa2e`), model provenance (`oa_model_provenance.tex`, `tab:oa7a`), spec-curve forks (`oa_speccurve_forks.tex`, `tab:oa8a`), R vs economic liberalism (incl. CMP categories, evidence-quote keyword classes, top-R parties), within/between-party decomposition and party FE, errors-in-variables check `oa_eiv.tex` (`tab:oa4b`; csv `45_eiv.csv`; PLLM-0248). In public mode the quote-class table is skipped |
| 47 | `47_OA_Examples.R` | `oa_examples` | – (restricted; full mode only, all outputs not public) | online appendix OA6 (`oa_quote_lengths.tex` `tab:oa6a`, `oa_validation_by_model.tex` `tab:oa6b`), OA9 worked examples (`oa9_examples_{populism,ideology,liberalism}.tex`, `tab:oa9a`–`c`) and OA10 model differences (`oa10_model_diff.tex`, `tab:oa10a`); csv `47_*`. Full quotes and contexts, Manifesto Project citation; selection rules in the header |
| 46 | `46_Numbers.R` | `numbers` | – (restricted: reads outputs of `ideology`, `a13`, `oa`) | `numbers.tex` (every in-text number as `\providecommand{\numXxx}{…}`) and the register `csv/46_numbers.csv` (macro, value, formatted, source file and field, `public`); `numbers_public.tex` / `46_numbers_public.csv` with placeholders for macros from restricted outputs (also written in public mode); reads only chain csv files, plus the archived Test 3 bootstrap (`output/independence/tables/t3_slope_bootstrap.csv`, a manifest input) for the halo macros |
| 44a | `44a_Fetch_External.R` | `fetch_external` | network, no key | `data/external/` (CHES, ParlGov, POPPA 2018, Global Party Survey 2019, RFPOPI) + `SOURCES.csv` with URL, DOI or commit, retrieval time, md5 and licence notes |
| 48 | `48_Freeze_Paper.R` | `freeze_paper` | – | freezes `paper/R0/main_v2.tex` into `data/CMP/v2026a/paper_main_v2_<md5>.csv` (fixed input of R/44, R/49; once per paper version) |
| 49 | `49_Comparison.R` | `compare` | – | `output/v2026a/COMPARISON_v2026a.md` (paper ticket list) |
| 50 | `50_Independence_0_Setup.R` … `50_Independence_4_External.R` | `ind1`, `ind2`, `ind4` | – (Test 3: yes) | independence tests; `IND_BASE=v2026a` mode for the chain; Test 3 (`3_Prompts`, `3a`, `3b`, `3c`) stays as archived, its two paper tables (`t3_slopes.tex`, `t3_slope_bootstrap.tex`) are written by `ind1` from the archived csv files. All tables paper-ready (labels `tab:indep:*`) via `write_tab()` in `0_Setup` |
| 51 | `51_A13_Reproducibility.R` | `a13` | – in `A13_BASE=v2026a` mode | A.13 determinism (archived fresh runs) + full-corpus cross-family ICC |
| 52 | `52_Argentina_Score.R` | `argentina_score` | yes | PLLM-0185 (2026-10-28): out-of-sample scoring of the 38 Argentine 2021–2025 platforms from the Cámara Nacional Electoral (`data/external/argentina/`, OCR text + register) with the frozen production scorers (R/15b, R/16, R/18 definitions loaded exactly as in R/32, incl. R/32's usage log, guards and model-version trace; Gemini `gemini-3.5-flash` on Vertex, abort on another version). Writes `data/CMP/argentina_oos/poplib_v4_<m>_raw.parquet` + `_validated_spans.parquet`. Never touches `data/CMP/v2026a/` |
| 53 | `53_Argentina_Baseline.R` | `argentina` | – | aggregation of the archived out-of-sample chunk scores with R/1_Aggregation (canonical rule) → `PIPE_DERIVED/argentina_scores_oos.parquet/.csv`; group means of the baseline (world / Latin America / Argentina × all / right / populist / right-wing populist), predicted liberalism for each platform from the Tables 4–8 models (cols 4, 5; Argentina country FE, own year FE), LLA table (`argentina/tables/tab_argentina_lla.tex`, `tab:argentina_lla`); csv `argentina/csv/53_*` |
| 54 | `54_Argentina_Figures.R` | `argentina_fig` | – | `argentina/figures/Milei_vs_Averages.pdf`, `Milei_Scatter.pdf`, `Milei_Predicted.pdf`, `Milei_Trajectory.pdf` |
| 56 | `56_Spec_Curve_Inference.R` | `speccurve_inference` | – (32 workers, about 42 min) | PLLM-0206: joint inference of the specification curve after Simonsohn et al. (2020): bootstrap under the null (B = 500, parties resampled, one draw shared by all specifications) over the distinct specifications of R/42 (reads `spec_curve/datasets_v2026a.parquet` written by R/42); `csv/56_joint_inference.csv`, `56_bootstrap_draws.csv`, `56_model_fork_decomposition.csv`, `tables/tab_speccurve_joint_inference.tex` (`tab:speccurve:joint`), `tab_speccurve_model_decomposition.tex` (`tab:speccurve:models`), `figures/spec_curve_null_distribution.pdf` |
| 57 | `57_Leakage_Test.R` | – (restricted, API; not a chain stage) | yes | PLLM-0208: redaction and translation test on 30 German-language manifestos (sub-commands `sample`, `prepare`, `translate`, `score`, `finalize`, `recognise`, `analyse`); data in `data/CMP/leakage_oos/`, report `output/v2026a/validation/leakage/REPORT.md`. Not reported in the paper (PLLM-0259) |
| 58 | `58_Ideology_Variant.R` | `ideovar` | – | PLLM-0245: ideology variant with a missing right or left component set to zero within model (ratio within model, then averaged; and on the zero-filled means), columns (4), (5), (7) of Tables 4–8 and marginal effects re-estimated; `csv/58_*`, `tables/oa_ideo_variant_regs.tex` (`tab:oa2g`), `oa_ideo_variant_coverage.tex` (`tab:oa2h`) |
| 60 | `60_Website_Data.R` | `website` | – | `output/v2026a/website_data/` (drop-in files for `website/data/`) |
| 70 | `70_Manifest.R` | `manifest` | – | `output/v2026a/MANIFEST.md5`, `env/packages.csv`, `env/sessionInfo.txt` |
| 71 | `71_Verify.R` | – | – | rerun + diff against the manifest (`--public`: from the redacted inputs) |
| 72 | `72_Public_Export.R` | `public_export` | – | redacted public inputs `data/CMP/v2026a_public/` |
| 73 | `73_Fetch_MPDS.R` | `fetch_mpds` (public package) | yes (Manifesto API) | PLLM-0266: fetches the MPDS2026a main dataset with the user's key (byte-identical to the canonical file) and rebuilds `data/CMP/v2026a_public/meta_2026a.parquet` from the shipped `meta_2026a_keys.parquet`; Manifesto Project data is not redistributed |

Default stages (no API calls): `checks recheck baseline tables speccurve nse ideology oa oa_examples ind1 ind2 ind4 a13 numbers compare website manifest`. Public stages: `checks_public baseline tables speccurve nse oa ind1 ind4 numbers website`.

## Renaming map (2026-10-24)

R/22–R/27 had colliding numbers. They were untracked, so they were renamed with `mv`.
Superseded scripts were moved to `_archive/` (see `_archive/README.md`). Nothing was deleted.
The archive moves of tracked files show up in `git status` as deletions plus new untracked
files; nothing has been staged.

| Old | New |
|---|---|
| `23_CMP_Update_Prep.R` | `30_CMP2026a_Prep.R` (inline helpers moved to `2_Helpers.R`) |
| `24_CMP_Update_Model_Probe.R` | `31_CMP2026a_Model_Probe.R` |
| `25_CMP_Update_Score.R` | `32_CMP2026a_Score.R` |
| `27_CMP_Merge_v2026a.R` | `33_CMP2026a_Merge.R` |
| `24_Independence_0_Setup.R` | `50_Independence_0_Setup.R` |
| `24_Independence_1_CMP.R` | `50_Independence_1_CMP.R` |
| `24_Independence_2_Overlap.R` | `50_Independence_2_Overlap.R` |
| `24_Independence_3_Prompts.R` | `50_Independence_3_Prompts.R` |
| `24_Independence_3a_Sample.R` | `50_Independence_3a_Sample.R` |
| `24_Independence_3b_Score.R` | `50_Independence_3b_Score.R` |
| `24_Independence_3c_Compare.R` | `50_Independence_3c_Compare.R` |
| `24_Independence_4_External.R` | `50_Independence_4_External.R` |
| `25_Reproducibility_v4.R` | `51_A13_Reproducibility.R` |
| `22_Paper_Tables_v4.R` | `_archive/22_Paper_Tables_v4.R` (superseded by `41`) |
| `23_Audit_Pipeline_v4.R` | `_archive/23_Audit_Pipeline_v4.R` |
| `24_Spec_Curve_Recompute_v4.R` | `_archive/24_Spec_Curve_Recompute_v4.R` (superseded by `42`) |
| `26_CMP_Update_Baseline.R` | `_archive/26_CMP_Update_Baseline.R` (superseded by `40`) |
| `1_batch.R`, `3_Populism.R`, `3_Test.R`, `3b_*`, `3d_*`, `4_Stats.R`, `5`–`11`, `13`, `14`, `15_CrossLLM_v4_Claude.R`, `17`, `20`, `21`, `_*.R` | `_archive/` |

Fixed references: `source()` paths inside the `50_Independence_*` scripts; R/32 and R/33
header and comment references; R/51 now parses `load_model_long()` from
`_archive/20_Spec_Curve_Compute.R` (only in its default `june` mode, which reproduces the
2026-10-07 run).

## Aggregation: one code path

`1_Aggregation.R` is the only aggregation code of the chain. Two named rules live there:
`AGG_RULES$canonical` (the baseline) and `AGG_RULES$june` (the June 2026 rule, a spec-curve fork
and the old-vs-new comparison). Deliberate exceptions outside the chain are R/51 in its default
`june` mode and R/50_Independence_3c. Both reproduce the 2026-10-07/08 analyses on the June data,
and their headers say so. R/32 finalize no longer writes `*_agg` files.

It replaces three divergent implementations:
- `website/data/_build_baseline.R` and `R/20:24-48`: verified chunks, `populism_confidence`
  weights for every dimension, no span validation;
- the `*_agg.parquet` blocks of R/15b, R/16, R/18 and R/19: `verified|mixed` &
  `quality_verified`, plain mean. These are empty for the `*_overall` dims.

The `*_agg.parquet` files in `data/CMP/` and `data/CMP/update_2026a/` are **deprecated**.
No chain stage reads them. They were left in place (data are never deleted) and the scoring
scripts still write them when re-run. `website/data/_build_baseline.R` and `_build_agg.R`
cannot be edited from this repo (submodule). Phase 4 replaces them by copying
`output/v2026a/website_data/`.

## Known measurement facts the chain encodes

- **Topic regex no-op.** Every pattern in `TOPIC_KEYWORDS_V4` (R/3c) ends in `|`, i.e. an
  empty alternative, so R/12's topic check passed every span of the 7 core dims. Stored R/12
  `quality_verified` therefore equals the quote check on those dims (verified in R/40 on
  118,821 spans). R/34 uses a fixed copy (`KW_FIXED`); R/3c is unchanged.
- **Parse losses.** 703 Claude chunk-dims (363 chunks, 10 of them with all 13 dims) had a
  missing `evidence_status` without a scoring error. Claude returned dimension objects as
  JSON strings, and the June flattener mapped them to NA. They were re-scored on 2026-10-24
  with the patched flattener (R/15b now decodes JSON strings and returns the raw tool_use JSON,
  stored per chunk in `raw_tool_json`) and merged as v2026a rev2 (`R/35`). The chain reads the
  rev2 files through `v26_file()` (`0_Config.R`). Any remaining parse losses are excluded like
  `no_evidence` and reported as `parse_lost`.
- **Span-validation rule** (`BASELINE_VALIDATION`): **`quote`** (default; rule (a'), decided
  2026-10-24). The forks `none`, `r12` (topic on L/R/C and `*_overall`) and `full` (topic on
  all dims) are in the spec curve (R/42) and the appendix robustness table (R/41,
  `tab:robust_measurement`). The topic check is a diagnostic (`40_topic_check_diagnostic.csv`);
  `40_rule_decomposition.csv` records how each step moves N and the means.

## Software environment

`output/v2026a/env/packages.csv` and `sessionInfo.txt` (R/70). `renv.lock` in the project
root was written once with `renv::snapshot(packages = …)` without activating renv. It does not
change the user library or `.Rprofile`.

## Public replication package: definitive file list (2026-10-26)

**Ships (code and docs).**
- Top level: `run_all.R`, `renv.lock`, `README_REPLICATION.md`, `PIPELINE.md`, `CODEBOOK.md`,
  `CHANGELOG_DATA.md`, `BLUEPRINT.md`.
- `R/`: `R/README.md` and the scripts `0_Config`, `1_Functions`, `1_Aggregation`, `2_Helpers`,
  `2_Manifesto_Prep`, `3c_Populism_v4_safe`, `12_Validate_Span_Quality`, `15b`, `16`, `18`, `19`,
  `30`–`35`, `34b`, `40`–`44`, `44a`, `49`, `50_*`, `51`, `60`, `70`–`72`.
- `R/_archive/` (provenance; `3_Populism.R` is quoted by R/44).

**Ships (data, no manifesto text beyond 8 words).**
- `data/CMP/v2026a_public/` (written by `R/72`):
  - raw files (quotes ≤ 8 words; no contexts, justifications, raw JSON or API error texts);
  - `archive/gemini_june_superseded.parquet` (redacted);
  - span-check flags, `coverage.parquet`, `gap_chunks.parquet` (ids and error class only);
  - `judge_topic_v2026a_hashed.parquet`;
  - `meta_2026a.parquet` (MPDS2026a metadata, no text) and `mpds_2026a_full.parquet` (MPDS2026a
    main dataset with per-codes). Check the MPDS licence before publishing these two; otherwise
    ship the download script `R/30` only;
  - `README.md`.
- `data/external/` (CHES, ParlGov, `SOURCES.csv`) and `output/independence/cache/external/`
  (PopuList 4.0, Partyfacts): public datasets, cite them.
- Optional: `website/data/manifesto_scores.parquet` (June 2026 baseline; only the full mode's
  old-vs-new comparison reads it).

**Ships (outputs).**
- `output/v2026a/`: `tables/`, `figures/`, `csv/`, `spec_curve/`, `website_data/`, `env/`,
  `MANIFEST.md5`, `COMPARISON_v2026a.md`, `IDEOLOGY_DIAGNOSIS.md`, and `validation/` without the two
  A.13 files named below.

**Must never ship (manifesto text, long quotes, keys).**
- Full text:
  - `data/CMP/full_data.parquet`, `full_data.RData`, `full_data_update_2026a.parquet`;
  - `data/CMP/update_2026a/corpus_texts_*.parquet`.
- Unredacted model outputs:
  - `data/CMP/v2026a/raw_*.parquet` and `validated_spans_*.parquet`;
  - `data/CMP/v2026a/archive/*` and `data/CMP/v2026a/judge_topic_v2026a.parquet`;
  - all `data/CMP/poplib_v4_*` and `*_agg` files;
  - `data/CMP/20251111-poplib_results.RData`.
- All checkpoints: `data/CMP/*checkpoints*/`, `data/CMP/update_2026a/checkpoints_*`,
  `output/reproducibility_v4/checkpoints/`, `output/independence/cache/t3_raw/`.
- A.13 copies with contexts: `output/v2026a/validation/a13/fresh_raw_chunks.parquet` and
  `fresh_validated_spans.parquet`; `output/reproducibility_v4/fresh_*.parquet`.
- `website/data/raw_*.parquet` (June raw files with contexts; also on the now-private HF dataset).
- `.Renviron`, `~/.gcp/`, any service-account JSON, `logs/`, `data/CMP/update_2026a/usage_logs/`
  (internal).

**Public mode.** `PIPE_PUBLIC=1 Rscript run_all.R` reruns `checks_public`, `baseline`, `tables`,
`speccurve`, `nse`, `ind1`, `ind4` and `website` from the shipped data, identical to the manifest
(`Rscript R/71_Verify.R --public`). The outputs of `checks`, `recheck`, `ideology`, `ind2`, `a13`,
`compare` and the June comparison ship as files; they need restricted inputs.
