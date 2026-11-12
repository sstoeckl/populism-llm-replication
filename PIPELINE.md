# PIPELINE — v2026a (MPDS2026a / corpus 2026-1)

Rewritten 2026-10-26 for the canonical chain. The superseded June-2026 description (single
baseline file of 3,327 manifestos, no span filter) is in git history.

Script-level detail, the old→new renaming map and the archive are in `R/README.md`. Prompts, schema,
formulas and variables are in `CODEBOOK.md`. Run instructions are in `README_REPLICATION.md`.

## Overview

```
Manifesto API ──R/30──▶ corpus 2026-1 text + MPDS2026a metadata            (API, key)
                │
LLM APIs ──R/15b,16,18,19 (June 2026) + R/32 (2026-10)──▶ per-chunk scores  (API, not deterministic)
                │
             R/33 merge ──▶ data/CMP/v2026a/raw_*.parquet, validated_spans_*  (rev 1)
             R/32 parseloss + R/35 ──▶ raw_claude_rev2, validated_spans_claude_rev2   (rev 2)
                │
   ┌────────────┴───────────── archived per-chunk scores = the reproducibility object ──────────┐
   │ R/34 checks ─▶ span_checks_*        (quote check; repaired topic check; judge cache)      │
   │ R/40 baseline ─▶ manifesto_scores_v2026a.parquet                                          │
   │ R/41 tables · R/42 spec curve · R/43 NSE · R/44 ideology diagnosis · R/45 OA tables       │
   │ R/50 independence tests 1, 2, 4 · R/51 A.13 determinism + ICC                             │
   │ R/46 numbers.tex · R/49 COMPARISON · R/60 website data · R/70 MANIFEST.md5                │
   └─────────────────────────────── byte-identical on rerun (R/71_Verify.R) ────────────────────┘
```

## Stages

`Rscript run_all.R --list` prints the same table from the code.

| Stage | Script | API | Inputs | Outputs |
|---|---|---|---|---|
| `prep` | `R/30_CMP2026a_Prep.R` | Manifesto | Manifesto API | `data/CMP/mpds_2026a_full.parquet`, `full_data_2026a_meta.parquet`, `update_2026a/` (corpus text cache, update status, text and metadata changes) |
| `probe` | `R/31_CMP2026a_Model_Probe.R` | yes | — | `update_2026a/model_probe.parquet` |
| `score` | `R/32_CMP2026a_Score.R` (targets `update`, `norm`, `retry`, `parseloss`; mode `anchor`) | yes | corpus text, production definitions parsed from R/15b, R/16, R/18 | `update_2026a/checkpoints_*`, `poplib_v4_<m>[_<target>]_raw/validated_spans.parquet`, `usage_logs/` |
| `merge` | `R/33_CMP2026a_Merge.R` | — | June raw files, 2026-10 re-scores | `data/CMP/v2026a/raw_<m>.parquet`, `validated_spans_<m>.parquet`, `coverage.parquet`, `gap_chunks.parquet`, `archive/` |
| `merge_rev2` | `R/35_Merge_ParseLoss_v2026a.R` | — | rev 1, parse-loss re-scores | `raw_claude_rev2.parquet`, `validated_spans_claude_rev2.parquet`, `archive/dropped_*_claude_rev2.parquet` |
| `judge` | `R/34_Span_Checks_v2026a.R judge` | yes (gpt-4o-mini) | spans | `judge_topic_v2026a.parquet` |
| `fetch_external` | `R/44a_Fetch_External.R` | network, no key | public URLs | `data/external/` + `SOURCES.csv` |
| `public_export` | `R/72_Public_Export.R` | — | v2026a inputs | `data/CMP/v2026a_public/` (without Manifesto Project data; `meta_2026a_keys.parquet` instead) |
| `fetch_mpds` | `R/73_Fetch_MPDS.R` (public package) | Manifesto API | own key | `data/CMP/mpds_2026a_full.parquet` (byte-identical to the manifest) and the rebuilt `data/CMP/v2026a_public/meta_2026a.parquet` |
| **`checks`** | `R/34 … checks` | — | spans, judge cache | `span_checks_{claude,gpt41mini,gemini,gemini_june_superseded}.parquet` |
| **`recheck`** | `R/36_Recheck_Spans.R` (restricted) | — | spans, corpus text, metadata | csv `36_*`: quote check recomputed from the text with the prompt actually sent vs the stored flags; parse-decoder test; quote-length audit |
| `freeze_paper` | `R/48_Freeze_Paper.R` | — | `paper/R0/main_v2.tex` | `data/CMP/v2026a/paper_main_v2_<md5>.csv`, the fixed paper input of R/44 and R/49 (once per paper version) |
| **`baseline`** | `R/40_Baseline.R` | — | raw files, span checks, coverage, metadata | `manifesto_scores_v2026a.parquet/.csv`; csv `40_*` (status by dim, coverage, old-vs-new, rule decomposition, topic diagnostic) |
| **`tables`** | `R/41_Paper_Tables.R` | — | baseline, chunk table | Tables 1–8, A.1, A.10, robustness table, Figures 1–16 (`tables/`, `figures/`, csv `41_*`); a second set of Figures 1–4 and 6–10 on the June 2026 axis limits in `figures/june_axes/` (limits used: `41_fig_june_axes_limits.csv`); the combined v2026a figures in `figures/paper_v2026a/` (trends by ideology; marginal effects as panels and as dots; csv `41_fig_trends_by_ideology.csv`, `41_fig_me_points.csv`) |
| **`speccurve`** | `R/42_Spec_Curve.R` | — | chunk table (all measurement forks) | `spec_curve/specs_grid_v2026a.parquet`, `specs_distinct_v2026a.parquet`, Table C.2/C.3, figures |
| **`speccurve_inference`** | `R/56_Spec_Curve_Inference.R` | — | spec-curve grid (`spec_curve/datasets_v2026a.parquet`), baseline | joint inference under the null (Simonsohn et al. 2020; party cluster bootstrap, B = 500, 32 workers, about 42 min): `tab_speccurve_joint_inference.tex`, `tab_speccurve_model_decomposition.tex`, `figures/spec_curve_null_distribution.pdf`, csv `56_*` |
| **`nse`** | `R/43_NSE.R` | — | spec-curve grid | `tab_nse.tex`, csv `43_*` |
| **`ideology`** | `R/44_Ideology_Diagnosis.R` | — | baseline, Nov-2025 scores, frozen paper lines, MPDS, CHES, ParlGov, PopuList, Partyfacts | `IDEOLOGY_DIAGNOSIS.md`, csv `44_*`, `tab_ideo_external.tex`, `tab_external_validity.tex` (paper Table 3), `tab_external_validity_by_model.tex`, `tab_external_matches.tex`, `oa_interaction_robustness.tex`, `ideo_pop_joint.pdf` |
| **`oa`** | `R/45_OA_Ideology.R` | — | baseline, MPDS per-codes; raw files (full quotes and contexts) for the quote-class table only | online appendix OA2–OA5: `tables/oa_centrist_desc.tex`, `oa_centrist_cor.tex`, `oa_ideo_alt_regs.tex`, `oa_ideo_alt_validity.tex`, `oa_abs_ideo.tex`, `oa_model_provenance.tex`, `oa_speccurve_forks.tex`, `oa_right_cmp_cor.tex`, `oa_right_vs_lib.tex`, `oa_right_regs.tex`, `oa_right_quotes.tex` (restricted), `oa_top_right_parties.tex`, `oa_partyfe.tex`; csv `45_*` |
| **`ideovar`** | `R/58_Ideology_Variant.R` | — | raw files, baseline | ideology variants with a missing R or L component set to zero (V1 within model, V2 on zero-filled means): `oa_ideo_variant_regs.tex`, `oa_ideo_variant_coverage.tex`, csv `58_*` (revision material, not in the paper) |
| **`oa_examples`** | `R/47_OA_Examples.R` (restricted) | — | chunk table, raw files (quotes, contexts, justifications), validated spans, baseline metadata, `csv/36_parse_loss_counts.csv` | `oa_quote_lengths.tex`, `oa_validation_by_model.tex`, `oa9_examples_{populism,ideology,liberalism}.tex`, `oa10_model_diff.tex`; csv `47_*` (all not public) |
| **`ind1`, `ind2`, `ind4`** | `R/50_Independence_*.R` (`IND_BASE=v2026a`) | — | baseline, raw files, corpus text (Test 2), MPDS, PopuList | `validation/independence/` |
| **`a13`** | `R/51_A13_Reproducibility.R analyze` (`A13_BASE=v2026a`) | — | archived A.13 fresh runs, v2026a chunks, baseline | `validation/a13/` |
| **`numbers`** | `R/46_Numbers.R` (full; in public mode only the `_public` files) | — | chain csv files (`csv/36_*` … `45_*`, `validation/a13/csv`, `validation/independence/tables`) and the archived Test 3 bootstrap `output/independence/tables/t3_slope_bootstrap.csv` | `numbers.tex` (LaTeX macros `\numXxx`, `\providecommand`), register `csv/46_numbers.csv`; `numbers_public.tex`, `csv/46_numbers_public.csv` (restricted macros as placeholders) |
| **`compare`** | `R/49_Comparison.R` | — | chain csv, frozen paper lines | `COMPARISON_v2026a.md`, csv `49_*` |
| **`website`** | `R/60_Website_Data.R` | — | baseline, spec curve | `website_data/` (drop-in files for `website/data/`) |
| `argentina_score` | `R/52_Argentina_Score.R` (`score`/`finalize <model>`, `estimate`) | yes | `data/external/argentina/argentina_docs.csv` + `text_clean/` (CNE platform filings 2021–2025, OCR; PLLM-0185) | `data/CMP/argentina_oos/poplib_v4_<m>_raw.parquet`, `_validated_spans.parquet`, `usage_logs/` — the frozen production scorers of R/15b, R/16, R/18 loaded as in R/32; Gemini pinned on Vertex with version check; never merged into the baseline |
| **`argentina`** | `R/53_Argentina_Baseline.R` | — | archived out-of-sample chunk scores, baseline | `PIPE_DERIVED/argentina_scores_oos.parquet/.csv` (38 platforms, canonical rule (a')), `argentina/csv/53_*` (group means world / Latin America / Argentina × all / right / populist / right-wing populist; predicted liberalism from the Tables 4–8 models; LLA table), `argentina/tables/tab_argentina_lla.tex` |
| **`argentina_fig`** | `R/54_Argentina_Figures.R` | — | `argentina/csv/53_*`, baseline | `argentina/figures/Milei_vs_Averages.pdf`, `Milei_Scatter.pdf`, `Milei_Predicted.pdf`, `Milei_Trajectory.pdf` |
| **`manifest`** | `R/70_Manifest.R` | — | everything | `MANIFEST.md5`, `env/`. Run it twice when the package list changes: `env/packages.csv` is both written here and hashed as an input |
| — | `R/57_Leakage_Test.R` (restricted, API; **not a chain stage**) | yes | 30 German-language manifestos, redacted and translated | `output/v2026a/validation/leakage/`, `data/CMP/leakage_oos/`; excluded from the manifest (PLLM-0208, PLLM-0259: not in the paper) |

Bold = default stages; `numbers` runs after `a13` and `argentina` because it reads their csv files. All outputs go to `PIPE_OUT` (default `output/v2026a/`) and derived data to
`PIPE_DERIVED` (default `data/CMP/v2026a/`); both are set in `R/0_Config.R`.

## The one aggregation path

`R/1_Aggregation.R` is the only code in the chain that turns chunk scores into manifesto scores.
It holds two named rules, `canonical` and `june`. Outside the chain, R/51 (`june` mode) and
R/50_Independence_3c deliberately reproduce the 2026-10-07/08 analyses with the June rule. The baseline,
the robustness variants, the specification curve and the website data all use it. The rule:
- verified chunks whose quote passes the quote check;
- the dimension's own confidence block as weight;
- per-model means, then the plain mean across the available models.

Formulas are in `CODEBOOK.md` §7. The three June-2026 implementations are archived or deprecated
(`R/README.md`):
- `website/data/_build_baseline.R`;
- `R/_archive/20_Spec_Curve_Compute.R`;
- the `*_agg.parquet` blocks.

## Provenance columns

- Every raw row carries:
  - `source_run`: which scoring run produced it;
  - `scoring_target`, `endpoint`;
  - `model_requested`, `model_resolved`: logged from the API response for every 2026-10 call;
  - `corpus_version`: the text version scored;
  - `orig_doc_id`: the id under which it was scored;
  - `scored_at`.
- Re-scored Claude rows additionally carry `raw_tool_json` and `parse_repaired`.
- The baseline carries `model_legs`, `source_run_<model>`, `update_status`, `baseline_version`,
  `corpus_version`, `aggregation` and `validation_rule`.

Field definitions are in `CODEBOOK.md` §8.

## Revisions

| Version | Date | What | Manifestos |
|---|---|---|---|
| June baseline | 2026-06 | three models on corpus 2025-1. Verified chunks, populism confidence for every dimension, no span filter. Gemini partial at first, then complete | 3,327 |
| v2026a rev 1 | 2026-10-14 | MPDS2026a / corpus 2026-1. June rows kept where the text is byte-identical; re-scores: 83 new or changed (all models), 131 normalisation-only (Claude, GPT; Gemini on Vertex), failed June chunks retried; error rows archived; 4 documented gap legs | 3,371 |
| v2026a rev 2 | 2026-10-24 | 363 Claude parse-loss chunks re-scored and re-parsed (0 parse losses remain); judge on all `*_overall` and core-dimension spans | 3,371 |
| baseline rule (a') | 2026-10-24 | quote check on all 13 dims, own-block weights; the topic check becomes a fork | — |

Row counts, costs and archived files per revision are in `CHANGELOG_DATA.md`.

## Certification levels (R/71)

- **Numeric/text** (csv, tex, md, json): every number is written with 10 significant digits, so
  these files must be byte-identical in any environment with the pinned package versions.
- **Binary** (pdf, parquet, rds): byte-identical in the same environment only, where R and the
  package versions match `output/v2026a/env/packages.csv`. In another environment R/71 compares
  parquet contents instead.
- R/71 reports the environment check first, separately from any numeric difference.
- **What R/71 certifies.** R/71 certifies everything from the archived per-chunk scores and span
  flags onward. The span flags themselves are re-derived from the manifesto text by the
  restricted stage `recheck` (R/36), which makes the default restricted run self-checking.

## Determinism measures (what makes the rerun byte-identical)

- PDFs: `save_pdf()` fixes the cairo `/CreationDate`.
- CSVs: `write_csv_det()` writes numbers with 10 significant digits.
- `setFixest_nthreads(1)`.
- Explicit sorting before every write.
- Fixed seeds for repelled labels.
- Atomic writes (temp file + rename).
- No timestamps inside outputs. Logs are excluded from the manifest.
