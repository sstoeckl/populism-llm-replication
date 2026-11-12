# R/_archive — superseded scripts

Moved here on 2026-10-24 (Phase 2, "one canonical pipeline"). Nothing was deleted; the files
are kept as provenance for what was tried, what produced earlier outputs and why it was
replaced. **No script in the canonical chain (`run_all.R`) sources or runs anything in this
folder**, with one exception: `R/51_A13_Reproducibility.R` parses `load_model_long()` from
`20_Spec_Curve_Compute.R` to reproduce the June-2026 A.13 run exactly (see that script).

Paths inside these scripts still refer to their old locations (`R/<name>.R`); run them, if at
all, from a checkout of an earlier commit.

| Script | What it was | Why superseded |
|---|---|---|
| `1_batch.R` | OpenAI Batch-API helpers (upload JSONL, poll, download) | used only by the gpt-4o-mini v1/v2 runs; no current script uses it |
| `3_Populism.R` | v1/v2 gpt-4o-mini scoring pipeline (Nov 2025 paper tables) | replaced by the v4 "safe" prompt (`R/3c`) and three model families |
| `3_Test.R` | minimal end-to-end API/parsing smoke test | development only |
| `3b_Populism_v3.R` | v3 prompt (offset-free, verbatim-locked evidence) | superseded by v4 safe (`R/3c`) |
| `3d_Populism_v4_calibrated.R` | v4 prompt with explicit score anchors for gpt-4o-mini | rejected; production uses `R/3c` unchanged for all models |
| `4_Stats.R` | aggregation, regressions, figures of the Nov-2025 paper | single-model gpt-4o-mini data; party FE by party **name** (pooled 54 parties); iid SEs. Replaced by `R/40`/`R/41` |
| `5_Checks.R` | 6-tier offset-repair span checker (v1/v2 spans with offsets) | v4 spans carry no offsets; validator is `R/12` |
| `6_Verify_Reproducibility.R` | A.13 determinism test (5 manifestos, OpenAI only) | replaced by `R/51_A13_Reproducibility.R` (20 docs, production models) |
| `7_Span_Validation.R` | full-corpus span validation for the v1/v2 spans | replaced by `R/12` + `R/34` |
| `8_Validate_Spans.R` | deterministic span relocation for v1/v2 | replaced by `R/12` |
| `9_Verify_Baseline.R` | no-API check that the Nov-2025 numbers still hold | baseline replaced; verification is `R/71_Verify.R` against `MANIFEST.md5` |
| `10_Stability_Test_v3.R` | v3 vs v1 prompt stability test | historical prompt development |
| `11_CrossLLM.R` | cross-LLM agreement study on the v3 prompt | historical; cross-model agreement now in `R/52` |
| `13_Test_v4_Safe.R` | 20-manifesto subset test of the v4 safe prompt | development of `R/3c` |
| `14_Score_Full_Corpus_v4.R` | full-corpus v4 scoring with gpt-4o-mini | production OpenAI model is gpt-4.1-mini (`R/16`) |
| `15_CrossLLM_v4_Claude.R` | 500-manifesto Claude robustness subset | superseded by the full-corpus Claude run `R/15b` |
| `17_Score_Full_Corpus_Gemini.R` | full corpus with Gemini 2.5 Flash | replaced by `R/18` (Gemini Flash latest); 2.5 + thinking rejected |
| `20_Spec_Curve_Compute.R` | June-2026 specification curve (8,100 cells, website grid) | populism-confidence weights for all dims, no span validation, leave-worst-out tie bug (l.109-112), 3,240 cells removed by design, only 2,160 distinct beta. Replaced by `R/42` |
| `21_Plot_Spec_Curve.R` | spec-curve figure + Table C.3 numbers from the June grid | replaced by `R/42` |
| `22_Paper_Tables_v4.R` | Oct-2026 recomputation of the paper tables on the June (3,327) baseline; source of `output/paper_v4/` | starting point of `R/41`; reads the June baseline and the old raw files |
| `23_Audit_Pipeline_v4.R` | end-to-end audit of the June pipeline (`output/paper_v4/audit/`) | audit done; its findings are fixed in the v2026a chain |
| `24_Spec_Curve_Recompute_v4.R` | exact re-run of `R/20` with distinct-beta diagnostics | replaced by `R/42` |
| `26_CMP_Update_Baseline.R` | candidate "extended" baseline (June scores + new manifestos, June rule) | replaced by the full v2026a baseline `R/40` |
| `_gemini_cost_forensics.R` | Gemini token-billing forensics | pilot |
| `_gemini_thinking_pilot.R` | Gemini 2.5 Flash + thinking calibration pilot | pilot; thinking rejected (over-attributes populism) |
| `_openai_models_pilot.R` | OpenAI model calibration pilot (50 docs) | pilot; chose gpt-4.1-mini |
| `_openrouter_gemini_test.R` | Gemini via OpenRouter, 5 docs | pilot |
| `_vertex_gemini_test.R` | Gemini via Vertex, 5 docs | pilot; Vertex later adopted for 2026-10 scoring (`R/32`) |
