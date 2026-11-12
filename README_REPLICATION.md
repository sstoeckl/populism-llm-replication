# Replication guide — Rode & Stöckl, "The Milei question" (v2026a)

This repository reproduces every table, figure and number of the paper, of its appendices and of
the inspector website. Everything runs from one script chain, `run_all.R`. The full script map is
in `R/README.md`; the stages are described in `PIPELINE.md`; prompts, schema, formulas and
variables are in `CODEBOOK.md`.

## What is reproduced exactly, and what is not

| Part | Reproducible? | How to check |
|---|---|---|
| Everything from the archived per-chunk LLM scores onward: span checks, the manifesto-level baseline, all tables, figures and csv files, the specification curve, the NSE inputs, the validation modules, the website data | **Identical at two levels** (below), checked by `R/71_Verify.R` against `output/v2026a/MANIFEST.md5` | `Rscript R/71_Verify.R` reruns the chain into a temporary directory and diffs every output. Exit status 0 = pass |
| The span flags the aggregation filters on (verbatim in the source, not copied from the prompt) | **Re-derived** from the manifesto text in the restricted stage `recheck` (R/36), with the prompt actually sent; equal to the stored flags on every span | `output/v2026a/csv/36_quote_check_recomputed.csv` |
| LLM scoring of the manifestos (`R/15b`, `R/16`, `R/18`, `R/32`) | **Rerunnable, not deterministic.** No model is bit-deterministic at temperature 0 (A.13: 86% / 77% / 68% exact chunk-score agreement run-to-run for Sonnet / GPT-4.1-mini / Gemini; `output/v2026a/validation/a13/csv/chunk_level_determinism.csv`) | The archived per-chunk scores (`data/CMP/v2026a/raw_*.parquet`) are the reproducibility object |
| LLM topic judge (`R/34 judge`) | Rerunnable, not deterministic | Its answers are archived in `data/CMP/v2026a/judge_topic_v2026a.parquet`, one row per (dimension, quote), with the resolved model version |
| The model behind the June 2026 Gemini scores | **Inferred, not logged.** The June run used the floating alias `gemini-flash-latest`; an anchor test identifies it as `gemini-3.5-flash` (`data/CMP/v2026a/ANCHOR.md`). All 2026-10 Gemini rows were scored on Vertex AI with the resolved version logged per call | `model_resolved` column of the raw files |
| Corpus download (`R/30`) | Needs a Manifesto Project API key | The cleaning helpers are in `R/2_Helpers.R`. Under them the 2026-1 text reproduces 94.9% of the stored June texts byte for byte; the rest were revised upstream |

## Two certification levels

| Level | Files | Guarantee |
|---|---|---|
| Numeric/text | csv, tex, md, json (every number written with 10 significant digits) | byte-identical in any environment with the R and package versions of `renv.lock` |
| Binary | pdf, parquet, rds | byte-identical in the same environment, i.e. R and all package versions as in `output/v2026a/env/packages.csv`. Elsewhere, R/71 compares parquet files by content |

R/71 first checks the environment and reports "environment differs" separately from "numbers
differ". The paper source enters only as a frozen, hashed copy
(`data/CMP/v2026a/paper_main_v2_<md5>.csv`), so editing the paper never breaks verification.

## One-command run

```bash
# from the project root, after installing the packages (see Requirements)
Rscript run_all.R            # default chain, no API calls
Rscript R/71_Verify.R        # rerun into a scratch directory and diff against MANIFEST.md5
Rscript run_all.R --list     # all stages, incl. the API stages (never run by default)
```

Default stages, in order:
- `checks`, `recheck`, `baseline`, `tables`;
- `speccurve`, `speccurve_inference`, `nse`, `ideology`, `oa`, `ideovar`, `oa_examples`;
- `ind1`, `ind2`, `ind4`, `a13`;
- `argentina`, `argentina_fig`;
- `numbers`, `compare`, `website`, `manifest`.

`R/71_Verify.R` reads this list from `run_all.R` (`DEFAULT`, or `PUBLIC_STAGES` with `--public`),
so a stage added to `run_all.R` is verified automatically. `R/57_Leakage_Test.R` is a restricted
API module, not a stage; its outputs under `output/v2026a/validation/leakage/` are not in the
manifest.

Single stages can be run by name (`Rscript run_all.R baseline tables`). Each stage runs in a fresh R
process and logs to `output/v2026a/logs/run_<stage>.log`.

**Runtime** (128-core Linux server, R 4.4.1): about 90 minutes for the default chain (2026-11-11: 89.8 min in the verify).
- The specification curve takes about 23 minutes (8 parallel workers); its joint inference (`speccurve_inference`, R/56, 32 workers) about 42 minutes.
- `recheck` takes 14 minutes.
- Independence Test 2 takes 4 minutes.
- Every other stage takes under 2 minutes.
- `manifest` must run twice when the package list changes (`env/packages.csv` is written and hashed as an input).
- `R/71_Verify.R` takes the same plus the comparison. It is slower when several run in parallel
  on a loaded machine.

## Requirements

- R 4.4.1 and the packages pinned in `renv.lock` (177 packages incl. dependencies). Every package the
  chain uses is in the lockfile.
  - Restore with `renv::restore()`. renv is not activated in this project, so the lockfile is a
    record, not an active library.
  - The software environment of the reference run is in `output/v2026a/env/sessionInfo.txt` and
    `packages.csv`.
- No API keys, no network. The no-API chain reads only the files listed in the INPUTS section of
  `output/v2026a/MANIFEST.md5`.
- Disk: about 1.3 GB of inputs for the full mode; 41 MB for the public mode.
- Outputs are byte-identical on the reference machine.
  - PDFs are written with a fixed CreationDate.
  - CSV numbers are formatted with 10 significant digits.
  - fixest runs single-threaded.
  - Byte identity across operating systems or BLAS libraries has not been tested. The numbers will
    agree to floating-point precision.

## Full mode vs public mode

The manifesto texts are provided by the Manifesto Project (WZB) under its terms of use, which do not
allow redistributing the provided data without written permission. The model outputs contain
quotations and context passages from those texts, often well beyond the 3–8-word quotes the prompt
asks for. This project's own conservative rule: nothing public carries more than 8 consecutive
words of manifesto text.

| Mode | Inputs | Command | Stages |
|---|---|---|---|
| Full (restricted) | `data/CMP/v2026a/` (raw files with quotes and contexts, R/12 span validation, judge cache, archive), corpus texts, A.13 fresh-run checkpoints, the Nov-2025 scores | `Rscript run_all.R` | all default stages |
| Public | `data/CMP/v2026a_public/`, written by `R/72_Public_Export.R`: scores, statuses, confidences and provenance; quotes truncated to 8 words; contexts, justifications, raw model JSON and API error texts removed; the span-check flags shipped as inputs; our identifiers and text flags (`meta_2026a_keys.parquet`); the judge cache with quotes hashed; plus `data/external/SOURCES.csv` and the PopuList/Partyfacts CSVs. Not redistributed and fetched by the user: the MPDS2026a main dataset and the no-text metadata file (`Rscript run_all.R fetch_mpds`, R/73, own Manifesto key; both byte-identical to the manifest) and the external party data (`fetch_external`, R/44a) | `Rscript run_all.R fetch_mpds fetch_external`, then `PIPE_PUBLIC=1 Rscript run_all.R`; check with `Rscript R/71_Verify.R --public` | `checks_public`, `baseline`, `tables`, `speccurve`, `speccurve_inference`, `nse`, `oa` (without its quote-class table), `ideovar`, `ind1`, `ind4`, `numbers` (only `numbers_public.tex` and `csv/46_numbers_public.csv`), `website` |

In public mode all files it produces are byte-identical to the full mode (see the clean-room test below). The stages below need restricted inputs. Their outputs (92 files) ship as files and cannot be
re-run publicly:
- `checks`: the topic regex runs on the full quote and context; its flags ship as inputs;
- `recheck`: re-derives the quote check from the manifesto text;
- the old-vs-new comparison with the June 2026 baseline in `baseline` (skipped with a message in
  public mode);
- `ideology`: needs the Nov-2025 scores, which contain long quotes;
- `argentina`, `argentina_fig`: CNE platform pdfs, OCR text and out-of-sample chunk scores with quotes (its macros in `numbers.tex` are restricted too);
- `ind2`: quote positions in the manifesto text;
- `a13`: fresh-run checkpoints contain chunk text;
- `compare`: needs the paper source;
- the quote-class table of `oa` (`oa_right_quotes.tex`, three `45_right_*` csv files): needs the
  full evidence quotes and contexts;
- `oa_examples` (R/47: OA6 quote lengths and validation by model, OA9 worked examples, OA10 model
  differences): prints or measures the full quotes and contexts;
- the full `numbers.tex` and `csv/46_numbers.csv`: some macros come from restricted outputs (see the
  `public` column of the register). Public mode writes `numbers_public.tex` and
  `csv/46_numbers_public.csv`, in which those macros read "n/a (restricted)"; these two files are
  byte-identical in both modes.

## Verification and clean-room tests (2026-11-12, v2026a; public package as released)

| Test | Directory contents | Result |
|---|---|---|
| In place | the project | `Rscript R/71_Verify.R`: environment = reference; inputs 338/338 unchanged; chain exit 0 (108 min); **328 of 328** derived and output files byte-identical (247 numeric/text, 81 binary) |
| Clean room, public mode | the public package exactly as pushed (`release/stage_public_package.py`: code, docs, `data/CMP/v2026a_public/` without Manifesto Project files, `data/external/SOURCES.csv`, PopuList/Partyfacts cache, archived Test 3 csv files, reference outputs incl. `env/packages.csv` and `MANIFEST.md5`; 62 MB) plus the files the user fetches with `Rscript run_all.R fetch_mpds fetch_external` (MPDS2026a, rebuilt metadata, five external files: all byte-identical to the manifest) | `Rscript R/71_Verify.R --public`: inputs 65/65; chain exit 0 (85 min); **201 of 201** produced files byte-identical (132 numeric/text, 69 binary), incl. `numbers_public.tex`; 127 files not produced publicly (stages `recheck`, `ideology`, `ind2`, `a13`, `argentina`, `argentina_fig`, `oa_examples`, `compare`, the full `numbers.tex` / `46_numbers.csv`, the quote-class table of `oa` and the June comparison) |

Both ran with `R_PROFILE_USER` and `R_ENVIRON_USER` pointing to an empty file (the clean room with only
`MANIFESTO_KEY` set, `release/cleanroom_public_verify.sh`). Earlier runs (2026-10-28: 265 files, 204
inputs) are superseded.
- No absolute or home paths are used outside the API stages (the Vertex key path in `R/32`).
- All packages the chain uses are in `renv.lock`, which records R 4.4.1, the version of the run.
- Logs: `output/v2026a/logs/71_verify_*.txt`.

## Where to start reading

- Every in-text number as a LaTeX macro: `output/v2026a/numbers.tex`, with its register
  `output/v2026a/csv/46_numbers.csv` (source file and field of each macro). Online-appendix tables:
  `output/v2026a/tables/oa_*.tex`.
- Headline: `output/v2026a/tables/tab_reg_4_liberalism_overall_clparty.tex`.
  - Column (1): β = −0.459, SE 0.018 clustered by party, N = 3,340, 988 parties.
  - SE clustered by country: 0.019.
  - Source: `output/v2026a/csv/41_tab4_8_regressions_long.csv`.
- What changed relative to the circulated paper: `output/v2026a/COMPARISON_v2026a.md` and
  `output/v2026a/IDEOLOGY_DIAGNOSIS.md`.
- Data history: `CHANGELOG_DATA.md`. Lessons for other projects: `BLUEPRINT.md`.
