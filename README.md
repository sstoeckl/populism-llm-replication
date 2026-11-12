# Populism-LLM — replication package (v2026a)

Replication package for **Rode, M. & Stöckl, S. (2026). _The Milei question, or: How liberal are
right-wing populist parties? A measurement approach using large language models._** (working paper).

Three model families (Claude Sonnet 4.6, GPT-4.1-mini, Gemini 3.5 Flash) score the 3,371 manifestos
of the Manifesto Project corpus 2026-1 (MPDS2026a; 995 parties, 62 countries, 1945–2025) on populism
(anti-elitism, people-centrism, Manichaean, overall), ideology (left, right, centrist) and liberalism
(political, social, economic, financial-market, overall), each score backed by a verbatim quote.
The paper's regressions, the 20,592-specification curve with non-standard errors, the
construct-independence tests and every table, figure and number come out of **one script chain**
(`run_all.R`) that is verified byte for byte against `output/v2026a/MANIFEST.md5` (`R/71_Verify.R`).

- Inspection website: <https://populism-llm.org>
- Public inputs (scores with 8-word quotes) also on Hugging Face: `sstoeckl/populism-llm-replication`
- Full guide: [`README_REPLICATION.md`](README_REPLICATION.md); stages: [`PIPELINE.md`](PIPELINE.md);
  prompt, schema, formulas, variables: [`CODEBOOK.md`](CODEBOOK.md); script map: [`R/README.md`](R/README.md);
  data history: [`CHANGELOG_DATA.md`](CHANGELOG_DATA.md)

## What is in the package

| Part | Where | Notes |
|---|---|---|
| Code | `R/`, `run_all.R`, `renv.lock` | R 4.4.1; package versions pinned in `renv.lock` and listed in `output/v2026a/env/packages.csv` |
| Public inputs | `data/CMP/v2026a_public/` | per-chunk scores, statuses, confidences, provenance of the three models; quotes truncated to their first 8 words; contexts, justifications and raw model JSON removed; span-check flags; coverage; judge cache with hashed quotes; our identifiers and text flags (`meta_2026a_keys.parquet`) |
| Archived inputs of the independence tests | `output/independence/` | PopuList 4.0 and Partyfacts cache (CC BY), the separate-call Test 3 results |
| Reference outputs | `output/v2026a/` | every table, figure and csv of the paper and its appendices, `MANIFEST.md5`, `env/` |
| External data register | `data/external/SOURCES.csv` | URL, version, retrieval time and md5 of CHES, ParlGov, POPPA, GPS and RFPOPI; the files are fetched, not redistributed |

**Not in the package** (licence or size): the manifesto texts and the Manifesto Project dataset
(fetch them with your own free key, see below), the model outputs with full quotes and contexts, the
Argentina 2021–2025 excursion of Section 5 (CNE platform pdfs, OCR text and out-of-sample scores; its
tables and figures are shipped as outputs), the paper source and the files derived from it.

## Quick start

```bash
# 1. packages (renv is not activated; the lockfile is the record)
Rscript -e 'install.packages("renv"); renv::restore()'
# 2. your Manifesto Project key (free academic registration at manifesto-project.wzb.eu)
cp .Renviron.example .Renviron     # fill in MANIFESTO_KEY
# 3. fetch what is not redistributed: MPDS2026a (byte-identical to the manifest) and the external party data
Rscript run_all.R fetch_mpds fetch_external
# 4. run the public chain and verify it against the manifest
PIPE_PUBLIC=1 Rscript run_all.R
Rscript R/71_Verify.R --public
```

`R/71_Verify.R --public` reruns the public stages into a scratch directory and reports, for every
file in the manifest, `identical`, `different` or `not produced in public mode`. The stages that
need the full texts or quotes (`recheck`, `ideology`, `ind2`, `a13`, `argentina`, `compare`, the
quote-class table and worked examples) cannot be rerun publicly; their outputs ship as files. The
expected result is `RESULT: BYTE-IDENTICAL` in an environment with the pinned package versions and
`NUMERIC/TEXT level: identical` elsewhere (binary files such as pdf and parquet may then differ in
bytes but not in content).

The full mode (`Rscript run_all.R`, about 90 minutes on a 128-core server) additionally needs the
restricted inputs listed in `MANIFEST.md5`. The LLM scoring itself (`R/15b`, `R/16`, `R/18`, `R/32`,
`R/52`) is rerunnable with API keys but not deterministic; the archived per-chunk scores are the
starting point of everything reproduced here.

## Licence and citation

Code: MIT (see `LICENSE`). Our scores, flags and derived tables: CC BY 4.0. The 8-word quotes are
excerpts of manifestos provided by the Manifesto Project (WZB) and quoted for identification;
the texts themselves are available from the Manifesto Project under its terms of use. Third-party
party data (CHES, ParlGov, POPPA, GPS, RFPOPI, PopuList, Partyfacts) carry their own licences,
recorded with the sources in `data/external/SOURCES.csv` and `R/44a_Fetch_External.R`.

Please cite the paper and this package (`CITATION.cff`).
