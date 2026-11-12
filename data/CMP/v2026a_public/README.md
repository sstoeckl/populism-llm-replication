# v2026a public inputs (written by R/72_Public_Export.R)

Redacted copies of the canonical inputs in data/CMP/v2026a/. Quotes are truncated to their
first 8 words; contexts, justifications and raw model JSON are removed. Manifesto text is
© Manifesto Project (WZB) and is not redistributed. Obtain it from manifesto-project.wzb.eu.

Manifesto Project data (MPDS2026a) is not redistributed either: `Rscript run_all.R fetch_mpds` (R/73,
your own API key) fetches it and rebuilds meta_2026a.parquet from meta_2026a_keys.parquet.

Run: `Rscript run_all.R fetch_mpds fetch_external`, then `PIPE_PUBLIC=1 Rscript run_all.R` (stages
checks_public, baseline, tables, speccurve, speccurve_inference, nse, oa, ideovar, ind1, ind4, numbers,
website). Stages that need the full texts or quotes and are not re-run publicly:
`checks` (topic regex on full quote + context; its flags are shipped as span_checks_*.parquet),
`ind2` (quote positions in the manifesto text), `a13` (fresh-run checkpoints with chunk text),
`compare` (internal paper ticket list).
