# ============================================================
# 72_Public_Export.R
#
# Redacted input set for the public replication package: data/CMP/v2026a_public/.
# No manifesto text beyond 8-word quotes (© WZB):
#   raw_<m>[_rev2].parquet         scores, statuses, confidences, provenance; *_quote truncated
#                                  to its first 8 words; *_context, *_justification and
#                                  raw_tool_json dropped
#   archive/gemini_june_superseded.parquet   same redaction (input of the Gemini-source fork)
#   span_checks_<set>.parquet      per-span flags of R/34 (quote_ok, topic_rx, judge, topic_ok;
#                                  no text): computed from the full quotes and contexts, they
#                                  are shipped as inputs because the redacted quotes cannot
#                                  reproduce them
#   coverage.parquet, gap_chunks.parquet   unchanged (no manifesto text)
#   judge_topic_v2026a_hashed.parquet       judge cache with the quote replaced by its md5
#   README.md                      what was redacted and which chain stages need restricted data
#
# With PIPE_PUBLIC=1, R/0_Config.R points PIPE_IN at this folder and run_all.R runs the
# public stages (see run_all.R). R/71_Verify.R --public checks those outputs against
# MANIFEST.md5.
# ============================================================

suppressPackageStartupMessages({ library(tidyverse); library(arrow) })
Sys.setenv(PIPE_PUBLIC = "")              # read from the full inputs
source("R/0_Config.R")
PUB <- "data/CMP/v2026a_public"
dir.create(file.path(PUB, "archive"), recursive = TRUE, showWarnings = FALSE)

first_words <- function(x, n = 8L) {
  w <- stringr::str_split(x, "\\s+")
  out <- vapply(w, function(v) paste(head(v[nzchar(v)], n), collapse = " "), "")
  out[is.na(x)] <- NA_character_
  out
}
redact <- function(r) {
  r |> select(-ends_with("_context"), -ends_with("_justification"), -any_of(c("raw_tool_json", "scoring_error"))) |>
    mutate(across(ends_with("_quote"), first_words))
}

for (stem in AGG_FILE_STEM_PUB <- c("claude", "gpt41mini", "gemini")) {
  src <- v26_file("raw", stem)
  write_atomic(redact(read_parquet(src)), file.path(PUB, basename(src)))
}
write_atomic(redact(read_parquet(file.path(PIPE_IN, "archive", "gemini_june_superseded.parquet"))),
             file.path(PUB, "archive", "gemini_june_superseded.parquet"))
for (f in c(list.files(PIPE_DERIVED, "^span_checks_.*\\.parquet$", full.names = TRUE),
            file.path(PIPE_IN, "coverage.parquet")))
  stopifnot(file.copy(f, file.path(PUB, basename(f)), overwrite = TRUE, copy.date = TRUE))
# gap chunks: ids and the error class only (scoring_error holds raw API error text; chain-review #12)
write_atomic(read_parquet(file.path(PIPE_IN, "gap_chunks.parquet")) |>
               select(any_of(c("model", "doc_id", "doc_id_root", "chunk_id", "gap_class"))),
             file.path(PUB, "gap_chunks.parquet"))
# Manifesto Project data is not redistributed (terms of use; PLLM-0266, 2026-11-11). The package ships only
# our own identifiers and text flags (meta_2026a_keys.parquet); R/73 (stage fetch_mpds) fetches MPDS2026a with
# the user's key and rebuilds meta_2026a.parquet (the R/72 selection of the full-mode metadata) byte for byte.
write_atomic(read_parquet(PIPE_META) |>
               select(doc_id, manifesto_id, party, date, token_count, has_text, update_status, text_change_type),
             file.path(PUB, "meta_2026a_keys.parquet"))
for (f in file.path(PUB, c("meta_2026a.parquet", "mpds_2026a_full.parquet"))) if (file.exists(f)) file.remove(f)
write_atomic(read_parquet(file.path(PIPE_IN, "judge_topic_v2026a.parquet")) |>
               mutate(quote_md5 = vapply(quote, function(q) digest::digest(q, algo = "md5", serialize = FALSE), "")) |>
               select(-quote) |> relocate(dim_name, quote_md5),
             file.path(PUB, "judge_topic_v2026a_hashed.parquet"))

# Audit: no character field longer than 8 words, except our own provenance strings
txt_cols_ok <- c("doc_id", "doc_id_root", "orig_doc_id", "schema_version", "scored_at", "manifesto_id", "partyname",
                 "partyabbrev", "countryname", "update_status", "text_change_type", "parse_repaired",
                 "candidatename", "corpusversion", "datasetversion", "id_perm",
                 "source_run", "scoring_target", "endpoint", "model_requested", "model_resolved", "corpus_version",
                 "gap_class", "gap_reason", "sources", "model", "dim_name", "span_source", "judge_source", "quote_md5",
                 "judged_at", "model_id", "drop_reason")
aud <- map_dfr(list.files(PUB, "\\.parquet$", recursive = TRUE, full.names = TRUE), function(f) {
  x <- read_parquet(f); cols <- setdiff(names(x)[vapply(x, is.character, logical(1))], txt_cols_ok)
  tibble(file = sub(paste0(PUB, "/"), "", f),
         max_words = if (length(cols)) max(0L, vapply(cols, function(c) max(0L, stringr::str_count(x[[c]], "\\S+"), na.rm = TRUE), 0L)) else 0L)
})
print(aud)
stopifnot("a public file carries more than 8 words of text" = all(aud$max_words <= 8L))

writeLines(c("# v2026a public inputs (written by R/72_Public_Export.R)", "",
  "Redacted copies of the canonical inputs in data/CMP/v2026a/. Quotes are truncated to their",
  "first 8 words; contexts, justifications and raw model JSON are removed. Manifesto text is",
  "© Manifesto Project (WZB) and is not redistributed. Obtain it from manifesto-project.wzb.eu.", "",
  "Manifesto Project data (MPDS2026a) is not redistributed either: `Rscript run_all.R fetch_mpds` (R/73,",
  "your own API key) fetches it and rebuilds meta_2026a.parquet from meta_2026a_keys.parquet.", "",
  "Run: `Rscript run_all.R fetch_mpds fetch_external`, then `PIPE_PUBLIC=1 Rscript run_all.R` (stages",
  "checks_public, baseline, tables, speccurve, speccurve_inference, nse, oa, ideovar, ind1, ind4, numbers,",
  "website). Stages that need the full texts or quotes and are not re-run publicly:",
  "`checks` (topic regex on full quote + context; its flags are shipped as span_checks_*.parquet),",
  "`ind2` (quote positions in the manifesto text), `a13` (fresh-run checkpoints with chunk text),",
  "`compare` (internal paper ticket list)."), file.path(PUB, "README.md"))
message("R/72: public inputs in ", PUB)
