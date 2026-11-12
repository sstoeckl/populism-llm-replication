# ============================================================
# 35_Merge_ParseLoss_v2026a.R
#
# v2026a revision 2 (2026-10-24): replaces the Claude rows of the 363 parse-loss chunks
# (evidence_status missing without a scoring error; see data/CMP/v2026a/BASELINE_NOTE.md) by
# the re-scores of `CMP_TARGET=parseloss Rscript R/32_CMP2026a_Score.R score claude`
# (claude-sonnet-4-6, patched R/15b flattener, raw tool_use JSON per call).
#
# Inputs : data/CMP/v2026a/raw_claude.parquet, validated_spans_claude.parquet (rev 1)
#          data/CMP/update_2026a/poplib_v4_claude_parseloss_raw.parquet / _validated_spans.parquet
# Outputs (new files, atomic, refuses to overwrite):
#          data/CMP/v2026a/raw_claude_rev2.parquet, validated_spans_claude_rev2.parquet
#          data/CMP/v2026a/archive/dropped_rows_claude_rev2.parquet   (superseded rows, drop_reason)
#          data/CMP/v2026a/archive/dropped_spans_claude_rev2.parquet
#          output/v2026a/logs/35_parseloss_merge.txt
# A chunk is replaced only if its re-score parsed (no scoring error); otherwise the rev-1 row
# stays and is counted.
#
# Re-parse: every re-scored chunk is flattened again OFFLINE from its stored raw tool_use JSON
# with the final R/15b flattener (lenient decoding of stringified dimension objects with
# unescaped quotes; repaired dims listed in parse_repaired). Its spans are validated with the
# deterministic part of R/12 (validate_spans_table: verbatim + not-in-prompt + R/12 topic regex)
# against the chunk text, re-chunked exactly as in production; no judge here. L/R/C spans
# without a stored R/12 judge answer are judged by R/34 (resolved model logged).
# ============================================================

suppressPackageStartupMessages({ library(tidyverse); library(arrow) })
source("R/0_Config.R")
source("R/1_Functions.R"); source("R/2_Helpers.R")
suppressMessages({ source("R/3c_Populism_v4_safe.R"); source("R/12_Validate_Span_Quality.R") })
ex <- parse("R/15b_Claude_Full_Corpus.R", keep.source = FALSE)
for (e in ex) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
                  as.character(e[[2]]) %in% c("V4_DIMS", "FULL_TEXT_TOKEN_CAP", "CHUNK_TOKENS", "OVERLAP_TOKENS",
                                              "decode_dim_string", "flatten_parsed_v4_claude")) eval(e)

UPD <- "data/CMP/update_2026a"
out_raw <- file.path(PIPE_IN, "raw_claude_rev2.parquet")
out_sp  <- file.path(PIPE_IN, "validated_spans_claude_rev2.parquet")
stopifnot("refusing to overwrite rev2 files" = !file.exists(out_raw) && !file.exists(out_sp))

old    <- read_parquet(file.path(PIPE_IN, "raw_claude.parquet"))
old_sp <- read_parquet(file.path(PIPE_IN, "validated_spans_claude.parquet"))
new    <- read_parquet(file.path(UPD, "poplib_v4_claude_parseloss_raw.parquet"))
st <- grep("_status$", names(old), value = TRUE)
lost_ids <- old$doc_id[rowSums(is.na(old[st])) > 0]

if (!"scoring_error" %in% names(new)) new$scoring_error <- NA_character_
ok_new <- new |> filter(is.na(scoring_error), !is.na(schema_version), doc_id %in% lost_ids, !is.na(raw_tool_json))
stopifnot(!anyDuplicated(ok_new$doc_id))
repl <- ok_new$doc_id

# ---- offline re-parse from the stored raw JSON ----
txt26 <- read_parquet(file.path(UPD, "corpus_texts_20260904101355.parquet"), col_select = c("doc_id", "text_raw")) |>
  filter(doc_id %in% ok_new$doc_id_root) |> distinct(doc_id, .keep_all = TRUE) |>
  transmute(doc_id, text = map_chr(text_raw, clean_manifesto_text))
n_old <- old |> count(doc_id_root, name = "n_chunks")
chunk_txt <- map_dfr(seq_len(nrow(txt26)), function(i) {
  t <- txt26$text[i]
  chunks <- suppressWarnings(if (estimate_tokens(t) <= FULL_TEXT_TOKEN_CAP) list(t) else as.list(chunk_text(t, CHUNK_TOKENS, OVERLAP_TOKENS)))
  stopifnot("re-chunking must reproduce the merged chunk count" =
              length(chunks) == n_old$n_chunks[n_old$doc_id_root == txt26$doc_id[i]])
  tibble(doc_id = paste0(txt26$doc_id[i], "_ch", seq_along(chunks)), chunk_text = unlist(chunks))
})
reflat <- map(seq_len(nrow(ok_new)), function(i) {
  ct <- chunk_txt$chunk_text[match(ok_new$doc_id[i], chunk_txt$doc_id)]
  flatten_parsed_v4_claude(jsonlite::fromJSON(ok_new$raw_tool_json[i], simplifyVector = FALSE),
                           ok_new$doc_id[i], ok_new$doc_id_root[i], ok_new$chunk_id[i], ct)
})
flat_rows <- bind_rows(map(reflat, "row"))
keep_prov <- ok_new |> select(doc_id, any_of(c("resolved_model", "model_id", "raw_tool_json")))
ok_new <- flat_rows |> left_join(keep_prov, by = "doc_id") |> mutate(scoring_error = NA_character_)
spans <- bind_rows(map(reflat, "spans"))
# hallucination check against the prompt actually sent (country and year of the manifesto;
# 2026-10-26, chain-review #6; R/36 verifies that this equals the stored flags on every span)
meta35 <- read_parquet("data/CMP/full_data_2026a_meta.parquet", col_select = c("doc_id", "countryname", "date", "has_text")) |>
  filter(has_text) |> distinct(doc_id, .keep_all = TRUE)
new_sp <- spans |> group_split(doc_id_root) |> map_dfr(function(sp) {
  m <- meta35[match(sp$doc_id_root[1], meta35$doc_id), ]
  validate_spans_table(sp |> select(doc_id, doc_id_root, dim_name, quote, context),
                       setNames(sp$chunk_text, sp$doc_id),
                       system_prompt_v4_safe(m$countryname, as.integer(format(as.Date(m$date), "%Y"))), TOPIC_KEYWORDS_V4)
}) |> mutate(judge_used = FALSE, judge_topic = NA)

prov <- old |> filter(doc_id %in% repl) |> select(doc_id, corpus_version, orig_doc_id)
usage <- list.files(file.path(UPD, "usage_logs"), "^usage_v2_", full.names = TRUE) |>
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c"))) |>
  filter(provider == "anthropic", doc_id %in% repl) |> arrange(ts) |>
  group_by(doc_id) |> slice_tail(n = 1) |> ungroup() |> select(doc_id, scored_at = ts)
ok_new <- ok_new |>
  select(-any_of(c("model_id", "scored_at", "corpus_version", "orig_doc_id"))) |>
  left_join(prov, by = "doc_id") |> left_join(usage, by = "doc_id") |>
  mutate(source_run = "parseloss_2026-10", scoring_target = "parseloss", endpoint = "anthropic-api",
         model_requested = "claude-sonnet-4-6", model_resolved = resolved_model) |>
  select(-resolved_model)
stopifnot("every re-score must log claude-sonnet-4-6" = all(ok_new$model_resolved %in% "claude-sonnet-4-6"))

rev2 <- bind_rows(old |> filter(!doc_id %in% repl), ok_new) |> arrange(doc_id_root, chunk_id)
stopifnot(nrow(rev2) == nrow(old), !anyDuplicated(rev2$doc_id), setequal(rev2$doc_id, old$doc_id))
sp2 <- bind_rows(old_sp |> filter(!doc_id %in% repl),
                 new_sp |> filter(doc_id %in% repl) |> mutate(span_source = "parseloss_2026a") |>
                   select(any_of(names(old_sp)))) |>
  arrange(doc_id, dim_name)
stopifnot(!anyDuplicated(sp2[c("doc_id", "dim_name")]))

reason <- "parse_loss_superseded_by_rescore_2026-10-24"
write_atomic(old |> filter(doc_id %in% repl) |> mutate(drop_reason = reason),
             file.path(PIPE_IN, "archive", "dropped_rows_claude_rev2.parquet"))
write_atomic(old_sp |> filter(doc_id %in% repl) |> mutate(drop_reason = reason),
             file.path(PIPE_IN, "archive", "dropped_spans_claude_rev2.parquet"))
write_atomic(rev2, out_raw)
write_atomic(sp2, out_sp)

na_new <- rowSums(is.na(ok_new[st]))
log <- c(sprintf("R/35 parse-loss merge: %d lost chunks, %d re-scored and parsed, %d kept from rev 1 (re-score failed)",
                 length(lost_ids), length(repl), length(setdiff(lost_ids, repl))),
         sprintf("rev-1 chunk-dims with missing status: %d; in the re-scored rows: %d (chunks with any: %d)",
                 sum(is.na(old[old$doc_id %in% lost_ids, st])), sum(na_new), sum(na_new > 0)),
         sprintf("rev-2 file: %d rows, chunk-dims with missing status: %d", nrow(rev2), sum(is.na(rev2[st]))),
         sprintf("raw tool_use JSON stored for %d of %d re-scored chunks",
                 sum(!is.na(ok_new$raw_tool_json)), nrow(ok_new)),
         sprintf("dims decoded from stringified JSON (parse_repaired): %d in %d chunks",
                 sum(lengths(strsplit(na.omit(ok_new$parse_repaired), ","))), sum(!is.na(ok_new$parse_repaired))),
         sprintf("new spans: %d (R/12 deterministic validation; quote check passed: %.1f%%)",
                 nrow(new_sp), 100 * mean(new_sp$verbatim_found & !new_sp$hallucination_flag)))
writeLines(log, file.path("output/v2026a/logs", "35_parseloss_merge.txt")); cat(log, sep = "\n")
