# ============================================================
# 36_Recheck_Spans.R   (restricted stage `recheck`; needs manifesto text and full quotes)
#
# Independent re-computation of what the chain otherwise takes from stored flags
# (chain-review #6 and the review's certification limit):
#   1. Quote check. For every span of the current revision, plus the June Gemini spans of the
#      Gemini-source fork, the deterministic quote check (verbatim in the source chunk, not a
#      substring of the system prompt) is recomputed with R/12's own functions. The chunk text is
#      re-chunked exactly as in production, and the hallucination check uses the prompt actually
#      sent (country and year of the manifesto), not system_prompt_v4_safe("X", 2020L). The
#      result is compared with the stored flags that span_checks_*.parquet carry (quote_ok).
#   2. Parse decoder. On every valid JSON dimension object in the stored raw tool JSON, the
#      lenient field extractor of R/15b (decode_dim_string(lenient_only = TRUE)) is compared with
#      jsonlite; and the stored rev-2 rows are compared with a re-flatten by the current
#      R/15b code.
#   3. Quote-length audit (copyright): words per quote and context, by model.
# Outputs: PIPE_OUT/csv/36_*.csv. No API calls.
# ============================================================

suppressPackageStartupMessages({ library(tidyverse); library(arrow); library(stringi); library(furrr) })
source("R/0_Config.R")
source("R/1_Aggregation.R")
source("R/2_Helpers.R")
suppressMessages(source("R/12_Validate_Span_Quality.R"))     # sources R/1_Functions.R and R/3c
ex <- parse("R/15b_Claude_Full_Corpus.R", keep.source = FALSE)
for (e in ex) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
                  as.character(e[[2]]) %in% c("V4_DIMS", "FULL_TEXT_TOKEN_CAP", "CHUNK_TOKENS", "OVERLAP_TOKENS",
                                              "decode_dim_string", "flatten_parsed_v4_claude")) eval(e)
CSV <- file.path(PIPE_OUT, "csv")

# ---- 1. Quote check, recomputed ----------------------------------------------------------
meta <- read_parquet(PIPE_META) |> filter(has_text) |> distinct(doc_id, .keep_all = TRUE) |>
  transmute(doc_id, countryname, year = as.integer(format(as.Date(date), "%Y")))
txt26 <- read_parquet("data/CMP/update_2026a/corpus_texts_20260904101355.parquet", col_select = c("doc_id", "text_raw")) |>
  filter(!is.na(doc_id)) |> distinct(doc_id, .keep_all = TRUE)
alt <- read_parquet(file.path(PIPE_IN, "archive", "gemini_june_superseded.parquet"), col_select = c("doc_id_root")) |>
  distinct(doc_id_root) |> pull()
june_txt <- read_parquet("data/CMP/full_data.parquet", col_select = c("doc_id", "text")) |>
  filter(!is.na(text), doc_id %in% alt) |> distinct(doc_id, .keep_all = TRUE)
sets <- list(claude = v26_file("validated_spans", "claude"), gpt41mini = v26_file("validated_spans", "gpt41mini"),
             gemini = v26_file("validated_spans", "gemini"))
spans <- imap_dfr(sets, ~ read_parquet(.x, col_select = c("doc_id", "doc_id_root", "dim_name", "quote",
                                                          "verbatim_found", "hallucination_flag")) |> mutate(set = .y))
spans_j <- read_parquet("data/CMP/poplib_v4_gemini_flash_latest_validated_spans.parquet",
                        col_select = c("doc_id", "doc_id_root", "dim_name", "quote", "verbatim_found", "hallucination_flag")) |>
  filter(doc_id_root %in% alt) |> mutate(set = "gemini_june_superseded")
spans <- bind_rows(spans, spans_j)

chunk_one <- function(t) suppressWarnings({
  if (estimate_tokens(t) <= FULL_TEXT_TOKEN_CAP) list(t) else as.list(chunk_text(t, CHUNK_TOKENS, OVERLAP_TOKENS))
})
docs <- spans |> distinct(set, doc_id_root) |> mutate(text_src = if_else(set == "gemini_june_superseded", "june", "2026-1"))
plan(multisession, workers = 8L)
recheck <- future_map_dfr(split(spans, interaction(spans$set, spans$doc_id_root, drop = TRUE, lex.order = TRUE)), function(sp) {
  root <- sp$doc_id_root[1]; june <- sp$set[1] == "gemini_june_superseded"
  t <- if (june) june_txt$text[match(root, june_txt$doc_id)] else clean_manifesto_text(txt26$text_raw[match(root, txt26$doc_id)])
  ch <- chunk_one(t); names(ch) <- paste0(root, "_ch", seq_along(ch))
  m <- meta[match(root, meta$doc_id), ]
  prompt <- system_prompt_v4_safe(m$countryname, m$year)
  sp |> mutate(chunk_found = doc_id %in% names(ch),
               verbatim_new = map2_lgl(quote, doc_id, function(q, cid) isTRUE(verbatim_check(q, ch[[cid]] %||% NA_character_)$found)),
               halluc_new = map_lgl(quote, ~ hallucination_check(.x, prompt)))
}, .options = furrr_options(seed = NULL, packages = c("dplyr", "purrr", "stringi", "rtiktoken"),
                            globals = c("june_txt", "txt26", "meta", "clean_manifesto_text", "chunk_one", "estimate_tokens",
                                        "chunk_text", "FULL_TEXT_TOKEN_CAP", "CHUNK_TOKENS", "OVERLAP_TOKENS",
                                        "verbatim_check", "hallucination_check", "norm_soft", "system_prompt_v4_safe", "%||%")))
plan(sequential)
recheck <- recheck |> mutate(quote_ok_stored = (verbatim_found %in% TRUE) & !(hallucination_flag %in% TRUE),
                             quote_ok_new = verbatim_new & !halluc_new) |> arrange(set, doc_id, dim_name)
agree <- recheck |> group_by(set) |>
  summarise(spans = n(), chunk_missing = sum(!chunk_found),
            verbatim_agree = mean(verbatim_new == (verbatim_found %in% TRUE)),
            halluc_agree = mean(halluc_new == (hallucination_flag %in% TRUE)),
            quote_ok_agree = mean(quote_ok_new == quote_ok_stored),
            n_disagree = sum(quote_ok_new != quote_ok_stored), .groups = "drop")
write_csv_det(agree, file.path(CSV, "36_quote_check_recomputed.csv"))
write_csv_det(recheck |> filter(quote_ok_new != quote_ok_stored) |>
                transmute(set, doc_id, dim_name, quote_ok_stored, quote_ok_new, verbatim_found, verbatim_new,
                          hallucination_flag, halluc_new, chunk_found),
              file.path(CSV, "36_quote_check_disagreements.csv"))

# ---- 2. Parse decoder ---------------------------------------------------------------------------
rev2 <- read_parquet(v26_file("raw", "claude")) |> filter(!is.na(raw_tool_json))
objs <- map(rev2$raw_tool_json, ~ jsonlite::fromJSON(.x, simplifyVector = FALSE))
dec_test <- map_dfr(seq_along(objs), function(i) map_dfr(V4_DIMS, function(d) {
  o <- objs[[i]][[d]]
  if (!is.list(o)) return(tibble())
  js <- as.character(jsonlite::toJSON(o, auto_unbox = TRUE, null = "null"))
  l <- decode_dim_string(js, lenient_only = TRUE)
  same <- function(a, b) identical(if (is.null(a)) NA else a, if (is.null(b)) NA else b)
  tibble(doc_id = rev2$doc_id[i], dim = d,
         score = same(as.numeric(o$score %||% NA), as.numeric(l$score %||% NA)),
         status = same(o$evidence_status, l$evidence_status),
         quote = same(o$span$quote %||% "", l$span$quote),
         context = same(o$span$context %||% "", l$span$context))
}))
dec_sum <- dec_test |> summarise(valid_dim_objects = n(), score_agree = mean(score), status_agree = mean(status),
                                 quote_agree = mean(quote), context_agree = mean(context),
                                 all_agree = mean(score & status & quote & context))
reflat <- bind_rows(map(seq_along(objs), function(i)
  flatten_parsed_v4_claude(objs[[i]], rev2$doc_id[i], rev2$doc_id_root[i], rev2$chunk_id[i], "")$row))
cmp_cols <- c(paste0(rep(V4_DIMS, each = 4), c("_score", "_status", "_quote", "_context")), "parse_repaired")
stored <- rev2[match(reflat$doc_id, rev2$doc_id), cmp_cols]
diff_cells <- map_int(cmp_cols, function(cc) sum(!mapply(identical, as.list(stored[[cc]]), as.list(reflat[[cc]]))))
dec_sum <- dec_sum |> mutate(rev2_rows = nrow(rev2), reflatten_differing_cells = sum(diff_cells),
                             reflatten_differing_columns = paste(cmp_cols[diff_cells > 0], collapse = " "))
write_csv_det(dec_sum, file.path(CSV, "36_parse_decoder_test.csv"))

# ---- 3. Quote-length audit -----------------------------------------------------------------------
qlen <- imap_dfr(c(sets, gemini_june_superseded = "data/CMP/poplib_v4_gemini_flash_latest_validated_spans.parquet"),
                 function(f, s) {
  x <- read_parquet(f, col_select = c("doc_id_root", "quote", "context"))
  if (s == "gemini_june_superseded") x <- x |> filter(doc_id_root %in% alt)
  qw <- stri_count_regex(coalesce(x$quote, ""), "\\S+"); cw <- stri_count_regex(coalesce(x$context, ""), "\\S+")
  tibble(set = s, spans = nrow(x), quote_gt8 = mean(qw > 8), quote_max = max(qw), context_gt8 = mean(cw > 8),
         context_gt50 = mean(cw > 50), context_max = max(cw), context_median = median(cw))
})
write_csv_det(qlen, file.path(CSV, "36_quote_length_audit.csv"))
# ---- 4. Parse-loss counts (Claude, rev1 -> rev2; A.6) -------------------------------------------------
# rev1: chunk-dims with a missing evidence_status and no scoring error (the parse loss); rev2: dims
# decoded by the lenient parser of R/15b (column parse_repaired, comma-separated dim names)
r1 <- read_parquet(file.path(PIPE_IN, "raw_claude.parquet"))
st_cols <- grep("_status$", names(r1), value = TRUE)
no_err <- if ("scoring_error" %in% names(r1)) is.na(r1$scoring_error) | r1$scoring_error == "" else rep(TRUE, nrow(r1))
na_st <- as.matrix(is.na(r1[no_err, st_cols]))
pr <- read_parquet(v26_file("raw", "claude"), col_select = "parse_repaired")$parse_repaired
pl <- tibble(parse_lost_chunk_dims_rev1 = sum(na_st), parse_lost_chunks_rev1 = sum(rowSums(na_st) > 0),
             parse_lost_chunks_all13_rev1 = sum(rowSums(na_st) == length(st_cols)),
             rescored_chunks_rev2 = nrow(rev2),
             lenient_decoded_dims_rev2 = sum(lengths(strsplit(pr[!is.na(pr) & pr != ""], ","))),
             lenient_decoded_chunks_rev2 = sum(!is.na(pr) & pr != ""))
write_csv_det(pl, file.path(CSV, "36_parse_loss_counts.csv"))
print(agree); print(dec_sum); print(qlen); print(pl)
