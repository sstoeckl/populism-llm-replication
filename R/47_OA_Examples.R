# ============================================================
# 47_OA_Examples.R   (stage `oa_examples`; RESTRICTED: full mode only, all outputs NOT public)
#
# Online-appendix tables that print evidence text or need the full span data. Written 2026-10-27
# (PLLM-0064 / PLLM-0065). No API calls. Quotes and contexts are printed in full: this is
# paper-only material with a citation of the Manifesto Project corpus; R/71 --public lists every
# output of this stage as not public, and R/72 never exports them.
#
#   OA6   tables/oa_quote_lengths.tex      tab:oa6a  csv/47_quote_length_dist.csv
#         words per evidence quote and context, per model (mean, median, 90th percentile, max)
#   OA6   tables/oa_validation_by_model.tex tab:oa6b csv/47_validation_by_model.csv
#         per model x dimension: chunk-dimension records, shares verified / verified and passing the
#         quote check / no evidence / mixed / status missing; rev1 parse loss in the note
#   OA9   tables/oa9_examples_{populism,ideology,liberalism}.tex  tab:oa9a/b/c  csv/47_oa9_examples.csv
#   OA10  tables/oa10_model_diff.tex       tab:oa10a csv/47_oa10_model_diff.csv
#
# Selection rules (deterministic):
#   English-language manifestos: manifestos of the United Kingdom, Ireland, Australia, New Zealand
#   and the United States (the English entries of the country-language map of R/51).
#   OA9, per dimension: chunks of English-language manifestos where all three models give a score
#   with status verified and a quote that passes the quote check, and every model scores >= 7.
#   Among them the chunk with the highest cross-model agreement (smallest range max - min of the
#   three scores); ties: higher mean score, then chunk id (alphabetical). The evidence printed is
#   that of the model with the shortest context among the three (ties: Sonnet, GPT-4.1-mini,
#   Gemini), so that the table stays printable; its quote, context and block justification
#   (populism / ideology / liberalism) are printed in full.
#   OA10: chunks of English-language manifestos where all three models give a verified score on
#   populism_overall (resp. liberalism_overall); the five with the largest range max - min of the
#   three scores; ties: chunk id. One row per chunk x model with the model's score and quote.
# LaTeX: text is escaped; typographic characters are mapped to LaTeX; characters outside Latin-1
# that pdflatex with T1 / utf8 cannot set are replaced by "?".
# ============================================================

suppressPackageStartupMessages({ library(tidyverse); library(arrow); library(stringi) })
source("R/0_Config.R")
source("R/1_Aggregation.R")
stopifnot("R/47 needs the full (restricted) inputs" = !PIPE_PUBLIC)
TAB <- file.path(PIPE_OUT, "tables"); CSV <- file.path(PIPE_OUT, "csv")
MODELS <- c(sonnet = "Sonnet 4.6", gpt41mini = "GPT-4.1-mini", gemini = "Gemini 3.5 Flash")
RAW_STEM <- c(sonnet = "claude", gpt41mini = "gpt41mini", gemini = "gemini")
ENGLISH <- c("United Kingdom", "Ireland", "Australia", "New Zealand", "United States")
DIM_LBL <- c(pop_anti_elitism = "Anti-elitism", pop_people_centrism = "People-centrism", pop_manichean = "Manichaean",
             populism_overall = "Populism (overall)", pop_ideology_left = "Left ideology", pop_ideology_right = "Right ideology",
             pop_ideology_centrist = "Centrist ideology", pop_ideology_overall = "Ideology (overall intensity)",
             lib_political = "Political liberalism", lib_social = "Social liberalism", lib_economic = "Economic liberalism",
             lib_financial_market = "Financial-market liberalism", liberalism_overall = "Liberalism (overall)")
BLOCK <- c(setNames(rep("populism", 4), names(DIM_LBL)[1:4]), setNames(rep("ideology", 4), names(DIM_LBL)[5:8]),
           setNames(rep("liberalism", 5), names(DIM_LBL)[9:13]))
JUST_COL <- c(populism = "pop_justification", ideology = "pop_ideology_justification", liberalism = "lib_justification")
f1 <- function(x) formatC(x, format = "f", digits = 1)
write_tex <- function(lines, file) write_atomic(lines, file.path(TAB, file), writer = function(x, p) writeLines(x, p, useBytes = TRUE))
# LaTeX-safe text: typographic characters mapped, Latin-1 kept, everything else "?"
# Latin Extended letters that pdflatex (T1, utf8) cannot set directly -> LaTeX accent commands
ACC <- c("\u0101" = "\\={a}", "\u0113" = "\\={e}", "\u012b" = "\\={\\i}", "\u014d" = "\\={o}", "\u016b" = "\\={u}",
         "\u0100" = "\\={A}", "\u0112" = "\\={E}", "\u012a" = "\\={I}", "\u014c" = "\\={O}", "\u016a" = "\\={U}",
         "\u0151" = "\\H{o}", "\u0171" = "\\H{u}", "\u0150" = "\\H{O}", "\u0170" = "\\H{U}",
         "\u0219" = "\\c{s}", "\u021b" = "\\c{t}", "\u015f" = "\\c{s}", "\u0163" = "\\c{t}", "\u0218" = "\\c{S}", "\u021a" = "\\c{T}",
         "\u0103" = "\\u{a}", "\u0102" = "\\u{A}", "\u011f" = "\\u{g}", "\u011e" = "\\u{G}",
         "\u0131" = "\\i{}", "\u0130" = "\\.{I}", "\u0142" = "\\l{}", "\u0141" = "\\L{}",
         "\u0144" = "\\'{n}", "\u015b" = "\\'{s}", "\u017a" = "\\'{z}", "\u0107" = "\\'{c}",
         "\u0143" = "\\'{N}", "\u015a" = "\\'{S}", "\u0179" = "\\'{Z}", "\u0106" = "\\'{C}", "\u017c" = "\\.{z}", "\u017b" = "\\.{Z}",
         "\u010d" = "\\v{c}", "\u0161" = "\\v{s}", "\u017e" = "\\v{z}", "\u0159" = "\\v{r}", "\u011b" = "\\v{e}",
         "\u010f" = "\\v{d}", "\u0165" = "\\v{t}", "\u0148" = "\\v{n}", "\u010c" = "\\v{C}", "\u0160" = "\\v{S}",
         "\u017d" = "\\v{Z}", "\u0158" = "\\v{R}", "\u011a" = "\\v{E}", "\u010e" = "\\v{D}", "\u0164" = "\\v{T}", "\u0147" = "\\v{N}",
         "\u016f" = "\\r{u}", "\u016e" = "\\r{U}", "\u0111" = "\\dj{}", "\u0110" = "\\DJ{}")
TYPO <- c("\u2018" = "`", "\u2019" = "'", "\u201c" = "``", "\u201d" = "''", "\u2013" = "--", "\u2014" = "---",
          "\u2026" = "\\ldots{}", "\u20ac" = "EUR", "\u00a0" = " ", "\u2022" = "-", "\u00b7" = "-")
tex_text <- function(x) {
  x <- coalesce(x, "")
  x <- gsub("\\\\", "/", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("^", "\\\\textasciicircum{}", x, fixed = TRUE)
  x <- gsub("[<>]", "", x)
  for (k in names(TYPO)) x <- gsub(k, TYPO[[k]], x, fixed = TRUE)
  for (k in names(ACC)) x <- gsub(k, ACC[[k]], x, fixed = TRUE)
  x <- vapply(x, function(s) { cp <- utf8ToInt(s); if (anyNA(cp)) return("?")
    cp[cp > 255 | (cp < 32 & cp != 10)] <- utf8ToInt("?"); intToUtf8(cp) }, "", USE.NAMES = FALSE)
  gsub("\\s+", " ", x)
}

# ---- Data ------------------------------------------------------------------------------------------
meta <- read_parquet(file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet"),
                     col_select = c("doc_id", "countryname", "partyname", "partyabbrev", "date")) |>
  rename(doc_id_root = doc_id)
cl <- load_chunk_dims(PIPE_IN, PIPE_DERIVED)
raw_text <- imap_dfr(RAW_STEM, function(stem, m) {
  f <- v26_file("raw", stem)
  cols <- c("doc_id", unlist(map(names(DIM_LBL), ~ paste0(.x, c("_quote", "_context")))), unname(JUST_COL))
  read_parquet(f, col_select = all_of(cols)) |> mutate(model = m)
})
txt_long <- function(dims) raw_text |>
  select(doc_id, model, all_of(paste0(rep(dims, each = 2), c("_quote", "_context"))), all_of(unname(JUST_COL))) |>
  pivot_longer(all_of(paste0(rep(dims, each = 2), c("_quote", "_context"))), names_to = c("dim", ".value"),
               names_pattern = paste0("^(", paste(dims, collapse = "|"), ")_(quote|context)$"))

# ---- OA6a: quote and context lengths -----------------------------------------------------------------
spans <- imap_dfr(RAW_STEM, function(stem, m)
  read_parquet(v26_file("validated_spans", stem), col_select = c("quote", "context")) |> mutate(model = m))
qd <- spans |> mutate(q_words = stri_count_regex(coalesce(quote, ""), "\\S+"), c_words = stri_count_regex(coalesce(context, ""), "\\S+")) |>
  group_by(model) |>
  summarise(spans = n(), q_mean = mean(q_words), q_median = median(q_words), q_p90 = quantile(q_words, .9, names = FALSE, type = 7),
            q_max = max(q_words), c_mean = mean(c_words), c_median = median(c_words),
            c_p90 = quantile(c_words, .9, names = FALSE, type = 7), c_max = max(c_words), .groups = "drop") |>
  mutate(model = factor(model, levels = names(MODELS))) |> arrange(model) |> mutate(model = as.character(model))
write_csv_det(qd, file.path(CSV, "47_quote_length_dist.csv"))
write_tex(c("% v2026a; generated by R/47_OA_Examples.R (restricted stage)",
  "\\begin{table}[ht]", "\\caption{Length of the evidence quotes and their contexts (words), by model}", "\\label{tab:oa6a}",
  "\\footnotesize", "\\centering", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%",
  "\\begin{tabular}{lrrrrrrrrr}", "\\toprule",
  " & & \\multicolumn{4}{c}{Quote} & \\multicolumn{4}{c}{Context} \\\\", "\\cmidrule(lr){3-6} \\cmidrule(lr){7-10}",
  "Model & Spans & Mean & Median & 90\\% & Max & Mean & Median & 90\\% & Max \\\\", "\\midrule",
  with(qd, sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\", MODELS[model], scales::comma(spans), f1(q_mean), f1(q_median),
                   f1(q_p90), scales::comma(q_max), f1(c_mean), f1(c_median), f1(c_p90), scales::comma(c_max))),
  "\\bottomrule", "\\end{tabular}}", "\\par\\smallskip",
  "\\begin{minipage}{\\linewidth}\\scriptsize All evidence spans of the archived production scores (13 dimensions, all chunks), words counted as whitespace-separated tokens. The prompt asks for a quote of 3 to 8 words and a short context.\\end{minipage}",
  "\\end{table}"), "oa_quote_lengths.tex")

# ---- OA6b: validation by model and dimension ------------------------------------------------------------
vb <- cl |> group_by(model, dim) |>
  summarise(records = n(), verified = mean(status %in% "verified"), verified_quote_ok = mean(status %in% "verified" & quote_ok %in% TRUE),
            no_evidence = mean(status %in% "no_evidence"), mixed = mean(status %in% "mixed"), missing = mean(is.na(status)), .groups = "drop") |>
  mutate(model = factor(model, levels = names(MODELS)), dim = factor(dim, levels = names(DIM_LBL))) |> arrange(model, dim) |>
  mutate(across(c(model, dim), as.character))
write_csv_det(vb, file.path(CSV, "47_validation_by_model.csv"))
plc <- read.csv(file.path(CSV, "36_parse_loss_counts.csv"))
pc <- function(x) formatC(100 * x, format = "f", digits = 1)
vb_rows <- unlist(map(names(MODELS), function(m) {
  x <- vb |> filter(model == m)
  c(sprintf("\\multicolumn{7}{l}{\\emph{%s}} \\\\", MODELS[[m]]),
    sprintf("\\quad %s & %s & %s & %s & %s & %s & %s \\\\", DIM_LBL[x$dim], scales::comma(x$records), pc(x$verified),
            pc(x$verified_quote_ok), pc(x$no_evidence), pc(x$mixed), pc(x$missing)))
}))
write_tex(c("% v2026a; generated by R/47_OA_Examples.R (restricted stage)",
  "\\begin{table}[ht]", "\\caption{Evidence status and quote check by model and dimension (percent of chunk-dimension records)}",
  "\\label{tab:oa6b}", "\\scriptsize", "\\centering", "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%",
  "\\begin{tabular}{lrrrrrr}", "\\toprule",
  "Dimension & Records & Verified & Verified, quote check passed & No evidence & Mixed & Status missing \\\\", "\\midrule",
  vb_rows, "\\bottomrule", "\\end{tabular}}", "\\par\\smallskip",
  sprintf("\\begin{minipage}{\\linewidth}\\scriptsize Chunk-dimension records of the archived production scores (gap legs included as records without status). Verified, quote check passed: the records that enter the aggregation. Status missing: parse loss. Before the lenient decoder for Claude's output was added, %s Claude chunk-dimensions in %s chunks (%s chunks with all 13 dimensions) had no parsable status; these chunks were scored again with the decoder in place, which decoded %s dimensions leniently. No status is missing in the scores used.\\end{minipage}",
          scales::comma(plc$parse_lost_chunk_dims_rev1), scales::comma(plc$parse_lost_chunks_rev1),
          scales::comma(plc$parse_lost_chunks_all13_rev1), scales::comma(plc$lenient_decoded_dims_rev2)),
  "\\end{table}"), "oa_validation_by_model.tex")

# ---- Chunk-level candidates (English-language manifestos) ---------------------------------------------------
eng <- meta |> filter(countryname %in% ENGLISH)
cand <- cl |> filter(doc_id_root %in% eng$doc_id_root, !is.na(score)) |>
  select(doc_id, doc_id_root, dim, model, score, status, quote_ok)
wide3 <- function(x) x |> group_by(doc_id, doc_id_root, dim) |> filter(n_distinct(model) == 3) |>
  summarise(s_min = min(score), s_max = max(score), s_mean = mean(score), .groups = "drop")

# ---- OA9: one worked example per dimension ----------------------------------------------------------------
ok9 <- cand |> filter(status == "verified", quote_ok %in% TRUE)
pick9 <- wide3(ok9) |> filter(s_min >= 7) |> mutate(range = s_max - s_min) |>
  arrange(dim, range, desc(s_mean), doc_id) |> group_by(dim) |> slice(1) |> ungroup()
stopifnot("OA9: a dimension has no eligible chunk" = all(names(DIM_LBL) %in% pick9$dim))
tl <- txt_long(names(DIM_LBL))
ex <- pick9 |> left_join(ok9 |> select(doc_id, dim, model, score), by = c("doc_id", "dim")) |>
  left_join(tl, by = c("doc_id", "dim", "model")) |>
  mutate(ctx_words = stri_count_regex(coalesce(context, ""), "\\S+"),
         just = case_when(BLOCK[dim] == "populism" ~ pop_justification, BLOCK[dim] == "ideology" ~ pop_ideology_justification,
                          TRUE ~ lib_justification),
         m_ord = match(model, names(MODELS)))
shown <- ex |> arrange(dim, ctx_words, m_ord) |> group_by(dim) |> slice(1) |> ungroup() |>
  select(dim, doc_id, shown_model = model, quote, context, justification = just)
scores9 <- ex |> select(dim, model, score) |> pivot_wider(names_from = model, values_from = score, names_prefix = "score_")
oa9 <- pick9 |> select(dim, doc_id, doc_id_root, range, s_mean) |> left_join(scores9, by = "dim") |> left_join(shown, by = c("dim", "doc_id")) |>
  left_join(meta, by = "doc_id_root") |> mutate(block = BLOCK[dim], dim = factor(dim, levels = names(DIM_LBL))) |> arrange(dim) |>
  mutate(dim = as.character(dim))
write_csv_det(oa9 |> select(block, dim, doc_id, doc_id_root, countryname, partyname, date, score_sonnet, score_gpt41mini, score_gemini,
                            range, shown_model, quote, context, justification), file.path(CSV, "47_oa9_examples.csv"))
BLK_LBL <- c(populism = "populism", ideology = "ideology", liberalism = "liberalism")
for (b in names(BLK_LBL)) {
  x <- oa9 |> filter(block == b)
  rows <- unlist(map(seq_len(nrow(x)), function(i) with(x[i, ], c(
    sprintf("\\textbf{%s} --- %s (%s), election %s; manifesto %s, chunk %s. Scores: Sonnet %s, GPT-4.1-mini %s, Gemini %s. \\\\",
            DIM_LBL[[dim]], tex_text(partyname), tex_text(countryname), paste(month.name[as.integer(format(as.Date(date), "%m"))], format(as.Date(date), "%Y")), tex_text(doc_id_root),
            sub(".*_ch", "", doc_id), score_sonnet, score_gpt41mini, score_gemini),
    sprintf("\\quad \\emph{Quote (%s):} ``%s'' \\\\", MODELS[[shown_model]], tex_text(quote)),
    sprintf("\\quad \\emph{Context:} %s \\\\", tex_text(context)),
    sprintf("\\quad \\emph{Justification (%s block):} %s \\\\", b, tex_text(justification)), "\\midrule"))))
  write_tex(c("% v2026a; generated by R/47_OA_Examples.R (restricted stage); manifesto text from the Manifesto Project corpus",
    "{\\footnotesize",
    "\\begin{longtable}{p{0.96\\linewidth}}",
    sprintf("\\caption{Worked examples: %s dimensions}\\label{tab:oa9%s} \\\\", BLK_LBL[[b]], c(populism = "a", ideology = "b", liberalism = "c")[[b]]),
    "\\toprule", head(rows, -1), "\\bottomrule",
    "\\multicolumn{1}{p{0.96\\linewidth}}{\\scriptsize For each dimension the chunk of an English-language manifesto (United Kingdom, Ireland, Australia, New Zealand, United States) on which all three models give a verified score of at least 7 with a quote that passes the quote check and agree most closely (smallest range; ties: higher mean, then chunk id). Evidence shown: the model with the shortest context. Quote, context and justification as returned by the model. Manifesto text: Manifesto Project corpus \\citep{lehmann2026manifesto}, identified by the manifesto id.} \\\\",
    "\\end{longtable}", "}"), sprintf("oa9_examples_%s.tex", b))
}

# ---- OA10: largest model differences ----------------------------------------------------------------------
vfd <- cand |> filter(status == "verified")
oa10 <- map_dfr(c("populism_overall", "liberalism_overall"), function(dd) {
  top <- wide3(vfd |> filter(dim == dd)) |> mutate(range = s_max - s_min) |> arrange(desc(range), doc_id) |> slice_head(n = 5) |>
    mutate(rank = row_number())
  top |> select(dim, doc_id, doc_id_root, range, rank) |>
    left_join(vfd |> filter(dim == dd) |> select(doc_id, model, score), by = "doc_id") |>
    left_join(txt_long(dd) |> select(doc_id, model, quote), by = c("doc_id", "model"))
}) |> left_join(meta, by = "doc_id_root") |> mutate(m_ord = match(model, names(MODELS))) |> arrange(dim != "populism_overall", rank, m_ord)
write_csv_det(oa10 |> select(dim, rank, doc_id, doc_id_root, countryname, partyname, date, range, model, score, quote),
              file.path(CSV, "47_oa10_model_diff.csv"))
oa10_rows <- unlist(map(c("populism_overall", "liberalism_overall"), function(dd) {
  x <- oa10 |> filter(dim == dd)
  c(sprintf("\\multicolumn{4}{l}{\\textbf{%s}} \\\\", DIM_LBL[[dd]]), "\\midrule",
    unlist(map(unique(x$rank), function(k) { y <- x |> filter(rank == k)
      c(sprintf("\\multicolumn{4}{p{0.96\\linewidth}}{%s (%s), election %s; manifesto %s, chunk %s; range %s} \\\\",
                tex_text(y$partyname[1]), tex_text(y$countryname[1]), paste(month.name[as.integer(format(as.Date(y$date[1]), "%m"))], format(as.Date(y$date[1]), "%Y")), tex_text(y$doc_id_root[1]),
                sub(".*_ch", "", y$doc_id[1]), y$range[1]),
        sprintf(" & %s & %s & %s \\\\", MODELS[y$model], y$score, paste0("``", tex_text(y$quote), "''")), "\\midrule") })))
}))
write_tex(c("% v2026a; generated by R/47_OA_Examples.R (restricted stage); manifesto text from the Manifesto Project corpus",
  "{\\footnotesize",
  "\\begin{longtable}{p{0.02\\linewidth}p{0.17\\linewidth}rp{0.62\\linewidth}}",
  "\\caption{Largest differences between the models on the same chunk}\\label{tab:oa10a} \\\\",
  "\\toprule", " & Model & Score & Evidence quote \\\\", "\\midrule", "\\endfirsthead",
  " & Model & Score & Evidence quote \\\\", "\\midrule", "\\endhead",
  head(oa10_rows, -1), "\\bottomrule",
  "\\multicolumn{4}{p{0.96\\linewidth}}{\\scriptsize Chunks of English-language manifestos (United Kingdom, Ireland, Australia, New Zealand, United States) on which all three models give a verified score; the five chunks with the largest range (maximum minus minimum of the three scores), ties by chunk id. Quotes as returned by each model. Manifesto text: Manifesto Project corpus \\citep{lehmann2026manifesto}, identified by the manifesto id.} \\\\",
  "\\end{longtable}", "}"), "oa10_model_diff.tex")

message("R/47 done: ", paste(c("oa_quote_lengths", "oa_validation_by_model", paste0("oa9_examples_", names(BLK_LBL)), "oa10_model_diff"), collapse = ", "))
