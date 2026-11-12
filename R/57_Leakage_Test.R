# ============================================================
# 57_Leakage_Test.R  (PLLM-0208, 2026-11-11)
#
# LEAKAGE TEST: do the models recognise the party behind a manifesto and score its reputation
# instead of its text? A small, language-independent redaction experiment on 30 German-language
# manifestos (10 each from Germany, Austria, Switzerland), scored by the FROZEN production
# pipeline in four arms:
#   de_orig  original German text, real CMP doc_id in the prompt   (fresh rerun = noise control)
#   de_red   German text, own-party names / abbreviations / leading candidates / URLs replaced by
#            [PARTEI] / [PERSON] / [URL]; neutral document id (LKnn) in the prompt
#   en_orig  English translation of de_orig (Claude Sonnet 4.6, temperature 0, translation-only
#            prompt), real CMP doc_id in the prompt
#   en_red   the same redaction applied to the English translation ([PARTY] / [PERSON] / [URL]),
#            neutral document id
# The production user prompt carries "DOC_ID: <id>". The CMP doc_id (<party code>_<YYYYMM>)
# names the party code, so the redacted arms send the neutral id LKnn instead. The production
# system prompt carries "META: Country=..; ElectionYear=.." in every arm (frozen pipeline): the
# redaction removes the party's identity, not the country and the year.
#
# Scoring: R/15b, R/16 and R/18 definitions loaded exactly as R/32 / R/52 load them (same prompt,
# schema poplib_v4, temperature 0, retries, chunking; every document is a single chunk), R/32's
# usage log, guards and model-version trace; Gemini gemini-3.5-flash on Vertex AI global, abort
# if another version is served. R/12 span validation (incl. judge) as in R/52 finalize.
# Aggregation: R/1_Aggregation.R, canonical rule (a'), span checks as in R/34 / R/53.
#
# Recognition: a separate minimal prompt (RECOG_SYS below) asks which party, country and election
# year published the text, with a 0-100 confidence, as JSON (temperature 0, no metadata, no
# document id). Run on de_orig (ceiling), de_red and en_red with all three models. A party hit:
# the answer (name or abbreviation) matches an alias pattern of the true party AND the country is
# right (several aliases, e.g. "Social Democratic Party", are shared across countries).
#
# Usage (run from the project root):
#   Rscript R/57_Leakage_Test.R sample                  # register data/CMP/leakage_oos/sample.csv
#   Rscript R/57_Leakage_Test.R prepare                 # de_orig, de_red (+ en_red once translated), aliases.csv, redaction counts
#   Rscript R/57_Leakage_Test.R translate               # en_orig via Claude (resumable, one file per doc)
#   Rscript R/57_Leakage_Test.R score <arm> <model>     # checkpointed, resumable; then finalize
#   Rscript R/57_Leakage_Test.R finalize <arm> <model>  # checkpoints -> raw + validated spans
#   Rscript R/57_Leakage_Test.R recognise <model>       # recognition calls (resumable, one file per call)
#   Rscript R/57_Leakage_Test.R budget                  # spent so far + projection of the full design
#   Rscript R/57_Leakage_Test.R analyse                 # csv, tex, REPORT.md
#   <arm> in {de_orig, de_red, en_orig, en_red}; <model> in {claude, gpt41mini, gemini}
# Environment: LEAK_LIMIT=n (only the first n documents), LEAK_DOCS=LK07,LK19 (only these; the
#   benchmark: 2 documents per arm and model, the two closest to the sample's mean length),
#   LEAK_BUDGET_USD (default 25; every API stage refuses to start a batch once the logged spend
#   reaches it), GEMINI_BACKEND / GEMINI_MODEL / EXPECT_GEMINI as in R/52, CLAUDE_WORKERS /
#   GEMINI_WORKERS.
#
# Outputs:
#   data/CMP/leakage_oos/  sample.csv, aliases.csv, redaction_counts.csv, residual_mentions.csv,
#                          text/<arm>/<LKnn>.txt, translations.csv, checkpoints/<arm>_<model>/,
#                          scores/poplib_v4_<arm>_<model>_{raw,validated_spans}.parquet,
#                          recognition/<arm>_<model>/<LKnn>.json, usage_logs/<task>/usage_v2_<pid>.csv
#   output/v2026a/validation/leakage/{csv,tables}/, REPORT.md
# Never writes to data/CMP/v2026a/. Not a chain stage (not in run_all.R, not in the manifest).
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(lubridate)
  library(jsonlite); library(httr); library(furrr); library(stringi)
})

args  <- commandArgs(trailingOnly = TRUE)
MODE  <- args[1] %||% "budget"
ARMS  <- c("de_orig", "de_red", "en_orig", "en_red")
MODELS <- c("claude", "gpt41mini", "gemini")

LK_DIR  <- "data/CMP/leakage_oos"
LK_OUT  <- "output/v2026a/validation/leakage"
for (d in c(LK_DIR, file.path(LK_OUT, c("csv", "tables")))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
SAMPLE_SEED <- 20261111L
N_PER_COUNTRY <- 10L
TERC_ALLOC <- c(`1` = 3L, `2` = 3L, `3` = 4L)       # per country: low / mid / high populism tercile
BUDGET_USD <- as.numeric(Sys.getenv("LEAK_BUDGET_USD", "25"))
LEAK_LIMIT <- as.integer(Sys.getenv("LEAK_LIMIT", "0"))
# LEAK_DOCS=LK07,LK19: only these documents (the benchmark used the two documents closest to the
# sample's mean length, so the per-call costs are representative)
LEAK_DOCS  <- strsplit(Sys.getenv("LEAK_DOCS", ""), ",")[[1]]
# USD per 1M tokens (list prices). Gemini 3.5 Flash: the project's conservative assumption
# (R/51: Gemini-3-Flash rate 0.50 / 3.00). Thinking tokens billed as output.
PRICES <- tribble(
  ~pattern,          ~usd_in, ~usd_out,
  "^claude-sonnet",  3.00,    15.00,
  "^gpt-4\\.1-mini", 0.40,     1.60,
  "^gpt-4o-mini",    0.15,     0.60,
  "^gemini",         0.50,     3.00)

reg_file <- file.path(LK_DIR, "sample.csv")
read_register <- function() {
  r <- read_csv(reg_file, col_types = cols(.default = "c")) |>
    mutate(date = as.Date(date), across(c(token_count, tok_o200k, populism_overall, liberalism_overall), as.numeric))
  if (LEAK_LIMIT > 0) r <- r |> slice_head(n = LEAK_LIMIT)
  if (length(LEAK_DOCS)) r <- r |> filter(lk_id %in% LEAK_DOCS)
  r
}
text_path <- function(arm, lk) file.path(LK_DIR, "text", arm, paste0(lk, ".txt"))
read_text <- function(arm, lk) paste(readLines(text_path(arm, lk), encoding = "UTF-8", warn = FALSE), collapse = "\n")
write_text_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp"); con <- file(tmp, "w", encoding = "UTF-8"); writeLines(x, con, useBytes = FALSE); close(con)
  stopifnot(file.rename(tmp, path))
}
write_atomic <- function(x, path) { tmp <- paste0(path, ".tmp"); arrow::write_parquet(x, tmp); stopifnot(file.rename(tmp, path)) }
write_csv_atomic <- function(x, path) { tmp <- paste0(path, ".tmp"); write_csv(x, tmp, na = ""); stopifnot(file.rename(tmp, path)) }

# ---- Cost accounting from the usage logs ------------------------------------------------
read_usage <- function() {
  fs <- list.files(file.path(LK_DIR, "usage_logs"), "^usage_v2_.*\\.csv$", recursive = TRUE, full.names = TRUE)
  if (!length(fs)) return(tibble(task = character(), model = character(), usd = numeric()))
  map_dfr(fs, ~ read_csv(.x, col_types = cols(.default = "c")) |> mutate(task = basename(dirname(.x)))) |>
    mutate(across(c(in_tok, out_tok, thought_tok), as.numeric), thought_tok = coalesce(thought_tok, 0)) |>
    mutate(p = map_int(model, ~ which(str_detect(.x, PRICES$pattern))[1]),
           usd = (in_tok * PRICES$usd_in[p] + (out_tok + thought_tok) * PRICES$usd_out[p]) / 1e6) |>
    select(-p)
}
spent_usd <- function() sum(read_usage()$usd, na.rm = TRUE)
check_budget <- function(where) {
  s <- spent_usd()
  if (s >= BUDGET_USD) { message(sprintf("BUDGET: USD %.2f spent >= cap %.0f; %s not started", s, BUDGET_USD, where)); quit(save = "no", status = 5) }
  invisible(s)
}

# ============================================================
# MODE: sample
# ============================================================
if (MODE == "sample") {
  source("R/1_Functions.R"); source("R/2_Helpers.R")
  cap <- local({ e <- new.env(); for (x in parse("R/15b_Claude_Full_Corpus.R", keep.source = FALSE))
    if (is.call(x) && identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name("FULL_TEXT_TOKEN_CAP"))) eval(x, e); e$FULL_TEXT_TOKEN_CAP })
  stopifnot(cap == 18000L)
  base <- read_parquet("data/CMP/v2026a/manifesto_scores_v2026a.parquet")
  # terciles of the baseline populism score over the whole corpus (ties at the cut points go down)
  cuts <- quantile(base$populism_overall, c(1/3, 2/3), na.rm = TRUE, names = FALSE)
  meta <- read_parquet("data/CMP/full_data_2026a_meta.parquet") |> filter(has_text) |> distinct(doc_id, .keep_all = TRUE)
  nch <- map_dfr(c("claude_rev2", "gpt41mini", "gemini"), ~ read_parquet(sprintf("data/CMP/v2026a/raw_%s.parquet", .x),
                                                                      col_select = c("doc_id_root", "chunk_id")) |>
                   count(doc_id_root, name = "n_ch") |> mutate(m = .x)) |>
    group_by(doc_id_root) |> summarise(n_ch_max = max(n_ch), n_models = n(), .groups = "drop")
  pool <- base |>
    filter(countryname %in% c("Germany", "Austria", "Switzerland"), !is.na(populism_overall), !is.na(liberalism_overall)) |>
    inner_join(nch |> filter(n_ch_max == 1L, n_models == 3L), by = c("doc_id" = "doc_id_root"))
  # the text the baseline was scored on: corpus 2026-1, cleaned as in R/30 (R/36 uses the same)
  txt <- read_parquet("data/CMP/update_2026a/corpus_texts_20260904101355.parquet", col_select = c("doc_id", "text_raw")) |>
    filter(doc_id %in% pool$doc_id) |> distinct(doc_id, .keep_all = TRUE) |>
    transmute(doc_id, text = map_chr(text_raw, clean_manifesto_text))
  pool <- pool |> inner_join(txt, by = "doc_id")
  # language: share of tokens that are German / French / Italian stopwords (first 20,000 characters)
  SW <- list(de = c("der", "die", "und", "das", "nicht", "ist", "wir", "für", "mit", "den", "von", "zu", "auf",
                    "sich", "ein", "eine", "dem", "des", "im", "auch", "werden"),
             fr = c("le", "la", "et", "les", "des", "pour", "que", "une", "dans", "est", "nous", "du", "en", "au", "pas", "sur"),
             it = c("il", "di", "che", "per", "e", "una", "della", "del", "non", "sono", "dei", "con", "alla", "gli"))
  sw_share <- function(t, w) { x <- tolower(unlist(strsplit(substr(t, 1, 20000), "[^[:alpha:]]+"))); mean(x %in% w) }
  pool <- pool |>
    mutate(sw_de = map_dbl(text, sw_share, SW$de), sw_fr = map_dbl(text, sw_share, SW$fr), sw_it = map_dbl(text, sw_share, SW$it),
           german = sw_de >= 0.15 & sw_de >= 5 * pmax(sw_fr, sw_it),
           tok_o200k = map_dbl(text, estimate_tokens)) |>
    filter(german, tok_o200k <= cap) |>
    mutate(terc = as.character(cut(populism_overall, c(-Inf, cuts, Inf), labels = 1:3)))
  message(sprintf("pool: %d German single-chunk manifestos (DE %d, AT %d, CH %d); tercile cuts %.2f / %.2f",
                  nrow(pool), sum(pool$countryname == "Germany"), sum(pool$countryname == "Austria"),
                  sum(pool$countryname == "Switzerland"), cuts[1], cuts[2]))
  set.seed(SAMPLE_SEED)
  smp <- pool |> arrange(countryname, terc, doc_id) |> group_by(countryname, terc) |>
    group_modify(~ slice_sample(.x, n = min(nrow(.x), TERC_ALLOC[[.y$terc]]))) |> ungroup()
  stopifnot(nrow(smp) == 3L * N_PER_COUNTRY)
  meta_cols <- meta |> select(doc_id, candidatename)
  reg <- smp |> left_join(meta_cols, by = "doc_id") |>
    mutate(candidatename = na_if(na_if(candidatename, "NA"), "")) |>
    arrange(match(countryname, c("Germany", "Austria", "Switzerland")), date, party) |>
    mutate(lk_id = sprintf("LK%02d", row_number())) |>
    transmute(lk_id, doc_id, party = as.character(party), partyname, partyabbrev, countryname, date,
              token_count, tok_o200k, populism_tercile = terc, populism_overall, liberalism_overall,
              candidatename, sw_de = round(sw_de, 4))
  write_csv_atomic(reg, reg_file)
  # the original German text = arm de_orig (exactly the text the baseline scored)
  for (i in seq_len(nrow(reg))) write_text_atomic(smp$text[match(reg$doc_id[i], smp$doc_id)], text_path("de_orig", reg$lk_id[i]))
  print(as.data.frame(reg |> select(lk_id, doc_id, partyabbrev, countryname, date, tok_o200k, populism_tercile, populism_overall, liberalism_overall)))
  quit(save = "no")
}

# ============================================================
# Alias table (built by hand on 2026-11-11 from partyname / partyabbrev of the 30 sampled
# documents, a reading of every text, and the leading candidates / own-party office holders
# named in the texts; candidatename is "NA" for all DE/AT/CH rows of MPDS2026a).
#   kind  name     party name and its variants (incl. self-designations "wir Sozialdemokraten",
#                  "wir Freiheitlichen", "Die Union"), -> [PARTEI] / [PARTY]
#         abbrev   abbreviations (case-sensitive, also in compounds like "SP-Bundesrat")
#         person   leading candidates and own-party office holders named in the text, -> [PERSON]
#                  (letter-spaced print "E r h a r d" and ALL-CAPS tolerated; applied in both languages)
#         check    case-insensitive stems used only to count residual mentions after redaction
#   lang  de | en | both
# Rules: lower-case ideological adjectives are kept (liberal, grün, christlich, sozialistisch,
# kommunistisch) because they carry substantive content; exceptions where the adjective is the
# party's own label: sozialdemokratisch* (SPD, SPÖ, SP), christlich-demokratisch* (CDU), freiheitlich*
# (FPÖ), freisinn* (FDP CH), grün* (both Green parties: "das grüne Modell", "grüne Konzepte").
# Only the OWN party is redacted (rival party names stay: they are content, e.g. anti-elitism).
# Regexes are ICU; "(?i)" marks case-insensitive patterns.
# ============================================================
A <- function(party, lang, kind, pattern, note = "") tibble(party = party, lang = lang, kind = kind, pattern = pattern, note = note)
ALIASES <- bind_rows(
  # --- Germany ---------------------------------------------------------------------------
  A("41220", "de", "name",  "(?i)kommunistische[nr]? partei( deutschlands)?"),
  A("41220", "de", "name",  "\\bKommunisten\\b", "self-designation 'wir Kommunisten'"),
  A("41220", "both", "abbrev", "\\bKPD\\b"),
  A("41220", "both", "person", "Max Reimann", "KPD chairman 1948-1956"),
  A("41220", "both", "person", "Reimann"),
  A("41220", "en", "name",  "(?i)communist party( of germany)?"),
  A("41220", "en", "name",  "\\bCommunists\\b"),
  A("41220", "both", "check", "(?i)\\bkpd\\b|kommunistische partei|communist party|reimann"),
  A("41522", "de", "name",  "(?i)deutsche[nr]? zentrumspartei"),
  A("41522", "de", "name",  "(?i)zentrumspartei"),
  A("41522", "de", "name",  "\\bZentrum\\b", "the party calls itself 'das Zentrum'"),
  A("41522", "both", "abbrev", "\\bDZ\\b"),
  A("41522", "both", "person", "Helene Wessel", "chair 1949"),
  A("41522", "en", "name",  "(?i)(german )?cent(re|er) party"),
  A("41522", "en", "name",  "\\b(the )?Cent(re|er)\\b(?! of)", "capitalised self-reference in the translation"),
  A("41522", "en", "name",  "\\bZentrum\\b"),
  A("41522", "both", "check", "(?i)zentrum|cent(re|er) party|\\bdz\\b"),
  A("41711", "de", "name",  "(?i)wirtschaftliche[nr]? aufbau-?\\s?vereinigung"),
  A("41711", "de", "name",  "(?i)aufbau-?\\s?vereinigung"),
  A("41711", "both", "abbrev", "\\bWAV\\b"),
  A("41711", "both", "person", "Alfred Loritz", "WAV founder and leader"),
  A("41711", "both", "person", "Loritz"),
  A("41711", "en", "name",  "(?i)economic reconstruction (association|league|union|alliance)"),
  A("41711", "en", "name",  "(?i)reconstruction (association|league|union|alliance)"),
  A("41711", "both", "check", "(?i)\\bwav\\b|aufbau-?\\s?vereinigung|reconstruction (association|league|union|alliance)|loritz"),
  A("41712", "de", "name",  "(?i)deutsche[nr]?[- ]rechts[- ]?partei"),
  A("41712", "de", "name",  "(?i)rechts-partei"),
  A("41712", "both", "abbrev", "\\bDRP\\b"),
  A("41712", "en", "name",  "(?i)german (right|rights|law|legal|justice)[- ]party"),
  A("41712", "en", "name",  "(?i)deutsche[- ]rechts[- ]partei"),
  A("41712", "en", "abbrev", "\\bGRP\\b", "abbreviation invented by the translation ('German Rights Party')"),
  A("41712", "both", "check", "(?i)\\b[dg]rp\\b|rechts-?partei|(right|rights|law|legal|justice)[- ]party"),
  A("41521", "de", "name",  "(?i)christlich[- ]demokratische[nr]? union( deutschlands)?"),
  A("41521", "de", "name",  "(?i)christlich[- ]soziale[nr]? union( in bayern)?"),
  A("41521", "de", "name",  "(?i)christdemokrat\\w*"),
  A("41521", "de", "name",  "(?i)christlich[- ]demokratisch\\w*", "the party's own label"),
  A("41521", "de", "name",  "(?<!Europäische |Europäischen |Europäischer |Westeuropäische |Westeuropäischen )\\bUnion(sparteien|s)?\\b", "'die Union' = CDU/CSU; EU forms excluded"),
  A("41521", "both", "abbrev", "\\bCDU/CSU\\b"), A("41521", "both", "abbrev", "\\bCDU/CDU\\b", "typo in the 1983 title"),
  A("41521", "both", "abbrev", "\\bCDU\\b"), A("41521", "both", "abbrev", "\\bCSU\\b"),
  A("41521", "both", "person", "Konrad Adenauer"), A("41521", "both", "person", "Adenauer"),
  A("41521", "both", "person", "Ludwig Erhard"), A("41521", "both", "person", "Erhard"),
  A("41521", "both", "person", "Helmut Kohl"), A("41521", "both", "person", "Kohl"),
  A("41521", "both", "person", "Franz Josef Strauß"), A("41521", "both", "person", "Strauß"), A("41521", "both", "person", "Strauss"),
  A("41521", "both", "person", "Barzel", "CDU parliamentary leader 1965"), A("41521", "both", "person", "Gerstenmaier", "CDU, Bundestag president"),
  A("41521", "both", "person", "Dufhues", "CDU executive chairman"), A("41521", "both", "person", "Schröder", "Gerhard Schröder (CDU), foreign minister 1965"),
  A("41521", "both", "person_rx", "\\bB\\s?[lI]\\s?a\\s?n\\s?k\\b", "Theodor Blank (CDU minister), letter-spaced with OCR I/l"),
  A("41521", "en", "name",  "(?i)christian democratic union( of germany)?"),
  A("41521", "en", "name",  "(?i)christian social union( in bavaria)?"),
  A("41521", "en", "name",  "(?i)christian democrat\\w*"),
  A("41521", "en", "name",  "(?<!European |Western European |Soviet |Monetary |Economic |Banking )\\b(the )?Union\\b(?! of Soviet)(?! for Peace)", "'the Union' = CDU/CSU in the translation"),
  A("41521", "en", "name",  "\\bUnion parties\\b"),
  A("41521", "both", "check", "(?i)\\bcdu\\b|\\bcsu\\b|christlich[- ]demokratisch|christian democratic|christdemokrat|adenauer|\\berhard|\\bkohl\\b|strau(ß|ss)\\b"),
  A("41420", "de", "name",  "(?i)freie[nr]? demokratische[nr]? partei"),
  A("41420", "de", "name",  "(?i)freie[nr]? demokraten"),
  A("41420", "de", "name",  "\\bLiberalen?\\b", "capitalised noun 'die Liberalen' only"),
  A("41420", "both", "abbrev", "\\bF\\.\\s?D\\.\\s?P\\.?"), A("41420", "both", "abbrev", "\\bFDP\\b"),
  A("41420", "both", "person", "Walter Scheel"), A("41420", "both", "person", "Scheel"), A("41420", "both", "person", "Genscher"),
  A("41420", "en", "name",  "(?i)free democratic party"), A("41420", "en", "name", "(?i)free democrats"),
  A("41420", "en", "name",  "\\bLiberals\\b", "capitalised noun only"),
  A("41420", "both", "check", "(?i)\\bf\\.?\\s?d\\.?\\s?p\\b|freie[nr]? demokrat|free democrat|scheel"),
  A("41320", "de", "name",  "(?i)sozialdemokratische[nr]? partei( deutschlands)?"),
  A("41320", "de", "name",  "(?i)sozialdemokrat\\w*"),
  A("41320", "both", "abbrev", "\\bSPD\\b"),
  A("41320", "both", "person", "Johannes Rau"), A("41320", "both", "person", "Rau"),
  A("41320", "both", "person", "Willy Brandt"), A("41320", "both", "person", "Brandt"),
  A("41320", "both", "person", "Helmut Schmidt"),
  A("41320", "en", "name",  "(?i)social democratic party( of germany)?"),
  A("41320", "en", "name",  "(?i)social democra\\w*"),
  A("41320", "both", "check", "(?i)\\bspd\\b|sozialdemokrat|social democra|\\brau\\b|brandt|helmut schmidt"),
  A("41953", "de", "name",  "(?i)alternative für deutschland"),
  A("41953", "both", "abbrev", "\\bAfD\\b"),
  A("41953", "both", "person", "Bernd Lucke"), A("41953", "both", "person", "Lucke"),
  A("41953", "en", "name",  "(?i)alternative for germany"),
  A("41953", "both", "check", "(?i)\\bafd\\b|alternative (für|for) deutschland|alternative for germany|lucke"),
  # --- Austria ----------------------------------------------------------------------------
  A("42320", "de", "name",  "(?i)sozialistische[nr]? partei( österreichs)?", "SPÖ name until 1991"),
  A("42320", "de", "name",  "(?i)sozialdemokratische[nr]? partei( österreichs)?"),
  A("42320", "de", "name",  "(?i)sozialdemokrat\\w*"),
  A("42320", "de", "name",  "\\bSozialisten\\b", "self-designation 'wir Sozialisten'"),
  A("42320", "both", "abbrev", "\\bSPÖ\\b"), A("42320", "both", "abbrev", "\\bSPO\\b"),
  A("42320", "both", "person", "Bruno Pittermann"), A("42320", "both", "person", "Pittermann"),
  A("42320", "both", "person", "Bruno Kreisky"), A("42320", "both", "person", "Kreisky"),
  A("42320", "both", "person", "Franz Vranitzky"), A("42320", "both", "person", "Vranitzky"),
  A("42320", "en", "name",  "(?i)socialist party( of austria)?"),
  A("42320", "en", "name",  "(?i)social democratic party( of austria)?"),
  A("42320", "en", "name",  "(?i)social democra\\w*"),
  A("42320", "en", "name",  "\\bSocialists\\b"),
  A("42320", "both", "check", "(?i)\\bsp(ö|o)\\b|sozialistische partei|socialist party|sozialdemokrat|social democra|pittermann|kreisky|vranitzky"),
  A("42420", "de", "name",  "(?i)freiheitliche[nr]? partei( österreichs)?"),
  A("42420", "de", "name",  "(?i)freiheitlich\\w*", "the party's own label; all case forms"),
  A("42420", "both", "abbrev", "\\bFPÖ\\b"), A("42420", "both", "abbrev", "\\bFPO\\b"),
  A("42420", "both", "person", "Friedrich Peter", "FPÖ chairman 1958-1978"),
  A("42420", "both", "person", "Jörg Haider"), A("42420", "both", "person", "Haider"),
  A("42420", "both", "person", "Norbert Steger"), A("42420", "both", "person", "Steger"),
  A("42420", "both", "person", "Heinz-Christian Strache"), A("42420", "both", "person", "Strache"),
  A("42420", "en", "name",  "(?i)freedom party( of austria)?"),
  A("42420", "en", "name",  "(?i)\\blibertarian party( of austria)?\\b"),
  A("42420", "en", "name",  "(?i)\\bfreedomites?\\b"),
  A("42420", "en", "name",  "\\bLibertarians?\\b", "capitalised noun, in case the translation renders 'die Freiheitlichen' so"),
  A("42420", "both", "check", "(?i)\\bfp(ö|o)\\b|freiheitlich|freedom party|freedomite|haider|steger|strache|friedrich peter"),
  A("42520", "de", "name",  "(?i)(österreichische[nr]? )?volkspartei"),
  A("42520", "both", "abbrev", "\\bÖVP\\b"), A("42520", "both", "abbrev", "\\bOVP\\b"),
  A("42520", "both", "person", "Josef Riegler"), A("42520", "both", "person", "Riegler"),
  A("42520", "en", "name",  "(?i)(austrian )?people['’]s party"),
  A("42520", "both", "check", "(?i)\\b(ö|o)vp\\b|volkspartei|people['’]s party|riegler"),
  A("42110", "de", "name",  "(?i)die grüne alternative"), A("42110", "de", "name", "(?i)grüne alternative"),
  A("42110", "de", "name",  "(?i)\\bgr[üu]n(e|en|er|es|em)?\\b", "noun and adjective: in the Green manifestos 'das grüne Modell' is the party label; incl. OCR 'GRüNE'"),
  A("42110", "both", "person", "Alexander Van der Bellen"), A("42110", "both", "person", "Van der Bellen"),
  A("42110", "both", "person", "Eva Glawischnig"), A("42110", "both", "person", "Glawischnig"),
  A("42110", "both", "person", "Madeleine Petrovic"), A("42110", "both", "person", "Petrovic"),
  A("42110", "en", "name",  "(?i)green alternative"),
  A("42110", "en", "name",  "(?i)\\bgreen party\\b"),
  A("42110", "en", "name",  "(?i)\\b(the )?greens?\\b", "noun and adjective, as in German"),
  A("42110", "both", "check", "(?i)\\bgr[üu]n(e|en|er|es|em)?\\b|\\bgreens?\\b|bellen|glawischnig|petrovic"),
  A("42120", "de", "name",  "(?i)liste (peter )?pilz"),
  A("42120", "both", "abbrev", "\\bPILZ\\b"),
  A("42120", "both", "person", "Peter Pilz"), A("42120", "both", "person", "Pilz"),
  A("42120", "both", "person", "Alma Zadic"), A("42120", "both", "person", "Daniela Holzinger"),
  A("42120", "both", "person", "Peter Kolba"), A("42120", "both", "person", "Stephanie Cox"),
  A("42120", "both", "person", "Maria Stern"), A("42120", "both", "person", "Alfred Noll"),
  A("42120", "both", "person", "Hannes Werthner"), A("42120", "both", "person", "Martha Bissmann"),
  A("42120", "both", "person", "Bruno Rossmann"), A("42120", "both", "person", "Renée Schroeder"),
  A("42120", "both", "person", "Sebastian Bohrn-Mena"), A("42120", "both", "person", "Teresa Roscher"),
  A("42120", "both", "person", "Wolfgang Zinggl"), A("42120", "both", "person", "Maria Chelucci"),
  A("42120", "en", "name",  "(?i)(peter )?pilz list"), A("42120", "en", "name", "(?i)list (peter )?pilz"),
  A("42120", "both", "check", "(?i)pilz|zadic|kolba|holzinger|rossmann|zinggl"),
  # --- Switzerland ------------------------------------------------------------------------
  A("43520", "de", "name",  "(?i)konservativ\\W{0,3}christlich\\W{0,3}sozial\\w*(\\s+(volks)?partei(\\s+der\\s+schweiz)?)?", "OCR '?' as hyphen"),
  A("43520", "de", "name",  "(?i)christlich\\W?demokratische\\w* volkspartei( der schweiz)?"),
  A("43520", "de", "name",  "(?i)christlichsozial\\w*"),
  A("43520", "de", "name",  "(?i)christliche volkspartei"),
  A("43520", "de", "name",  "(?i)volkspartei"),
  A("43520", "both", "abbrev", "\\bK\\.?C\\.?V\\.?P\\b"), A("43520", "both", "abbrev", "\\bCVP\\b"),
  A("43520", "both", "abbrev", "\\bPDC\\b"),
  A("43520", "both", "person", "Christophe Darbellay"), A("43520", "both", "person", "Darbellay"),
  A("43520", "both", "person", "Doris Leuthard"), A("43520", "both", "person", "Leuthard"),
  A("43520", "en", "name",  "(?i)conservative[- ]christian[- ]social\\w*( people['’]s)?( party)?( of switzerland)?"),
  A("43520", "en", "name",  "(?i)christian democratic people['’]s party( of switzerland)?"),
  A("43520", "en", "name",  "(?i)christian people['’]s party"),
  A("43520", "en", "name",  "(?i)people['’]s party"),
  A("43520", "both", "check", "(?i)\\bk?cvp\\b|volkspartei|people['’]s party|christlichsozial|christian[- ]social|darbellay|leuthard"),
  A("43320", "de", "name",  "(?i)sozialdemokratische[nr]? partei( der schweiz)?"),
  A("43320", "de", "name",  "(?i)sozialdemokrat\\w*"),
  A("43320", "de", "name",  "(?i)jungsozialist\\w*", "own youth organisation"),
  A("43320", "both", "abbrev", "\\bSPS\\b"), A("43320", "both", "abbrev", "\\bSP\\b"),
  A("43320", "both", "abbrev", "\\bPSS\\b"), A("43320", "both", "abbrev", "\\bJUSO\\b"),
  A("43320", "both", "person", "Max Frisch", "named author of the 1971 foreword"),
  A("43320", "both", "person_rx", "\\bH[.,]\\s?P\\.\\s?Tschudi\\b", "Hans-Peter Tschudi (SP federal councillor)"),
  A("43320", "both", "person", "Tschudi"),
  A("43320", "both", "person", "Willi Ritschard"), A("43320", "both", "person", "Ritschard"),
  A("43320", "both", "person", "Spühler"), A("43320", "both", "person", "Graber"),
  A("43320", "both", "person", "Arthur Schmid"), A("43320", "both", "person", "Helmut Hubacher"), A("43320", "both", "person", "Hubacher"),
  A("43320", "both", "person", "Lilian Uchtenhagen"), A("43320", "both", "person", "Uchtenhagen"),
  A("43320", "both", "person", "Peter Bodenmann"), A("43320", "both", "person", "Bodenmann"),
  A("43320", "both", "person", "Christiane Brunner"), A("43320", "both", "person", "Brunner"),
  A("43320", "both", "person", "Andreas Gross"), A("43320", "both", "person", "Otto Stich"),
  A("43320", "both", "person_rx", "\\bBundesrat Stich\\b"), A("43320", "both", "person", "Ruth Dreifuss"), A("43320", "both", "person", "Dreifuss"),
  A("43320", "en", "name",  "(?i)social democratic party( of switzerland)?"),
  A("43320", "en", "name",  "(?i)social democra\\w*"),
  A("43320", "en", "name",  "(?i)young socialists?"),
  A("43320", "both", "check", "(?i)\\bsps?\\b|sozialdemokrat|social democra|tschudi|ritschard|hubacher|bodenmann|dreifuss|uchtenhagen|frisch"),
  A("43810", "de", "name",  "(?i)schweizerische[nr]? volkspartei"),
  A("43810", "de", "name",  "(?i)volkspartei"),
  A("43810", "de", "name",  "(?i)bauern-?,? gewerbe- und bürgerpartei"),
  A("43810", "both", "abbrev", "\\bSVP\\b"), A("43810", "both", "abbrev", "\\bUDC\\b"), A("43810", "both", "abbrev", "\\bBGB\\b"),
  A("43810", "both", "person", "Rudolf Gnägi"), A("43810", "both", "person", "Gnägi"),
  A("43810", "both", "person", "Hans Conzett"), A("43810", "both", "person", "Conzett"),
  A("43810", "en", "name",  "(?i)swiss people['’]s party"), A("43810", "en", "name", "(?i)people['’]s party"),
  A("43810", "both", "check", "(?i)\\bsvp\\b|\\budc\\b|volkspartei|people['’]s party|gnägi|conzett"),
  A("43110", "de", "name",  "(?i)grüne partei der schweiz"), A("43110", "de", "name", "(?i)grüne schweiz"),
  A("43110", "de", "name",  "(?i)\\bgr[üu]n(e|en|er|es|em)?\\b", "noun and adjective, as for the Austrian Greens"),
  A("43110", "both", "abbrev", "\\bGPS\\b"), A("43110", "both", "abbrev", "\\bPES\\b"),
  A("43110", "both", "person", "Ruth Genner"), A("43110", "both", "person", "Genner"),
  A("43110", "en", "name",  "(?i)green party( of switzerland)?"), A("43110", "en", "name", "(?i)greens? switzerland|swiss greens"),
  A("43110", "en", "name",  "(?i)\\b(the )?greens?\\b", "noun and adjective, as in German"),
  A("43110", "both", "check", "(?i)\\bgr[üu]n(e|en|er|es|em)?\\b|\\bgreens?\\b|\\bgps\\b|genner"),
  A("43220", "de", "name",  "(?i)partei der arbeit( der schweiz)?"),
  A("43220", "both", "abbrev", "\\bPdA[ST]?\\b"), A("43220", "both", "abbrev", "\\bPdAS/PdTS\\b"), A("43220", "both", "abbrev", "\\bPdT[S]?\\b"),
  A("43220", "en", "name",  "(?i)(swiss )?(party of labou?r|labou?r party)( of switzerland)?"),
  A("43220", "en", "name",  "(?i)workers['’]? party( of switzerland)?"),
  A("43220", "both", "check", "(?i)\\bpd[at]s?\\b|partei der arbeit|party of labou?r|labou?r party|workers['’]? party"),
  A("43420", "de", "name",  "(?i)fdp\\.?\\s?die liberalen"),
  A("43420", "de", "name",  "(?i)freisinnig-demokratische[nr]? partei( der schweiz)?"),
  A("43420", "de", "name",  "(?i)freisinn\\w*", "the party's historic label"),
  A("43420", "de", "name",  "\\bLiberalen?\\b", "capitalised noun only"),
  A("43420", "both", "abbrev", "\\bFDP\\b"), A("43420", "both", "abbrev", "\\bPLR\\b"),
  A("43420", "both", "person", "Philipp Müller"), A("43420", "both", "person", "Petra Gössi"),
  A("43420", "en", "name",  "(?i)fdp\\.?\\s?the liberals"), A("43420", "en", "name", "(?i)free democratic party( of switzerland)?"),
  A("43420", "en", "name",  "(?i)radical[- ]democratic party"), A("43420", "en", "name", "\\b(The )?Liberals\\b"),
  A("43420", "both", "check", "(?i)\\bfdp\\b|\\bplr\\b|freisinn|die liberalen|the liberals|free democratic")
)
PLACEHOLDER <- c(de = "[PARTEI]", en = "[PARTY]")
URL_RX <- "(?i)\\b(https?://|www\\.)\\S+|\\b[\\w.+-]+@[\\w-]+\\.[\\w.]+"

# person name -> regex tolerating letter-spaced print ("E r h a r d") and ALL CAPS
person_rx <- function(name) {
  one <- function(nm) paste(vapply(strsplit(nm, " ")[[1]], function(tok)
    paste(stri_escape_unicode_rx(strsplit(tok, "")[[1]]), collapse = "\\s?"), character(1)), collapse = "\\s+")
  paste0("\\b(", one(name), "|", one(toupper(name)), ")s?\\b")       # genitive "Adenauers"
}
stri_escape_unicode_rx <- function(ch) ifelse(grepl("[][(){}.*+?^$|\\\\-]", ch), paste0("\\", ch), ch)

# the redaction rules for one party and language, in application order: names, persons,
# abbreviations, each in the hand-written order of ALIASES (full forms before short forms);
# URLs and e-mail addresses last
redaction_rules <- function(party, lang) {
  ph <- PLACEHOLDER[[lang]]
  al <- ALIASES |> mutate(row = row_number()) |>
    filter(.data$party == !!party, .data$lang %in% c(!!lang, "both"), kind != "check") |>
    mutate(rx = case_when(kind == "person" ~ map_chr(pattern, person_rx), TRUE ~ pattern),
           repl = if_else(kind %in% c("person", "person_rx"), "[PERSON]", ph),
           ord = match(kind, c("name", "person", "person_rx", "abbrev"))) |>
    arrange(ord, row)
  bind_rows(al |> select(kind, pattern, rx, repl), tibble(kind = "url", pattern = "URL / e-mail", rx = URL_RX, repl = "[URL]"))
}
redact <- function(text, party, lang) {
  rules <- redaction_rules(party, lang)
  n <- integer(nrow(rules)); kwic <- list()
  for (k in seq_len(nrow(rules))) {
    loc <- stri_locate_all_regex(text, rules$rx[k], omit_no_match = TRUE)[[1]]
    n[k] <- nrow(loc)
    if (n[k]) {
      kwic[[k]] <- tibble(kind = rules$kind[k], pattern = rules$pattern[k],
                          match = stri_sub(text, loc[, 1], loc[, 2]),
                          context = stri_sub(text, pmax(1, loc[, 1] - 40), pmin(stri_length(text), loc[, 2] + 40)))
      text <- stri_replace_all_regex(text, rules$rx[k], rules$repl[k])
    }
  }
  list(text = text, counts = rules |> mutate(n = n) |> select(kind, pattern, n), kwic = bind_rows(kwic))
}
residuals_of <- function(text, party, lang) {
  ck <- ALIASES |> filter(.data$party == !!party, kind == "check") |> pull(pattern)
  loc <- stri_locate_all_regex(text, ck, omit_no_match = TRUE)[[1]]
  if (!nrow(loc)) return(tibble(match = character(), context = character()))
  tibble(match = stri_sub(text, loc[, 1], loc[, 2]),
         context = stri_sub(text, pmax(1, loc[, 1] - 50), pmin(stri_length(text), loc[, 2] + 50)))
}

# ============================================================
# MODE: prepare  (de_red always; en_red for every document with a translation)
# ============================================================
if (MODE == "prepare") {
  reg <- read_register()
  stopifnot(all(reg$party %in% ALIASES$party))
  write_csv_atomic(ALIASES |> mutate(rx_used = if_else(kind == "person", map_chr(pattern, person_rx), pattern)),
                   file.path(LK_DIR, "aliases.csv"))
  counts <- list(); kw <- list(); resid <- list()
  for (arm in c("de_red", "en_red")) {
    src <- c(de_red = "de_orig", en_red = "en_orig")[[arm]]; lang <- substr(arm, 1, 2)
    for (i in seq_len(nrow(reg))) {
      lk <- reg$lk_id[i]
      if (!file.exists(text_path(src, lk))) next
      r <- redact(read_text(src, lk), reg$party[i], lang)
      write_text_atomic(r$text, text_path(arm, lk))
      counts[[length(counts) + 1]] <- r$counts |> mutate(arm = arm, lk_id = lk, party = reg$party[i])
      if (nrow(r$kwic)) kw[[length(kw) + 1]] <- r$kwic |> mutate(arm = arm, lk_id = lk)
      rs <- residuals_of(r$text, reg$party[i], lang)
      if (nrow(rs)) resid[[length(resid) + 1]] <- rs |> mutate(arm = arm, lk_id = lk)
    }
  }
  counts <- bind_rows(counts)
  resid <- bind_rows(resid)
  if (!nrow(resid)) resid <- tibble(arm = character(), lk_id = character(), match = character(), context = character())
  per_doc <- counts |> group_by(arm, lk_id, party) |>
    summarise(n_party = sum(n[kind %in% c("name", "abbrev")]), n_person = sum(n[kind %in% c("person", "person_rx")]),
              n_url = sum(n[kind == "url"]), n_total = sum(n), .groups = "drop") |>
    left_join(resid |> count(arm, lk_id, name = "n_residual"), by = c("arm", "lk_id")) |>
    mutate(n_residual = coalesce(n_residual, 0L)) |>
    left_join(reg |> select(lk_id, doc_id, partyabbrev, countryname), by = "lk_id") |> arrange(arm, lk_id)
  write_csv_atomic(per_doc, file.path(LK_DIR, "redaction_counts.csv"))
  write_csv_atomic(counts |> filter(n > 0), file.path(LK_DIR, "redaction_counts_by_pattern.csv"))
  write_csv_atomic(bind_rows(kw), file.path(LK_DIR, "redaction_kwic.csv"))            # restricted: manifesto text
  write_csv_atomic(resid |> select(arm, lk_id, match, context), file.path(LK_DIR, "residual_mentions.csv"))
  print(as.data.frame(per_doc |> select(arm, lk_id, partyabbrev, n_party, n_person, n_url, n_residual)))
  message(sprintf("redaction: %d documents in %s; residual mentions: %d (see residual_mentions.csv)",
                  n_distinct(paste(per_doc$arm, per_doc$lk_id)), paste(unique(per_doc$arm), collapse = ", "), sum(per_doc$n_residual)))
  quit(save = "no")
}

# ============================================================
# API stages: production definitions loaded exactly as in R/52 (R/32 helpers, R/15b/16/18 scorers)
# ============================================================
if (MODE %in% c("translate", "score", "finalize", "recognise")) {
  source("R/1_Functions.R")
  source("R/3c_Populism_v4_safe.R")
  source("R/12_Validate_Span_Quality.R")
  UPD_DIR   <- LK_DIR                                  # R/32 wrappers write STOP/ABORT files here
  USAGE_DIR <- file.path(LK_DIR, "usage_logs", "misc") # set per task below
  CHECKPOINT_EVERY_UPD <- 10L
  WORKERS <- c(claude = 2L, gpt41mini = 3L, gemini = 2L)
  if (nzchar(Sys.getenv("CLAUDE_WORKERS"))) WORKERS[["claude"]] <- as.integer(Sys.getenv("CLAUDE_WORKERS"))
  if (nzchar(Sys.getenv("GEMINI_WORKERS"))) WORKERS[["gemini"]] <- as.integer(Sys.getenv("GEMINI_WORKERS"))
  r32 <- parse("R/32_CMP2026a_Score.R", keep.source = FALSE)
  eval_defs <- function(exprs, keep) {
    got <- character()
    for (e in exprs) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
                         as.character(e[[2]]) %in% keep) { eval(e, envir = globalenv()); got <- c(got, as.character(e[[2]])) }
    miss <- setdiff(keep, got); if (length(miss)) stop("R/32 definitions not found: ", paste(miss, collapse = ", "))
    invisible(got)
  }
  eval_defs(r32, c("load_defs", "CHUNK_CONSTS", "chunk_fingerprint"))
  load_defs("R/15b_Claude_Full_Corpus.R",
            c(CHUNK_CONSTS, "MODEL_CLAUDE", "anthropic_v4_schema", "anthropic_v4_chat",
              "score_chunk_v4_claude", "decode_dim_string", "flatten_parsed_v4_claude", "score_manifesto_v4_claude"))
  fp_claude <- chunk_fingerprint()
  load_defs("R/16_Score_Full_Corpus_gpt41mini.R",
            c(CHUNK_CONSTS, "MODEL", "score_chunk_v4_robust", "flatten_parsed_v4", "score_manifesto_v4_robust"))
  fp_gpt <- chunk_fingerprint()
  load_defs("R/18_Score_Full_Corpus_Gemini_flash_latest.R",
            c(CHUNK_CONSTS, "MODEL_GEMINI", "to_gemini_schema", "gemini_v4_schema",
              "gemini_v4_chat", "score_chunk_v4_gemini", "flatten_parsed_v4_gemini", "score_manifesto_v4_gemini"))
  stopifnot("chunking config must be identical across production scripts" =
              fp_claude == fp_gpt && fp_gpt == chunk_fingerprint())
  GEMINI_BACKEND <- Sys.getenv("GEMINI_BACKEND", "vertex")
  MODEL_GEMINI   <- Sys.getenv("GEMINI_MODEL", "gemini-3.5-flash")
  EXPECT_GEMINI  <- Sys.getenv("EXPECT_GEMINI", MODEL_GEMINI)
  MODEL_ID <- c(claude = MODEL_CLAUDE, gpt41mini = MODEL, gemini = MODEL_GEMINI)
  TARGET <- "leakage"; SUFFIX <- ""
  ABORT_FILE <- file.path(UPD_DIR, "ABORT_gemini_model_version")
  eval_defs(r32, c("STOP_FILE", "BILLING_PAT", "ensure_model_trace", "traced_call", "doc_of", "log_usage",
                   "guarded_call", "anthropic_v4_chat_prod", "anthropic_v4_chat", "oa_chat_json_prod", "oa_chat_json",
                   "VERTEX_URL", "VERTEX_KEY", "vertex_token", "gemini_v4_chat_vertex", "gemini_v4_chat_prod",
                   "gemini_v4_chat", "CHUNK_FUNS", "score_selected_chunks", "SCORER", "COMMON_GLOBALS",
                   "score_rows", "latest_attempt", "ok_docs_of", "read_ckpts"))
  set_usage_dir <- function(task) { USAGE_DIR <<- file.path(LK_DIR, "usage_logs", task); ensure_dir(USAGE_DIR) }
  reg <- read_register()
}

# ============================================================
# MODE: translate  (de_orig -> en_orig; one Claude call per document, resumable)
# ============================================================
TRANSLATE_MODEL <- "claude-sonnet-4-6"
TRANSLATE_SYS <- paste(
  "You are a professional translator of political documents.",
  "Translate the German text that the user sends into English.",
  "Translate faithfully and completely, sentence by sentence: do not summarise, shorten, omit, add, explain or comment on anything.",
  "Keep the order, structure, headings, lists, numbers and punctuation of the original.",
  "Translate the names of organisations as an English-language translator normally would, and leave personal names unchanged.",
  "Output only the translation, with no preface and no notes.")
if (MODE == "translate") {
  set_usage_dir("translate")
  log_file <- file.path(LK_DIR, "translations.csv")
  todo <- reg |> filter(!file.exists(text_path("en_orig", lk_id)))
  message(sprintf("[translate | %s] to translate: %d of %d", TRANSLATE_MODEL, nrow(todo), nrow(reg)))
  check_budget("translate")
  translate_one <- function(lk) {
    text <- read_text("de_orig", lk)
    call <- function() {
      body <- list(model = TRANSLATE_MODEL, max_tokens = 32000L, temperature = 0, system = TRANSLATE_SYS,
                   messages = list(list(role = "user", content = text)))
      req <- httr::POST("https://api.anthropic.com/v1/messages",
                        httr::add_headers("x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"), "anthropic-version" = "2023-06-01",
                                          "content-type" = "application/json"),
                        body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "raw", httr::timeout(1500))
      if (httr::status_code(req) >= 400) stop("Anthropic API error: ", httr::content(req, "text", encoding = "UTF-8"))
      httr::content(req, "parsed", type = "application/json")
    }
    for (attempt in 1:3) {
      t <- tryCatch(guarded_call("anthropic", call), error = function(e) e)
      if (!inherits(t, "error")) break
      if (grepl("STOP", conditionMessage(t))) stop(t)
      Sys.sleep(10 * attempt)
    }
    if (inherits(t, "error")) return(tibble(lk_id = lk, ok = FALSE, error = substr(conditionMessage(t), 1, 300)))
    log_usage("anthropic", TRANSLATE_MODEL, t$r$usage, t$resolved, lk)
    out <- paste(vapply(t$r$content, function(b) if (identical(b$type, "text")) b$text else "", ""), collapse = "")
    ok <- identical(t$r$stop_reason, "end_turn") && nzchar(out)
    if (ok) write_text_atomic(out, text_path("en_orig", lk))
    tibble(lk_id = lk, ok = ok, error = if (ok) NA_character_ else paste("stop_reason", t$r$stop_reason %||% "?"),
           model_requested = TRANSLATE_MODEL, model_resolved = t$resolved, stop_reason = t$r$stop_reason %||% NA_character_,
           in_tok = t$r$usage$input_tokens %||% NA, out_tok = t$r$usage$output_tokens %||% NA,
           translated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  }
  plan(multisession, workers = as.integer(Sys.getenv("TRANSLATE_WORKERS", "3")))
  res <- future_map_dfr(todo$lk_id, translate_one, .options = furrr_options(
    globals = c("translate_one", "read_text", "text_path", "write_text_atomic", "LK_DIR", "TRANSLATE_MODEL", "TRANSLATE_SYS",
                "guarded_call", "traced_call", "ensure_model_trace", "STOP_FILE", "BILLING_PAT", "UPD_DIR", "ABORT_FILE",
                "log_usage", "USAGE_DIR", "%||%"),
    packages = c("tibble", "httr", "jsonlite", "stringi"), seed = TRUE))
  plan(sequential)
  if (nrow(res)) {
    prev <- if (file.exists(log_file)) read_csv(log_file, col_types = cols(.default = "c")) else NULL
    write_csv_atomic(bind_rows(prev, res |> mutate(across(everything(), as.character))) |>
                       group_by(lk_id) |> slice_tail(n = 1) |> ungroup() |> arrange(lk_id), log_file)
    print(as.data.frame(res |> select(lk_id, ok, model_resolved, stop_reason, in_tok, out_tok)))
  }
  # single-chunk check for the translations (production sends a text <= FULL_TEXT_TOKEN_CAP whole)
  have <- reg |> filter(file.exists(text_path("en_orig", lk_id)))
  tk <- map_dbl(have$lk_id, ~ estimate_tokens(read_text("en_orig", .x)))
  message(sprintf("translations: %d / %d; o200k tokens en/de: median ratio %.2f; max en tokens %d (cap %d)",
                  nrow(have), nrow(reg), median(tk / have$tok_o200k), max(c(0, tk)), FULL_TEXT_TOKEN_CAP))
  quit(save = "no")
}

# ============================================================
# MODE: score / finalize  (as R/52; per arm and model)
# ============================================================
if (MODE %in% c("score", "finalize")) {
  ARM <- args[2]; WHICH <- args[3]
  stopifnot(ARM %in% ARMS, WHICH %in% names(SCORER))
  # orig arms: the real CMP doc_id in the prompt, exactly as in production; red arms: neutral LKnn
  prompt_id <- function(r) if (ARM %in% c("de_orig", "en_orig")) r$doc_id else r$lk_id
  rows_all <- reg |> filter(file.exists(text_path(ARM, lk_id))) |>
    mutate(doc_id = prompt_id(pick(everything())), text = map_chr(lk_id, ~ read_text(ARM, .x))) |>
    select(lk_id, doc_id, countryname, date, text)
  if (nrow(rows_all) < nrow(reg)) message(sprintf("  note: %d documents have no %s text yet", nrow(reg) - nrow(rows_all), ARM))
  ckpt_dir <- file.path(LK_DIR, "checkpoints", paste0(ARM, "_", WHICH))
  set_usage_dir(paste0("score_", ARM, "_", WHICH))
}
if (MODE == "score") {
  ensure_dir(ckpt_dir)
  prev <- read_ckpts()
  done <- ok_docs_of(bind_rows(map(prev, "raw")))
  todo <- rows_all |> filter(!doc_id %in% done) |> select(doc_id, countryname, date, text)
  message(sprintf("[%s | %s | %s | %s] to score: %d (already ok: %d)", ARM, WHICH, MODEL_ID[[WHICH]],
                  if (WHICH == "gemini") GEMINI_BACKEND else "production endpoint", nrow(todo), length(done)))
  b0 <- length(prev)
  n_b <- ceiling(nrow(todo) / CHECKPOINT_EVERY_UPD)
  for (b in seq_len(n_b)) {
    check_budget(sprintf("score %s %s batch %d", ARM, WHICH, b))
    rows <- todo[((b - 1) * CHECKPOINT_EVERY_UPD + 1):min(b * CHECKPOINT_EVERY_UPD, nrow(todo)), ]
    t0 <- Sys.time()
    res <- score_rows(rows, WHICH, WORKERS[[WHICH]])
    batch_result <- list(raw = bind_rows(map(res, "raw")), spans = bind_rows(map(res, "spans")),
                         errors = unlist(map(res, "errors")), timestamp = Sys.time(), model = MODEL_ID[[WHICH]])
    ck_final <- sprintf("%s/batch_%04d.RData", ckpt_dir, b0 + b); ck_tmp <- paste0(ck_final, ".tmp")
    save(batch_result, file = ck_tmp); stopifnot(file.rename(ck_tmp, ck_final))
    message(sprintf("  batch %d/%d: %d docs, %d chunk rows, %d errors, %.1f min, spent so far USD %.2f", b, n_b, nrow(rows),
                    nrow(batch_result$raw), length(batch_result$errors), as.numeric(Sys.time() - t0, units = "mins"), spent_usd()))
    if (length(batch_result$errors)) message("  first error: ", substr(batch_result$errors[1], 1, 300))
    if (file.exists(ABORT_FILE) && WHICH == "gemini") { message("  ABORT: ", readLines(ABORT_FILE)[1]); quit(save = "no", status = 4) }
    prov <- c(claude = "anthropic", gpt41mini = "openai", gemini = "google")[[WHICH]]
    if (file.exists(STOP_FILE(prov)) || any(grepl(BILLING_PAT, batch_result$errors, ignore.case = TRUE))) {
      message("  STOP: persistent quota/billing errors. Re-run later to resume."); quit(save = "no", status = 3)
    }
  }
  MODE <- "finalize"
}
if (MODE == "finalize") {
  ck <- read_ckpts()
  raw_all <- bind_rows(map(ck, "raw")); spans_all <- bind_rows(map(ck, "spans"))
  ok <- ok_docs_of(raw_all)
  last_b <- latest_attempt(raw_all) |> distinct(doc_id_root, .batch)
  raw_all <- raw_all |> semi_join(last_b, by = c("doc_id_root", ".batch")) |> filter(doc_id_root %in% ok) |> select(-.batch)
  spans_all <- spans_all |> semi_join(last_b, by = c("doc_id_root", ".batch")) |> filter(doc_id_root %in% ok) |> select(-.batch)
  raw_all$model_id <- unique(map_chr(ck, ~ .x$model %||% NA_character_)) |> paste(collapse = ";")
  prov <- c(claude = "anthropic", gpt41mini = "openai", gemini = "google")[[WHICH]]
  mv <- list.files(USAGE_DIR, "^usage_v2_", full.names = TRUE) |>
    map_dfr(~ read_csv(.x, col_types = cols(.default = "c"))) |>
    filter(provider %in% c(prov, paste0(prov, "-vertex")), model == MODEL_ID[[WHICH]], !is.na(doc_id)) |>
    arrange(ts) |> group_by(doc_id) |> slice_tail(n = 1) |> ungroup() |>
    select(doc_id, model_resolved = resolved_model, endpoint = provider, scored_at = ts)
  key <- rows_all |> select(lk_id, doc_id_root = doc_id)
  raw_all <- raw_all |> left_join(mv, by = "doc_id") |> left_join(key, by = "doc_id_root") |>
    mutate(source_run = "leakage_oos_2026-11", scoring_target = TARGET, arm = ARM, model_requested = MODEL_ID[[WHICH]])
  spans_all <- spans_all |> left_join(key, by = "doc_id_root") |> mutate(arm = ARM)
  message("  resolved model versions: ", paste(names(table(raw_all$model_resolved, useNA = "ifany")),
                                             table(raw_all$model_resolved, useNA = "ifany"), sep = "=", collapse = " | "))
  ensure_dir(file.path(LK_DIR, "scores"))
  write_atomic(raw_all, file.path(LK_DIR, "scores", sprintf("poplib_v4_%s_%s_raw.parquet", ARM, WHICH)))
  message(sprintf("[%s %s] ok documents: %d / %d | chunk rows: %d | spans: %d | not ok: %s", ARM, WHICH, length(ok),
                  nrow(rows_all), nrow(raw_all), nrow(spans_all), paste(setdiff(rows_all$doc_id, ok), collapse = ", ")))
  # R/12 span validation, called exactly as in R/52 / R/32
  chunk_text_lookup <- setNames(spans_all$chunk_text, spans_all$doc_id)
  sp_for_check <- system_prompt_v4_safe("X", 2020L)
  validated <- validate_spans_table_with_judge(
    spans_all |> select(doc_id, doc_id_root, dim_name, quote, context),
    chunk_text_lookup, sp_for_check, TOPIC_KEYWORDS_V4, use_judge = TRUE, verbose = FALSE)
  validated <- validated |> left_join(key, by = "doc_id_root") |> mutate(arm = ARM)
  write_atomic(validated, file.path(LK_DIR, "scores", sprintf("poplib_v4_%s_%s_validated_spans.parquet", ARM, WHICH)))
  message(sprintf("  saved raw + validated spans for %s %s; spent so far USD %.2f", ARM, WHICH, spent_usd()))
  quit(save = "no")
}

# ============================================================
# MODE: recognise  (new minimal prompt; arms de_orig = ceiling, de_red, en_red)
# ============================================================
RECOG_ARMS <- c("de_orig", "de_red", "en_red")
RECOG_SYS <- paste(
  "You are an expert on European party politics and election manifestos.",
  "The user sends the text of a party manifesto (an election programme).",
  "Identify the political party that published it, the country, and the year of the national election for which it was written.",
  "Some names in the text may have been replaced by placeholders such as [PARTEI], [PARTY], [PERSON] or [URL].",
  "Always give your single best guess, even if you are uncertain.",
  "Reply with JSON only, in exactly this form:",
  "{\"party_name\": \"full party name\", \"party_abbreviation\": \"abbreviation, or empty string\",",
  "\"country\": \"country name in English\", \"election_year\": 1990, \"confidence\": 50}",
  "where confidence (0-100) is your probability, in percent, that the party is correct.")
RECOG_SCHEMA_OA <- list(type = "object", additionalProperties = FALSE,
  required = list("party_name", "party_abbreviation", "country", "election_year", "confidence"),
  properties = list(party_name = list(type = "string"), party_abbreviation = list(type = "string"),
                    country = list(type = "string"), election_year = list(type = "integer"),
                    confidence = list(type = "integer")))
RECOG_SCHEMA_GEM <- list(type = "OBJECT",
  required = list("party_name", "party_abbreviation", "country", "election_year", "confidence"),
  properties = list(party_name = list(type = "STRING"), party_abbreviation = list(type = "STRING"),
                    country = list(type = "STRING"), election_year = list(type = "INTEGER"),
                    confidence = list(type = "INTEGER")))
if (MODE == "recognise") {
  WHICH <- args[2]; stopifnot(WHICH %in% MODELS)
  set_usage_dir(paste0("recog_", WHICH))
  recog_claude <- function(text) {
    # forced tool call with a JSON schema (as production calls Claude); without it Claude reasons in
    # prose before the JSON and runs into the token limit
    tool <- list(name = "submit_recognition", description = "Submit the party, country and election year you identify.",
                 input_schema = RECOG_SCHEMA_OA[c("type", "required", "properties")])
    body <- list(model = MODEL_CLAUDE, max_tokens = 400L, temperature = 0, system = RECOG_SYS,
                 tools = list(tool), tool_choice = list(type = "tool", name = "submit_recognition"),
                 messages = list(list(role = "user", content = paste0("TEXT:\n", text))))
    req <- httr::POST("https://api.anthropic.com/v1/messages",
                      httr::add_headers("x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"), "anthropic-version" = "2023-06-01",
                                        "content-type" = "application/json"),
                      body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "raw", httr::timeout(300))
    if (httr::status_code(req) >= 400) stop("Anthropic API error: ", httr::content(req, "text", encoding = "UTF-8"))
    cont <- httr::content(req, "parsed", type = "application/json")
    tu <- NULL
    for (bl in cont$content) if (identical(bl$type, "tool_use")) { tu <- bl; break }
    if (is.null(tu)) stop("Claude did not return a tool_use block.")
    list(text = as.character(jsonlite::toJSON(tu$input, auto_unbox = TRUE, null = "null")), usage = cont$usage)
  }
  recog_gemini <- function(text) {
    body <- list(systemInstruction = list(parts = list(list(text = RECOG_SYS))),
                 contents = list(list(role = "user", parts = list(list(text = paste0("TEXT:\n", text))))),
                 generationConfig = list(temperature = 0, maxOutputTokens = 400L, responseMimeType = "application/json",
                                         responseSchema = RECOG_SCHEMA_GEM, thinkingConfig = list(thinkingBudget = 0)))
    req <- httr::POST(sprintf(VERTEX_URL, MODEL_GEMINI), httr::add_headers(Authorization = paste("Bearer", vertex_token())),
                      httr::content_type_json(), body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"), httr::timeout(300))
    if (httr::status_code(req) >= 400) stop("Gemini API error: ", httr::content(req, "text", encoding = "UTF-8"))
    cont <- httr::content(req, "parsed", type = "application/json")
    list(text = cont$candidates[[1]]$content$parts[[1]]$text %||% "", usage = cont$usageMetadata)
  }
  parse_recog <- function(txt) {
    j <- regmatches(txt, regexpr("\\{[\\s\\S]*\\}", txt, perl = TRUE))
    p <- tryCatch(jsonlite::fromJSON(j), error = function(e) NULL)
    if (is.null(p) || !is.list(p) || is.null(p$party_name) || !nzchar(p$party_name)) return(list(parse_ok = FALSE))
    c(list(parse_ok = TRUE), lapply(p[c("party_name", "party_abbreviation", "country", "election_year", "confidence")],
                                     function(v) if (is.null(v)) NA else v))
  }
  todo <- tidyr::expand_grid(arm = RECOG_ARMS, lk_id = reg$lk_id) |>
    mutate(out = file.path(LK_DIR, "recognition", paste0(arm, "_", WHICH), paste0(lk_id, ".json"))) |>
    filter(!file.exists(out), file.exists(text_path(arm, lk_id)))
  message(sprintf("[recognise | %s | %s] calls to make: %d", WHICH, MODEL_ID[[WHICH]], nrow(todo)))
  for (i in seq_len(nrow(todo))) {
    if (i %% 10 == 1) check_budget(sprintf("recognise %s call %d", WHICH, i))
    a <- todo$arm[i]; lk <- todo$lk_id[i]; text <- read_text(a, lk); doc <- paste0(a, ":", lk)
    t <- NULL
    for (attempt in 1:4) {
      t <- tryCatch(switch(WHICH,
        claude    = guarded_call("anthropic", recog_claude, text),
        gpt41mini = guarded_call("openai", oa_chat_json_prod, system_prompt = RECOG_SYS, user_prompt = paste0("TEXT:\n", text),
                                 model = MODEL, json_schema = RECOG_SCHEMA_OA, max_output_tokens = 400),
        gemini    = guarded_call("google", recog_gemini, text)), error = function(e) e)
      if (!inherits(t, "error")) break
      if (grepl("STOP|ABORT", conditionMessage(t))) stop(t)
      Sys.sleep(5 * attempt)
    }
    if (inherits(t, "error")) { message("  failed ", doc, ": ", substr(conditionMessage(t), 1, 200)); next }
    prov <- c(claude = "anthropic", gpt41mini = "openai", gemini = "google-vertex")[[WHICH]]
    log_usage(prov, MODEL_ID[[WHICH]], t$r$usage, t$resolved, doc, t$r$system_fingerprint %||% NA_character_)
    if (WHICH == "gemini" && nzchar(EXPECT_GEMINI) && !identical(t$resolved, EXPECT_GEMINI)) {
      writeLines(paste("served", t$resolved, "expected", EXPECT_GEMINI), ABORT_FILE)
      stop("ABORT: Gemini served ", t$resolved, " instead of ", EXPECT_GEMINI)
    }
    rec <- c(list(arm = a, lk_id = lk, model = WHICH, model_requested = MODEL_ID[[WHICH]], model_resolved = t$resolved,
                  raw_text = t$r$text, called_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")), parse_recog(t$r$text))
    dir.create(dirname(todo$out[i]), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(todo$out[i], ".tmp"); writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", na = "null"), tmp, useBytes = TRUE)
    stopifnot(file.rename(tmp, todo$out[i]))
  }
  message(sprintf("[recognise %s] done; spent so far USD %.2f", WHICH, spent_usd()))
  quit(save = "no")
}

# ============================================================
# Recognition: answer-matching rules (case-insensitive, applied to "party_name | party_abbreviation").
# Narrower than the redaction aliases so that a rival party never counts (e.g. the CVP rule does not
# match "Swiss People's Party"). A party hit also needs the right country.
# ============================================================
RECOG_MATCH <- tribble(
  ~party,  ~rx,
  "41220", "\\bKPD\\b|communist party|kommunistische partei",
  "41522", "zentrum|cent(re|er) party",
  "41711", "\\bWAV\\b|aufbau|reconstruction",
  "41712", "\\bDRP\\b|rechts-?partei|right(s)? party|reich(s)?[- ]?partei|reich party|german right",
  "41521", "\\bCDU\\b|\\bCSU\\b|christian democratic union|christian social union|christlich[- ]demokratische union|christlich[- ]soziale union",
  "41420", "\\bF\\.?D\\.?P\\b|free democratic|freie demokratische",
  "41320", "\\bSPD\\b|social democratic party|sozialdemokratische partei",
  "41953", "\\bAfD\\b|alternative f(ü|u)r deutschland|alternative for germany",
  "42320", "\\bSP(Ö|O|OE)\\b|socialist party|social democratic party|sozialistische partei|sozialdemokratische partei",
  "42420", "\\bFP(Ö|O|OE)\\b|freedom party|freiheitliche",
  "42520", "\\b(Ö|O|OE)VP\\b|people['’]?s party|volkspartei",
  "42110", "gr(ü|ue)ne|green",
  "42120", "pilz|\\bjetzt\\b",
  "43520", "\\b(K?CVP|KK|PDC)\\b|christ\\w*[- ]?(democratic|social|demokratische|soziale)\\w*.*(people|volks)|conservative.*christian|catholic.*conservative|katholisch-konservativ|konservative volkspartei|conservative people",
  "43320", "\\bSPS?\\b|\\bPSS\\b|social democratic party|sozialdemokratische partei",
  "43810", "\\bSVP\\b|\\bUDC\\b|swiss people['’]?s party|schweizerische volkspartei|\\bBGB\\b|farmers",
  "43110", "gr(ü|ue)ne|green|\\bGPS\\b",
  "43220", "\\bPdA|\\bPdT|party of labou?r|partei der arbeit|labou?r party|workers",
  "43420", "\\bFDP\\b|\\bPLR\\b|free democratic|freisinn|radical[- ]democratic|the liberals|die liberalen")
norm_country <- function(x) case_when(
  str_detect(coalesce(x, ""), "(?i)german|deutschland|\\bFRG\\b|\\bBRD\\b") ~ "Germany",
  str_detect(coalesce(x, ""), "(?i)austria|österreich") ~ "Austria",
  str_detect(coalesce(x, ""), "(?i)switzerland|swiss|schweiz") ~ "Switzerland",
  TRUE ~ coalesce(x, NA_character_))
read_recognition <- function(reg) {
  fs <- list.files(file.path(LK_DIR, "recognition"), "\\.json$", recursive = TRUE, full.names = TRUE)
  if (!length(fs)) return(NULL)
  map_dfr(fs, function(f) { j <- jsonlite::fromJSON(f)
    tibble(arm = j$arm, lk_id = j$lk_id, model = j$model, model_resolved = j$model_resolved %||% NA_character_,
           parse_ok = isTRUE(j$parse_ok), party_name = as.character(j$party_name %||% NA), party_abbreviation = as.character(j$party_abbreviation %||% NA),
           country = as.character(j$country %||% NA), election_year = suppressWarnings(as.integer(j$election_year %||% NA)),
           confidence = suppressWarnings(as.numeric(j$confidence %||% NA))) }) |>
    left_join(reg |> transmute(lk_id, party, true_country = countryname, true_year = as.integer(format(date, "%Y"))), by = "lk_id") |>
    left_join(RECOG_MATCH, by = "party") |>
    mutate(answer = paste(coalesce(party_name, ""), "|", coalesce(party_abbreviation, "")),
           country_hit = norm_country(country) == true_country,
           party_hit = parse_ok & stri_detect_regex(answer, paste0("(?i)", rx)) & country_hit,
           year_hit = !is.na(election_year) & election_year == true_year,
           year_hit1 = !is.na(election_year) & abs(election_year - true_year) <= 1) |>
    select(-rx)
}

# ============================================================
# MODE: budget  (spend from the usage logs; projection of the full design from the benchmark)
# Per-call input tokens are projected as b * (o200k tokens of the text sent), b = ratio of logged
# input tokens to text tokens per model and task type (prompt overhead included); output tokens: mean per call (scoring, recognition) or a multiple of the input text
# (translation). The R/12 judge (gpt-4o-mini) is projected per scored document.
# ============================================================
if (MODE == "budget") {
  source("R/1_Functions.R")
  reg <- read_csv(reg_file, col_types = cols(.default = "c")) |> mutate(tok_o200k = as.numeric(tok_o200k))
  u <- read_usage()
  if (!nrow(u)) { message("no usage logged yet"); quit(save = "no") }
  u <- u |> mutate(type = case_when(task == "translate" ~ "translate", str_starts(task, "score_") ~ "score",
                                    str_starts(task, "recog_") ~ "recog", TRUE ~ "other"),
                   arm = case_when(type == "score" ~ str_match(task, "^score_(de_orig|de_red|en_orig|en_red)_")[, 2],
                                   type == "recog" ~ sub(":.*", "", doc_id), TRUE ~ "de_orig"),
                   lk_id = case_when(type == "recog" ~ sub(".*:", "", doc_id), type == "translate" ~ doc_id, TRUE ~ NA_character_),
                   mfam = case_when(str_detect(model, "^claude") ~ "claude", str_detect(model, "^gpt-4\\.1") ~ "gpt41mini",
                                    str_detect(model, "^gemini") ~ "gemini", str_detect(model, "^gpt-4o-mini") ~ "judge"))
  # scoring rows carry the prompt doc id: map to LKnn
  root <- sub("_ch\\d+$", "", u$doc_id)
  u <- u |> mutate(lk_id = coalesce(lk_id, reg$lk_id[match(root, reg$doc_id)], if_else(root %in% reg$lk_id, root, NA_character_)))
  tok_of <- function(arm, lk) { p <- text_path(arm, lk); if (file.exists(p)) estimate_tokens(read_text(arm, lk)) else NA_real_ }
  calls <- u |> filter(mfam != "judge", !is.na(lk_id)) |> mutate(tok_text = map2_dbl(arm, lk_id, tok_of))
  en_ratio <- { tr <- calls |> filter(type == "score", arm == "en_orig") |> distinct(lk_id, tok_text) |>
    left_join(reg |> select(lk_id, tok_o200k), by = "lk_id"); if (nrow(tr)) median(tr$tok_text / tr$tok_o200k) else 0.85 }
  fit <- calls |> group_by(type, mfam) |>
    group_modify(~ { x <- .x$tok_text; y <- .x$in_tok
      b <- sum(y) / sum(x); a <- 0     # pure ratio (system prompt overhead scales with it; stable with 2 documents)
      tibble(a = a, b = b, out_call = mean(.x$out_tok + .x$thought_tok), out_ratio = mean((.x$out_tok + .x$thought_tok) / x), n = nrow(.x)) }) |> ungroup()
  print(as.data.frame(fit))
  price <- function(m) { p <- which(str_detect(MODEL_ID_BUDGET[[m]], PRICES$pattern))[1]; c(PRICES$usd_in[p], PRICES$usd_out[p]) }
  MODEL_ID_BUDGET <- c(claude = "claude-sonnet-4-6", gpt41mini = "gpt-4.1-mini-2025-04-14", gemini = "gemini-3.5-flash")
  tok_full <- tidyr::expand_grid(arm = ARMS, lk_id = reg$lk_id) |>
    left_join(reg |> select(lk_id, tok_o200k), by = "lk_id") |>
    mutate(tok = if_else(str_starts(arm, "en"), tok_o200k * en_ratio, tok_o200k) * if_else(str_ends(arm, "red"), 0.995, 1))
  proj_one <- function(type_, m, tok) {
    f <- fit |> filter(type == type_, mfam == m)
    if (!nrow(f)) f <- fit |> filter(type == "score", mfam == m) |> mutate(out_call = if (type_ == "recog") 80 else out_call)
    if (!nrow(f)) return(NA_real_)
    pr <- price(m); inp <- f$a + f$b * tok
    out <- if (type_ == "translate") f$out_ratio * tok else f$out_call
    (inp * pr[1] + out * pr[2]) / 1e6
  }
  judge_per_doc <- { j <- u |> filter(mfam == "judge"); nd <- n_distinct(calls$lk_id[calls$type == "score"] |> paste(calls$arm[calls$type == "score"], calls$mfam[calls$type == "score"]))
    if (nrow(j) && nd) sum(j$usd) / nd else 0.002 }
  done_key <- calls |> distinct(type, mfam, arm, lk_id) |> mutate(done = TRUE)
  full <- bind_rows(
    tidyr::expand_grid(type = "score", mfam = MODELS, tok_full),
    tidyr::expand_grid(type = "recog", mfam = MODELS, tok_full |> filter(arm %in% RECOG_ARMS)),
    tibble(type = "translate", mfam = "claude", arm = "de_orig", lk_id = reg$lk_id, tok = reg$tok_o200k)) |>
    left_join(done_key, by = c("type", "mfam", "arm", "lk_id")) |> mutate(done = coalesce(done, FALSE)) |>
    mutate(usd = pmap_dbl(list(type, mfam, tok), proj_one) + if_else(type == "score", judge_per_doc, 0))
  spent <- sum(u$usd, na.rm = TRUE); remaining <- sum(full$usd[!full$done], na.rm = TRUE)
  summ <- full |> group_by(type, mfam) |> summarise(calls = n(), done = sum(done), usd_full_design = sum(usd), .groups = "drop")
  print(as.data.frame(summ))
  message(sprintf("spent (logged): USD %.2f | projected remaining: USD %.2f | projected total: USD %.2f | cap USD %.0f | en/de token ratio %.2f | judge per scored doc USD %.4f",
                  spent, remaining, spent + remaining, BUDGET_USD, en_ratio, judge_per_doc))
  write_csv_atomic(summ |> mutate(spent_usd = spent, projected_total_usd = spent + remaining, cap_usd = BUDGET_USD),
                   file.path(LK_DIR, "budget_projection.csv"))
  if (spent + remaining > BUDGET_USD) message("PROJECTION EXCEEDS THE CAP: do not run the full design")
  quit(save = "no")
}

# ============================================================
# MODE: analyse
#   (a) per model and dimension: mean difference, SE, MAD and correlation for
#       de_orig vs production (run-to-run noise), de_red vs de_orig, en_orig vs de_orig,
#       en_red vs de_orig, en_red vs en_orig; MAD excess over the noise MAD (paired by document)
#   (b) slope of liberalism_overall on populism_overall (OLS, 0-10 scales, HC1) per arm and model;
#       paired bootstrap (documents resampled, B = 2,000) of the ratio to de_orig
#   (c) redaction shift for documents the model recognised vs did not (Welch difference in means)
#   (d) REPORT.md with the numbers, the cost from the usage logs and the model versions served
# ============================================================
if (MODE == "analyse") {
  suppressPackageStartupMessages({ library(fixest) })
  source("R/0_Config.R")          # write_csv_det (deterministic CSV)
  source("R/1_Aggregation.R")
  suppressMessages(source("R/3c_Populism_v4_safe.R"))
  setFixest_nthreads(1)
  CSV <- file.path(LK_OUT, "csv"); TAB <- file.path(LK_OUT, "tables")
  reg <- read_register()
  STEM2AGG <- c(claude = "sonnet", gpt41mini = "gpt41mini", gemini = "gemini")
  MODEL_LAB <- c(claude = "Claude Sonnet 4.6", gpt41mini = "GPT-4.1-mini", gemini = "Gemini 3.5 Flash", all = "Mean of 3 models")
  ARM_LAB <- c(prod = "Production (stored)", de_orig = "German original (rerun)", de_red = "German redacted",
               en_orig = "English original", en_red = "English redacted")
  FOCUS <- c("populism_overall", "liberalism_overall", "lib_political", "lib_social", "lib_economic", "lib_financial_market", "IDEO")
  DIM_LAB <- c(populism_overall = "Populism", liberalism_overall = "Liberalism", lib_political = "Lib. political",
               lib_social = "Lib. social", lib_economic = "Lib. economic", lib_financial_market = "Lib. financial", IDEO = "Ideology (IDEO)")

  # span checks as in R/34 / R/53 (quote check = canonical; repaired topic regex + judge: diagnostic)
  KW_FIXED <- map(TOPIC_KEYWORDS_V4, ~ sub("\\|+$", "", .x))
  KW_FIXED$populism_overall   <- paste(KW_FIXED[c("pop_anti_elitism", "pop_people_centrism", "pop_manichean")], collapse = "|")
  KW_FIXED$liberalism_overall <- paste(KW_FIXED[c("lib_political", "lib_social", "lib_economic", "lib_financial_market")], collapse = "|")
  span_checks <- function(sp) {
    sp |>
      mutate(quote_ok = verbatim_found %in% TRUE & !(hallucination_flag %in% TRUE),
             txt = paste(coalesce(quote, ""), coalesce(context, ""))) |>
      group_by(dim_name) |>
      mutate(topic_rx = if (cur_group()$dim_name %in% names(KW_FIXED))
        stri_detect_regex(txt, KW_FIXED[[cur_group()$dim_name]], case_insensitive = TRUE) else NA) |>
      ungroup() |>
      mutate(topic_ok = topic_rx %in% TRUE | judge_topic %in% TRUE) |>
      select(doc_id, doc_id_root, dim_name, quote_ok, topic_rx, judge = judge_topic, topic_ok)
  }
  # ---- 1. Scores per arm (canonical aggregation) ---------------------------------------
  status <- list(); qrate <- list()
  arm_long <- function(arm) {
    cl <- list()
    for (m in MODELS) {
      f_raw <- file.path(LK_DIR, "scores", sprintf("poplib_v4_%s_%s_raw.parquet", arm, m))
      if (!file.exists(f_raw)) { message("  missing: ", f_raw); next }
      r  <- read_parquet(f_raw) |> filter(lk_id %in% reg$lk_id) |> mutate(doc_id = paste0(lk_id, "_ch", chunk_id), doc_id_root = lk_id)
      sp <- read_parquet(sub("_raw", "_validated_spans", f_raw)) |> filter(lk_id %in% reg$lk_id) |>
        mutate(doc_id = paste0(lk_id, "_ch", sub(".*_ch", "", doc_id)), doc_id_root = lk_id)
      ck <- span_checks(sp)
      qrate[[paste(arm, m)]] <<- tibble(arm = arm, model = m, spans = nrow(ck), quote_ok = mean(ck$quote_ok))
      cl[[m]] <- chunk_dim_long(r, ck, STEM2AGG[[m]])
    }
    cl <- bind_rows(cl)
    status[[arm]] <<- chunk_status_summary(cl) |> mutate(arm = arm, .before = 1)
    wl <- aggregate_within(cl, validation = AGG_RULES$canonical$validation)
    base <- combine_models(wl, stat = AGG_RULES$canonical$stat, between = "mean")
    pm <- wl |> transmute(model = names(STEM2AGG)[match(model, STEM2AGG)], lk_id = doc_id, dim, value = cwm)
    ideo <- pm |> filter(dim %in% c("pop_ideology_right", "pop_ideology_left")) |>
      pivot_wider(names_from = dim, values_from = value) |>
      transmute(model, lk_id, dim = "IDEO", value = ideo_direction(pop_ideology_right, pop_ideology_left))
    allm <- base |> select(lk_id = doc_id, all_of(AGG_DIMS), IDEO) |>
      pivot_longer(-lk_id, names_to = "dim", values_to = "value") |> mutate(model = "all")
    bind_rows(pm, ideo, allm) |> mutate(arm = arm)
  }
  S_arms <- map_dfr(ARMS, arm_long)
  # production scores (stored baseline, per model and cross-model)
  b <- read_parquet("data/CMP/v2026a/manifesto_scores_v2026a.parquet") |> inner_join(reg |> select(doc_id, lk_id), by = "doc_id")
  prod <- bind_rows(
    map_dfr(MODELS, function(m) {
      x <- map_dfr(AGG_DIMS, ~ tibble(lk_id = b$lk_id, dim = .x, value = b[[paste0(.x, "_", STEM2AGG[[m]])]]))
      bind_rows(x, tibble(lk_id = b$lk_id, dim = "IDEO",
                          value = ideo_direction(b[[paste0("pop_ideology_right_", STEM2AGG[[m]])]], b[[paste0("pop_ideology_left_", STEM2AGG[[m]])]]))) |>
        mutate(model = m)
    }),
    map_dfr(AGG_DIMS, ~ tibble(lk_id = b$lk_id, dim = .x, value = b[[.x]])) |>
      bind_rows(tibble(lk_id = b$lk_id, dim = "IDEO", value = b$pop_ideology_direction)) |> mutate(model = "all")) |>
    mutate(arm = "prod")
  S <- bind_rows(prod, S_arms) |> filter(!is.na(value) | TRUE) |> arrange(arm, model, dim, lk_id)
  write_csv_det(S |> select(arm, model, lk_id, dim, value), file.path(CSV, "57_scores_long.csv"))
  write_csv_det(bind_rows(status), file.path(CSV, "57_chunk_status_by_arm_model_dim.csv"))
  write_csv_det(bind_rows(qrate), file.path(CSV, "57_quote_check_rate.csv"))
  W <- S |> pivot_wider(names_from = arm, values_from = value)

  # ---- 2. (a) Differences vs the reference arms -----------------------------------------
  PAIRS <- tribble(~cmp, ~x, ~y,
    "noise",        "de_orig", "prod",
    "de_red",       "de_red",  "de_orig",
    "en_orig",      "en_orig", "de_orig",
    "en_red",       "en_red",  "de_orig",
    "en_red_en",    "en_red",  "en_orig")
  CMP_LAB <- c(noise = "Rerun vs production (noise)", de_red = "DE redacted vs DE original", en_orig = "EN original vs DE original",
               en_red = "EN redacted vs DE original", en_red_en = "EN redacted vs EN original")
  diff_stats <- function(x, y, noise_abs = NULL) {
    ok <- !is.na(x) & !is.na(y); d <- x[ok] - y[ok]; n <- length(d)
    tibble(n = n, mean_diff = if (n) mean(d) else NA_real_, se_diff = if (n > 1) sd(d) / sqrt(n) else NA_real_,
           p = if (n > 2 && sd(d) > 0) 2 * pt(-abs(mean(d) / (sd(d) / sqrt(n))), n - 1) else NA_real_,
           mad = if (n) mean(abs(d)) else NA_real_, rmsd = if (n) sqrt(mean(d^2)) else NA_real_,
           r = if (n > 2 && sd(x[ok]) > 0 && sd(y[ok]) > 0) cor(x[ok], y[ok]) else NA_real_,
           share_equal = if (n) mean(abs(d) < 1e-9) else NA_real_)
  }
  A <- pmap_dfr(PAIRS, function(cmp, x, y) {
    W |> group_by(model, dim) |> group_modify(~ diff_stats(.x[[x]], .x[[y]])) |> ungroup() |> mutate(cmp = cmp, .before = 1)
  })
  # MAD excess over noise, paired by document: mean(|x - y| - |de_orig - prod|)
  ex <- pmap_dfr(PAIRS |> filter(cmp != "noise"), function(cmp, x, y) {
    W |> group_by(model, dim) |> group_modify(~ {
      dn <- abs(.x$de_orig - .x$prod); dc <- abs(.x[[x]] - .x[[y]]); ok <- !is.na(dn) & !is.na(dc); e <- dc[ok] - dn[ok]
      tibble(mad_excess = if (length(e)) mean(e) else NA_real_, se_mad_excess = if (length(e) > 1) sd(e) / sqrt(length(e)) else NA_real_) }) |>
      ungroup() |> mutate(cmp = cmp)
  })
  A <- A |> left_join(ex, by = c("cmp", "model", "dim")) |> arrange(match(cmp, PAIRS$cmp), match(model, c(MODELS, "all")), dim)
  write_csv_det(A, file.path(CSV, "57_differences.csv"))

  # ---- 3. (b) Slopes and paired bootstrap of the ratio ----------------------------------
  slope_of <- function(x, y) { v <- var(x); if (!is.finite(v) || v == 0) NA_real_ else cov(x, y) / v }
  SL <- W |> filter(dim %in% c("populism_overall", "liberalism_overall")) |>
    pivot_longer(c(prod, all_of(ARMS)), names_to = "arm", values_to = "value") |>
    pivot_wider(names_from = dim, values_from = value)
  slopes <- SL |> group_by(model, arm) |> group_modify(~ {
    d <- .x |> filter(!is.na(populism_overall), !is.na(liberalism_overall))
    if (nrow(d) < 5) return(tibble(n = nrow(d), slope = NA_real_, se_hc1 = NA_real_))
    f <- feols(liberalism_overall ~ populism_overall, data = d, vcov = "hetero")
    tibble(n = nrow(d), slope = unname(coef(f)["populism_overall"]), se_hc1 = unname(se(f)["populism_overall"]))
  }) |> ungroup()
  B_BOOT <- 2000L
  boot <- SL |> group_by(model) |> group_modify(~ {
    wide <- .x |> select(lk_id, arm, populism_overall, liberalism_overall) |>
      pivot_wider(names_from = arm, values_from = c(populism_overall, liberalism_overall))
    map_dfr(setdiff(c("prod", ARMS), "de_orig"), function(a) {
      cols <- c(paste0(c("populism_overall_", "liberalism_overall_"), a), paste0(c("populism_overall_", "liberalism_overall_"), "de_orig"))
      d <- wide[complete.cases(wide[cols]), cols]
      if (nrow(d) < 5) return(tibble(arm = a, n_common = nrow(d)))
      est <- slope_of(d[[1]], d[[2]]) / slope_of(d[[3]], d[[4]])
      set.seed(SAMPLE_SEED)
      rb <- replicate(B_BOOT, { i <- sample.int(nrow(d), replace = TRUE)
        c(slope_of(d[[1]][i], d[[2]][i]) / slope_of(d[[3]][i], d[[4]][i]), slope_of(d[[1]][i], d[[2]][i]) - slope_of(d[[3]][i], d[[4]][i])) })
      tibble(arm = a, n_common = nrow(d), ratio = est, ratio_lo = quantile(rb[1, ], 0.025, na.rm = TRUE, names = FALSE),
             ratio_hi = quantile(rb[1, ], 0.975, na.rm = TRUE, names = FALSE),
             diff = slope_of(d[[1]], d[[2]]) - slope_of(d[[3]], d[[4]]),
             diff_lo = quantile(rb[2, ], 0.025, na.rm = TRUE, names = FALSE), diff_hi = quantile(rb[2, ], 0.975, na.rm = TRUE, names = FALSE))
    })
  }) |> ungroup()
  slopes <- slopes |> left_join(boot, by = c("model", "arm")) |>
    arrange(match(model, c(MODELS, "all")), match(arm, c("prod", ARMS)))
  write_csv_det(slopes, file.path(CSV, "57_slopes.csv"))

  # ---- 4. Recognition --------------------------------------------------------------------
  rec <- read_recognition(reg)
  write_csv_atomic(RECOG_MATCH, file.path(LK_DIR, "recognition_match_rules.csv"))
  write_csv_det(rec |> select(arm, model, lk_id, party, true_country, true_year, party_name, party_abbreviation, country,
                              election_year, confidence, party_hit, country_hit, year_hit, year_hit1, model_resolved, parse_ok),
                file.path(CSV, "57_recognition_calls.csv"))
  hit <- rec |> group_by(model, arm) |>
    summarise(n = n(), parsed = sum(parse_ok),
              conf_hit = mean(confidence[party_hit %in% TRUE], na.rm = TRUE), conf_miss = mean(confidence[party_hit %in% FALSE], na.rm = TRUE),
              party_hit = mean(party_hit), country_hit = mean(country_hit),
              year_exact = mean(year_hit), year_pm1 = mean(year_hit1), mean_conf = mean(confidence, na.rm = TRUE), .groups = "drop") |>
    relocate(conf_hit, conf_miss, .after = mean_conf) |>
    arrange(match(model, MODELS), match(arm, RECOG_ARMS))
  any_hit <- rec |> group_by(arm, lk_id) |> summarise(k = sum(party_hit), .groups = "drop")
  hit <- bind_rows(hit, any_hit |> group_by(arm) |> summarise(n = n(), party_hit = mean(k >= 2), .groups = "drop") |> mutate(model = "all (>= 2 of 3)"),
                   any_hit |> group_by(arm) |> summarise(n = n(), party_hit = mean(k >= 1), .groups = "drop") |> mutate(model = "any model"))
  write_csv_det(hit, file.path(CSV, "57_recognition_hit_rates.csv"))
  # by country and by decade for the redacted arms
  hit_by <- rec |> filter(arm != "de_orig") |> left_join(reg |> select(lk_id, countryname), by = "lk_id") |>
    mutate(period = if_else(true_year < 1990, "before 1990", "1990 and later")) |>
    group_by(model, arm, countryname, period) |> summarise(n = n(), party_hit = mean(party_hit), .groups = "drop")
  write_csv_det(hit_by, file.path(CSV, "57_recognition_by_country_period.csv"))

  # ---- 5. (c) Redaction shift by recognition -------------------------------------------
  RS_PAIRS <- tribble(~pair, ~x, ~y, ~rec_arm, "de", "de_red", "de_orig", "de_red", "en", "en_red", "en_orig", "en_red")
  recw <- rec |> select(arm, model, lk_id, party_hit) |>
    bind_rows(any_hit |> transmute(arm, model = "all", lk_id, party_hit = k >= 2))
  welch <- function(a, b) {
    if (length(a) < 2 || length(b) < 2) return(c(diff = NA_real_, se = NA_real_, p = NA_real_))
    se <- sqrt(var(a) / length(a) + var(b) / length(b)); d <- mean(a) - mean(b)
    df <- se^4 / ((var(a) / length(a))^2 / (length(a) - 1) + (var(b) / length(b))^2 / (length(b) - 1))
    c(diff = d, se = se, p = if (is.finite(se) && se > 0) 2 * pt(-abs(d / se), df) else NA_real_)
  }
  RS <- pmap_dfr(RS_PAIRS, function(pair, x, y, rec_arm) {
    W |> filter(dim %in% FOCUS) |> mutate(d = .data[[x]] - .data[[y]]) |> filter(!is.na(d)) |>
      left_join(recw |> filter(arm == rec_arm) |> select(model, lk_id, recognised = party_hit), by = c("model", "lk_id")) |>
      filter(!is.na(recognised)) |>
      group_by(model, dim) |> group_modify(~ {
        a <- .x$d[.x$recognised]; bb <- .x$d[!.x$recognised]
        w1 <- welch(a, bb); w2 <- welch(abs(a), abs(bb))
        tibble(n_rec = length(a), n_not = length(bb), mean_d_rec = if (length(a)) mean(a) else NA_real_,
               mean_d_not = if (length(bb)) mean(bb) else NA_real_,
               diff = w1[["diff"]], se = w1[["se"]], p = w1[["p"]],
               mad_rec = if (length(a)) mean(abs(a)) else NA_real_, mad_not = if (length(bb)) mean(abs(bb)) else NA_real_, diff_abs = w2[["diff"]], se_abs = w2[["se"]], p_abs = w2[["p"]]) }) |>
      ungroup() |> mutate(pair = pair, .before = 1)
  }) |> arrange(pair, match(model, c(MODELS, "all")), match(dim, FOCUS))
  write_csv_det(RS, file.path(CSV, "57_shift_by_recognition.csv"))

  # ---- 6. Cost and model versions ----------------------------------------------------------
  u <- read_usage() |> mutate(type = case_when(task == "translate" ~ "translation", str_starts(task, "score_") ~ "scoring",
                                               str_starts(task, "recog_") ~ "recognition", TRUE ~ task))
  cost <- u |> group_by(type, model) |> summarise(calls = n(), in_tok = sum(in_tok, na.rm = TRUE), out_tok = sum(out_tok + thought_tok, na.rm = TRUE),
                                                  usd = sum(usd, na.rm = TRUE), .groups = "drop") |> arrange(type, model)
  write_csv_det(cost, file.path(CSV, "57_cost.csv"))
  versions <- u |> count(type, model_requested = model, model_resolved = resolved_model, name = "calls") |> arrange(type, model_requested)
  write_csv_det(versions, file.path(CSV, "57_model_versions.csv"))
  rc <- read_csv(file.path(LK_DIR, "redaction_counts.csv"), show_col_types = FALSE)

  # ---- 7. Tables (booktabs) ----------------------------------------------------------------
  tex_escape <- function(x) { x <- gsub("\\\\", "\\\\textbackslash{}", x); x <- gsub("([%&_#$])", "\\\\\\1", x); x }
  fmt <- function(v, d = 2) { out <- formatC(v, format = "f", digits = d); out <- sub("^-", "$-$", out); out[is.na(v)] <- ""; out }
  stars <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "$^{***}$", p < 0.05 ~ "$^{**}$", p < 0.1 ~ "$^{*}$", TRUE ~ "")
  write_tab_tex <- function(df, name, caption, note, header = NULL, align = NULL, midrule_after = NULL) {
    align <- align %||% paste0("l", strrep("r", ncol(df) - 1))
    body <- apply(as.matrix(df), 1, function(r) paste(ifelse(is.na(r), "", r), collapse = " & "))
    body <- paste0(body, " \\\\")
    if (length(midrule_after)) body[midrule_after] <- paste0(body[midrule_after], " \\midrule")
    tex <- c(sprintf("%% generated by R/57_Leakage_Test.R (PLLM-0208); csv in ../csv/"),
             "\\begin{table}[htbp]\\centering\\scriptsize\\setlength{\\tabcolsep}{4pt}",
             sprintf("\\caption{%s}\\label{tab:leak:%s}", caption, name),
             sprintf("\\begin{tabular}{@{}%s@{}}\\toprule", align),
             header %||% paste0(paste(names(df), collapse = " & "), " \\\\"), "\\midrule", body, "\\bottomrule\\end{tabular}",
             sprintf("\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}\\scriptsize\\textit{Notes:} %s\\end{minipage}", note),
             "\\end{table}")
    writeLines(tex, file.path(TAB, paste0(name, ".tex")))
  }
  # (i) recognition
  t1 <- hit |> filter(model %in% MODELS) |>
    transmute(Model = if_else(duplicated(model), "", MODEL_LAB[model]), Arm = ARM_LAB[arm], N = as.character(n), Party = fmt(100 * party_hit, 0),
              Country = fmt(100 * country_hit, 0), `Year exact` = fmt(100 * year_exact, 0), `Year $\\pm$1` = fmt(100 * year_pm1, 0),
              Confidence = fmt(mean_conf, 0), `Conf. hit` = fmt(conf_hit, 0), `Conf. miss` = fmt(conf_miss, 0))
  write_tab_tex(t1, "recognition", "Leakage test: can the models name the party behind the text?",
    paste("Hit rates in percent over the 30 sampled German-language manifestos (10 each from Germany, Austria and Switzerland). The",
          "recognition prompt shows the text without metadata or document id and asks for party, country and election year with a",
          "0--100 confidence. A party hit requires the named party to match the true party and the country to be right. Redacted",
          "arms: own-party names, abbreviations, self-designations, leading candidates and own-party office holders, URLs replaced",
          "by placeholders. Confidence columns: mean stated confidence, overall, for party hits and for misses."),
    header = "Model & Arm & N & Party & Country & Year exact & Year $\\pm$1 & Conf. & Conf. hit & Conf. miss \\\\",
    align = "llrrrrrrrr", midrule_after = c(3, 6))
  # (ii) differences
  t2 <- A |> filter(dim %in% FOCUS, cmp %in% c("noise", "de_red", "en_orig", "en_red")) |>
    mutate(cell_d = paste0(fmt(mean_diff), stars(p)), cell_r = fmt(r)) |>
    select(model, dim, cmp, cell_d, cell_r) |>
    pivot_wider(names_from = cmp, values_from = c(cell_d, cell_r)) |>
    arrange(match(model, c(MODELS, "all")), match(dim, FOCUS)) |>
    transmute(Model = if_else(duplicated(model), "", MODEL_LAB[model]), Dimension = DIM_LAB[dim],
              cell_d_noise, cell_r_noise, cell_d_de_red, cell_r_de_red, cell_d_en_orig, cell_r_en_orig, cell_d_en_red, cell_r_en_red)
  write_tab_tex(t2, "differences", "Leakage test: score differences under redaction and translation",
    paste("Mean difference $\\Delta$ (first arm minus second, 0--10 scales; IDEO on $[-1,1]$) and Pearson correlation $r$ across the",
          "30 manifestos, per model and for the cross-model mean (canonical aggregation, as the baseline). Noise: a fresh rerun of the",
          "unchanged German text against the stored production score. Stars: paired $t$-test of $\\Delta = 0$; $^{*}$ $p<0.1$,",
          "$^{**}$ $p<0.05$, $^{***}$ $p<0.01$. Documents without evidence on a dimension in either arm drop out of that cell."),
    header = paste("& & \\multicolumn{2}{c}{Noise} & \\multicolumn{2}{c}{DE red.\\ vs DE} & \\multicolumn{2}{c}{EN vs DE} & \\multicolumn{2}{c}{EN red.\\ vs DE} \\\\",
                   "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}\\cmidrule(lr){9-10}",
                   "Model & Dimension & $\\Delta$ & $r$ & $\\Delta$ & $r$ & $\\Delta$ & $r$ & $\\Delta$ & $r$ \\\\"),
    align = "llrrrrrrrr", midrule_after = c(7, 14, 21))
  # (iii) MAD vs noise
  t3 <- A |> filter(dim %in% c("populism_overall", "liberalism_overall", "IDEO"), cmp %in% c("noise", "de_red", "en_orig", "en_red", "en_red_en")) |>
    mutate(cell = if_else(cmp == "noise", fmt(mad), paste0(fmt(mad), " (", fmt(mad_excess), ")"))) |>
    select(model, dim, cmp, cell) |> pivot_wider(names_from = cmp, values_from = cell) |>
    arrange(match(model, c(MODELS, "all")), match(dim, FOCUS)) |>
    transmute(Model = if_else(duplicated(model), "", MODEL_LAB[model]), Dimension = DIM_LAB[dim], noise, de_red, en_orig, en_red, en_red_en)
  write_tab_tex(t3, "mad", "Leakage test: mean absolute differences against run-to-run noise",
    paste("Mean absolute difference between the arms per document; in parentheses the excess over the noise MAD of the same",
          "document (rerun of the German original against the stored production score), averaged over documents."),
    header = "Model & Dimension & Noise & DE red.\\ vs DE & EN vs DE & EN red.\\ vs DE & EN red.\\ vs EN \\\\",
    align = "llrrrrr", midrule_after = c(3, 6, 9))
  # (iv) slopes
  t4 <- slopes |> transmute(Model = if_else(duplicated(model), "", MODEL_LAB[model]), Arm = ARM_LAB[arm], N = as.character(n),
                            Slope = fmt(slope, 3), SE = fmt(se_hc1, 3),
                            Ratio = if_else(arm == "de_orig", "1", fmt(ratio)),
                            CI = if_else(is.na(ratio_lo), "", paste0("[", fmt(ratio_lo), ", ", fmt(ratio_hi), "]")))
  write_tab_tex(t4, "slopes", "Leakage test: slope of liberalism on populism by arm",
    paste("OLS of liberalism\\_overall on populism\\_overall (both 0--10) across the 30 manifestos, no fixed effects; SE: HC1. Ratio:",
          "slope relative to the German-original rerun, computed on the documents with both scores in both arms (so it is not the",
          "quotient of the printed slopes, which use every document of the arm); 95\\% percentile interval from a paired bootstrap",
          sprintf("over those documents (%s draws, both slopes on the same resample).", format(B_BOOT, big.mark = ","))),
    header = "Model & Arm & N & Slope & SE & Ratio to DE original & 95\\% CI \\\\", align = "llrrrrl",
    midrule_after = c(5, 10, 15))
  # (v) shift by recognition
  t5 <- RS |> filter(dim %in% c("populism_overall", "liberalism_overall")) |>
    transmute(Pair = if_else(pair == "de", "DE", "EN"), Model = MODEL_LAB[model], Dimension = DIM_LAB[dim],
              `N rec.` = as.character(n_rec), `N not` = as.character(n_not),
              `Shift rec.` = fmt(mean_d_rec), `Shift not` = fmt(mean_d_not), Difference = paste0(fmt(diff), stars(p)), SE = fmt(se),
              `|Shift| diff.` = paste0(fmt(diff_abs), stars(p_abs)), `SE ` = fmt(se_abs))
  write_tab_tex(t5, "shift_recognition", "Leakage test: redaction shift for recognised and unrecognised documents",
    paste("Shift = redacted minus original score of the same language (DE: German redacted minus German original; EN: English",
          "redacted minus English original). Recognised: the model names the right party (and country) for the redacted text;",
          "for the cross-model mean, at least two of three models do. Difference: recognised minus not recognised, Welch SE;",
          "$|$Shift$|$ diff.: the same for the absolute shift."),
    header = "Pair & Model & Dimension & N rec. & N not & Shift rec. & Shift not & Diff. & SE & $|$Shift$|$ diff. & SE \\\\",
    align = "lllrrrrrrrr", midrule_after = c(8))

  # ---- 8. REPORT.md --------------------------------------------------------------------------
  g <- function(tbl, ...) { r <- tbl |> filter(...); if (nrow(r) != 1) stop("lookup not unique"); r }
  num <- function(v, d = 2) ifelse(is.na(v), "NA", formatC(v, format = "f", digits = d))
  total_usd <- sum(cost$usd)
  diff_line <- function(m, d) {
    pick1 <- function(c) g(A, cmp == c, model == m, dim == d)
    sprintf("| %s | %s | %s (%s) | %s | %s (%s) | %s | %s (%s) | %s | %s (%s) | %s |", MODEL_LAB[m], DIM_LAB[d],
            num(pick1("noise")$mean_diff), num(pick1("noise")$se_diff), num(pick1("noise")$r),
            num(pick1("de_red")$mean_diff), num(pick1("de_red")$se_diff), num(pick1("de_red")$r),
            num(pick1("en_orig")$mean_diff), num(pick1("en_orig")$se_diff), num(pick1("en_orig")$r),
            num(pick1("en_red")$mean_diff), num(pick1("en_red")$se_diff), num(pick1("en_red")$r))
  }
  hr <- function(m, a) num(100 * g(hit, model == m, arm == a)$party_hit, 0)
  ad <- function(c, d, m = "all") g(A, cmp == c, model == m, dim == d)
  sl <- function(a, m = "all") g(slopes, model == m, arm == a)
  n_sig <- A |> filter(model == "all", dim %in% FOCUS, cmp %in% c("de_red", "en_orig", "en_red")) |> summarise(k = sum(p < 0.05, na.rm = TRUE), n = n())
  resid <- read_csv(file.path(LK_DIR, "residual_mentions.csv"), show_col_types = FALSE)
  headline <- c(
    "## Headline", "",
    sprintf("- **Recognition survives redaction.** On the redacted German text the models name the right party in %s%% (Claude), %s%% (GPT-4.1-mini) and %s%% (Gemini) of the 30 manifestos; on the redacted English text in %s%%, %s%% and %s%%. With the names in place: %s%%, %s%%, %s%%.",
            hr("claude", "de_red"), hr("gpt41mini", "de_red"), hr("gemini", "de_red"), hr("claude", "en_red"), hr("gpt41mini", "en_red"), hr("gemini", "en_red"),
            hr("claude", "de_orig"), hr("gpt41mini", "de_orig"), hr("gemini", "de_orig")),
    "  Name redaction therefore does not hide the party from these models; the text itself (rivals named, dates, slogans, policy profile) identifies it.",
    sprintf("- **Scores barely move.** Cross-model mean, redacted minus original German: populism %s (SE %s), liberalism %s (SE %s), IDEO %s (SE %s); correlations %s, %s, %s. Run-to-run noise (rerun minus stored production score): populism %s (SE %s), liberalism %s (SE %s).",
            num(ad("de_red", "populism_overall")$mean_diff), num(ad("de_red", "populism_overall")$se_diff), num(ad("de_red", "liberalism_overall")$mean_diff), num(ad("de_red", "liberalism_overall")$se_diff),
            num(ad("de_red", "IDEO")$mean_diff), num(ad("de_red", "IDEO")$se_diff), num(ad("de_red", "populism_overall")$r), num(ad("de_red", "liberalism_overall")$r), num(ad("de_red", "IDEO")$r),
            num(ad("noise", "populism_overall")$mean_diff), num(ad("noise", "populism_overall")$se_diff), num(ad("noise", "liberalism_overall")$mean_diff), num(ad("noise", "liberalism_overall")$se_diff)),
    sprintf("  Of the %d cross-model mean differences for the seven focus dimensions under redaction or translation, %d is significant at 5%% (financial-market liberalism, German redacted: %s, SE %s); about one is expected by chance.",
            n_sig$n, n_sig$k, num(ad("de_red", "lib_financial_market")$mean_diff), num(ad("de_red", "lib_financial_market")$se_diff)),
    sprintf("- **The populism-liberalism slope is unchanged.** Cross-model slope ratio to the German rerun: German redacted %s [%s, %s], English %s [%s, %s], English redacted %s [%s, %s]; stored production %s [%s, %s].",
            num(sl("de_red")$ratio), num(sl("de_red")$ratio_lo), num(sl("de_red")$ratio_hi), num(sl("en_orig")$ratio), num(sl("en_orig")$ratio_lo), num(sl("en_orig")$ratio_hi),
            num(sl("en_red")$ratio), num(sl("en_red")$ratio_lo), num(sl("en_red")$ratio_hi), num(sl("prod")$ratio), num(sl("prod")$ratio_lo), num(sl("prod")$ratio_hi)),
    sprintf("- **Recognised vs unrecognised documents cannot be compared with power.** Because recognition stays high, at most %d of 30 documents per model and arm go unrecognised (%d for the 2-of-3 rule in German); see the last table.",
            max(RS$n_not[RS$model != "all"]), min(RS$n_not[RS$model == "all" & RS$pair == "de"])),
    sprintf("- **Cost:** USD %s logged (cap USD %s).", num(total_usd), format(BUDGET_USD)), "")
  mad_rows <- A |> filter(model == "all", dim %in% c("populism_overall", "liberalism_overall", "IDEO")) |>
    select(cmp, dim, mad, mad_excess, se_mad_excess) |> arrange(match(dim, FOCUS), match(cmp, PAIRS$cmp))
  caveats <- c(
    "## Caveats", "",
    "- **The redaction does not remove identity.** It removes the party's own names, abbreviations, self-designations, leading candidates and own-party office holders and URLs. Rival parties, slogans, dates, regional references and the policy profile stay, and the models use them. The test therefore bounds the effect of the explicit name cue, not of recognition as such.",
    "- **The production prompt states country and election year** in every arm (frozen pipeline), which narrows the candidate parties to a handful. The recognition prompt gets no metadata, so its hit rates are, if anything, lower than what the scorer could infer.",
    "- **Document id.** The original arms send the real CMP doc_id (party code and date), as production does; the redacted arms send a neutral id. The redaction contrast therefore removes the id channel together with the names.",
    sprintf("- **Run-to-run noise is measured against the stored production scores**, 27 of the 30 from the June 2026 run (served versions not logged; Gemini through AI Studio with the floating alias) and 3 from the October 2026 run. The noise row mixes nondeterminism and possible version or endpoint drift (Gemini populism: rerun minus production %s, SE %s). There is no second English run, so the English noise level is not measured: the English comparisons are judged against the German noise.", num(ad("noise", "populism_overall", "gemini")$mean_diff), num(ad("noise", "populism_overall", "gemini")$se_diff)),
    "- **Translation.** Claude Sonnet 4.6 translated the texts; it is also one of the three scorers. The translation added paragraph breaks to texts that are single paragraphs in the corpus. In two passages of LK14 (FPÖ 1986) 'freiheitlich' became the generic 'liberal' and is redacted in German but not in English; the translation invented the abbreviation 'GRP' for the DRP (LK04), which was added to the alias table.",
    sprintf("- **Small N.** 30 documents; per-model N per dimension is lower where a model returns no verified evidence. Gemini yields a per-model IDEO for only %d to %d documents (it rarely verifies both left and right populism), so the Gemini IDEO rows are uninformative. GPT-4.1-mini's slope is close to zero in these 30 documents, so its ratios have very wide intervals.",
            min(A$n[A$model == "gemini" & A$dim == "IDEO"]), max(A$n[A$model == "gemini" & A$dim == "IDEO"])),
    sprintf("- **Residual mentions** after redaction (check stems): %d, %s; these refer to Catholic social teaching, not to the party.",
            nrow(resid), if (nrow(resid)) paste(unique(paste0(resid$arm, " ", resid$lk_id, " '", resid$match, "'")), collapse = "; ") else "none"),
    sprintf("- **Sample.** German-language single-chunk manifestos only (short and older texts over-represented: %d of 30 are from before 1990); Austria and", sum(as.integer(format(reg$date, "%Y")) < 1990)),
    "  Switzerland add language variety within German, not across languages; the English arms carry the cross-language check.",
    "- **Cost accounting.** The spend comes from the usage logs at list prices; Gemini is priced at the project's assumption (USD 0.50 / 3.00 per 1M tokens). A duplicate translation process started by mistake was stopped after about 10 seconds; its one completed call (LK20) is logged, and up to two further in-flight translation requests (at most about USD 0.65) may have been billed without being logged.", "")
  rep <- c(
    "# Leakage test (PLLM-0208): redacted German and English manifestos", "",
    sprintf("Generated by `R/57_Leakage_Test.R analyse` on %s. Not a chain stage; outputs are not in the manifest.", format(Sys.Date())), "",
    headline,
    "## Design", "",
    "- **Sample.** 30 German-language manifestos, 10 each from Germany, Austria and Switzerland, drawn with seed 20261111",
    "  from the single-chunk documents of the v2026a baseline (o200k tokens <= 18,000; one chunk for all three models;",
    "  Swiss texts kept only if the stopword heuristic says German), stratified 3 / 3 / 4 by corpus-wide populism tercile.",
    "- **Arms.** `de_orig` original German text with the real CMP doc_id (a fresh rerun of production); `de_red` own-party names,",
    "  abbreviations, self-designations, leading candidates / own-party office holders and URLs replaced by [PARTEI] / [PERSON] / [URL],",
    "  neutral document id; `en_orig` English translation by Claude Sonnet 4.6 (temperature 0, translation-only prompt), real doc_id;",
    "  `en_red` the same redaction on the translation ([PARTY]), neutral id.",
    "- **Scoring.** The frozen production pipeline (R/15b, R/16, R/18 via R/32 definitions, as R/52), all three models, canonical",
    "  aggregation of R/1_Aggregation.R. The production system prompt still states country and election year in every arm.",
    "- **Recognition.** A minimal prompt asks for party, country, election year and a 0-100 confidence (no metadata, no doc id);",
    "  run on `de_orig` (ceiling), `de_red` and `en_red`. Party hit = right party and right country.", "",
    "## Redaction", "",
    sprintf("- German: %d party mentions and %d person names replaced in 30 documents (median %d per document); %d URLs / e-mails.",
            sum(rc$n_party[rc$arm == "de_red"]), sum(rc$n_person[rc$arm == "de_red"]), as.integer(median(rc$n_party[rc$arm == "de_red"] + rc$n_person[rc$arm == "de_red"])),
            sum(rc$n_url[rc$arm == "de_red"])),
    sprintf("- English: %d party mentions and %d person names replaced; %d URLs / e-mails.",
            sum(rc$n_party[rc$arm == "en_red"]), sum(rc$n_person[rc$arm == "en_red"]), sum(rc$n_url[rc$arm == "en_red"])),
    sprintf("- Documents that never name their own party: %s.", paste(rc$lk_id[rc$arm == "de_red" & rc$n_party == 0 & rc$n_person == 0], collapse = ", ")),
    sprintf("- Residual mentions after redaction (case-insensitive check stems per party): %d.", sum(rc$n_residual)), "",
    "## Recognition", "",
    "| Model | Arm | N | Party hit | Country | Year exact | Mean confidence |", "|---|---|---|---|---|---|---|",
    hit |> filter(model %in% MODELS) |> transmute(l = sprintf("| %s | %s | %d | %s%% | %s%% | %s%% | %s |", MODEL_LAB[model], arm, n,
                                                       num(100 * party_hit, 0), num(100 * country_hit, 0), num(100 * year_exact, 0), num(mean_conf, 0))) |> pull(l),
    "", sprintf("Any model right on the redacted text: German %s%%, English %s%%; at least two of three: German %s%%, English %s%%.",
                num(100 * g(hit, model == "any model", arm == "de_red")$party_hit, 0), num(100 * g(hit, model == "any model", arm == "en_red")$party_hit, 0),
                num(100 * g(hit, model == "all (>= 2 of 3)", arm == "de_red")$party_hit, 0), num(100 * g(hit, model == "all (>= 2 of 3)", arm == "en_red")$party_hit, 0)), "",
    "## Score differences (mean difference (SE) and correlation)", "",
    "| Model | Dimension | Noise: rerun - prod | r | DE red - DE | r | EN - DE | r | EN red - DE | r |", "|---|---|---|---|---|---|---|---|---|---|",
    unlist(map(c(MODELS, "all"), function(m) map_chr(FOCUS, ~ diff_line(m, .x)))), "",
    "## Mean absolute differences, cross-model mean (excess over the noise MAD of the same document, SE)", "",
    "| Dimension | Comparison | N | MAD | Excess over noise (SE) |", "|---|---|---|---|---|",
    A |> filter(model == "all", dim %in% c("populism_overall", "liberalism_overall", "IDEO")) |> arrange(match(dim, FOCUS), match(cmp, PAIRS$cmp)) |>
      transmute(l = sprintf("| %s | %s | %d | %s | %s |", DIM_LAB[dim], CMP_LAB[cmp], n, num(mad),
                            if_else(cmp == "noise", "", sprintf("%s (%s)", num(mad_excess), num(se_mad_excess))))) |> pull(l), "",
    "Per model: `csv/57_differences.csv`, `tables/mad.tex`.", "",
    "## Slope of liberalism on populism (OLS, HC1; ratio to the German-original rerun, paired bootstrap 95% CI)", "",
    "| Model | Arm | N | Slope (SE) | Ratio [95% CI] |", "|---|---|---|---|---|",
    slopes |> transmute(l = sprintf("| %s | %s | %d | %s (%s) | %s |", MODEL_LAB[model], arm, n, num(slope, 3), num(se_hc1, 3),
                                    if_else(arm == "de_orig", "1", sprintf("%s [%s, %s]", num(ratio), num(ratio_lo), num(ratio_hi))))) |> pull(l), "",
    "## Redaction shift: recognised vs not recognised documents", "",
    "| Pair | Model | Dimension | N rec / not | Shift rec | Shift not | Diff (SE) | abs-shift diff (SE) |", "|---|---|---|---|---|---|---|---|",
    RS |> filter(dim %in% c("populism_overall", "liberalism_overall")) |>
      transmute(l = sprintf("| %s | %s | %s | %d / %d | %s | %s | %s (%s) | %s (%s) |", pair, MODEL_LAB[model], DIM_LAB[dim], n_rec, n_not,
                            num(mean_d_rec), num(mean_d_not), num(diff), num(se), num(diff_abs), num(se_abs))) |> pull(l), "",
    "## Cost (from the usage logs; list prices, Gemini at the project's assumption of USD 0.50 / 3.00 per 1M tokens)", "",
    "| Task | Model | Calls | Input tokens | Output tokens | USD |", "|---|---|---|---|---|---|",
    cost |> transmute(l = sprintf("| %s | %s | %d | %s | %s | %s |", type, model, calls, format(in_tok, big.mark = ","), format(out_tok, big.mark = ","), num(usd))) |> pull(l),
    sprintf("| **Total** | | %d | | | **%s** |", sum(cost$calls), num(total_usd)), "",
    "## Model versions served", "",
    "| Task | Requested | Resolved | Calls |", "|---|---|---|---|",
    versions |> transmute(l = sprintf("| %s | %s | %s | %d |", type, model_requested, coalesce(model_resolved, "not logged"), calls)) |> pull(l), "",
    caveats,
    "## Files", "",
    "- Inputs and raw outputs (restricted: manifesto text and model quotes): `data/CMP/leakage_oos/`.",
    "- Tables: `output/v2026a/validation/leakage/tables/{recognition,differences,mad,slopes,shift_recognition}.tex`.",
    "- CSV: `output/v2026a/validation/leakage/csv/57_*.csv`.")
  writeLines(rep, file.path(LK_OUT, "REPORT.md"))
  message("analyse: wrote ", file.path(LK_OUT, "REPORT.md"), sprintf("; total cost USD %.2f", total_usd))
  quit(save = "no")
}
stop("unknown mode: ", MODE)
