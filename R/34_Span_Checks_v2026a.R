# ============================================================
# 34_Span_Checks_v2026a.R
#
# Per-span checks that the aggregation (R/1_Aggregation.R) filters on, for all 13 dimensions
# including the three *_overall dimensions, which R/12 never validated (no keyword set, no
# judge description, so their quality_verified is FALSE by construction).
#
# For every span in data/CMP/v2026a/validated_spans_<m>.parquet (and the June AI-Studio
# Gemini spans of the 131 normalisation-only manifestos, for the Gemini-source fork):
#
#   quote_ok   = verbatim_found & !hallucination_flag
#                (stored R/12 results: tiered verbatim match against the source chunk, and
#                 "not a substring of the system prompt")
#   topic_rx   = quote or context matches the dimension's keyword regex, FIXED:
#                every TOPIC_KEYWORDS_V4 pattern in R/3c ends with a trailing "|", i.e. an
#                empty alternative that matches any text, so in R/12 the topic regex passed
#                every span (bug found 2026-10-24). KW_FIXED strips the trailing "|".
#                *_overall dims use the union of their sub-dimension sets
#                (populism_overall: anti-elitism + people-centrism + Manichean;
#                 liberalism_overall: the four lib_* sets). The ideology dims have no
#                keyword set (NA -> judge only).
#   judge      = LLM topic judge (gpt-4o-mini-2024-07-18, prompt as in R/12) for spans with
#                quote_ok and no regex match. Ideology L/R/C reuse the stored R/12 judge
#                answer; all other dims use data/CMP/v2026a/judge_topic_v2026a.parquet
#                (written by the `judge` stage).
#   topic_ok   = topic_rx | judge
#
# STAGES   Rscript R/34_Span_Checks_v2026a.R [checks | estimate | judge]
#   checks   (default, no API) writes span_checks_<m>.parquet from the stored flags + cache
#   estimate (no API) counts the (dim, quote) pairs the judge would need, with a cost estimate
#   judge    (API, OpenAI) asks the judge for missing pairs of JUDGE_DIMS, checkpointed,
#            hard cap JUDGE_CAP_USD; logs the resolved model of every call. Then run `checks`.
#
# Outputs (new files, atomic writes; nothing canonical is overwritten):
#   PIPE_DERIVED/span_checks_{claude,gpt41mini,gemini,gemini_june_superseded}.parquet
#   PIPE_IN/judge_topic_v2026a.parquet       (judge cache, one row per dim x quote; `judge`
#                                            stage only; an archived API artefact like the raw files)
#   PIPE_OUT/logs/34_span_checks.txt, 34_judge_estimate.csv
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(stringi)
})

STAGE <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(STAGE)) STAGE <- "checks"
stopifnot(STAGE %in% c("checks", "estimate", "judge"))

source("R/0_Config.R")
DIR   <- PIPE_IN                         # canonical inputs (read-only) + judge cache
LOG   <- file.path(PIPE_OUT, "logs")
JUDGE_CACHE   <- file.path(DIR, "judge_topic_v2026a.parquet")
JUDGE_MODEL   <- "gpt-4o-mini-2024-07-18"
JUDGE_CAP_USD <- 10
PRICE_IN <- 0.15 / 1e6; PRICE_OUT <- 0.60 / 1e6        # gpt-4o-mini list price, USD/token
# Dimensions sent to the judge: the 7 core dims and the 3 *_overall dims (L/R/C reuse the
# stored R/12 answers). 2026-10-24: *_overall first (authorised in Phase 2 step 2); the core
# dims after decision B, where the repaired topic check is a reported diagnostic and a
# spec-curve fork, not the baseline filter.
JUDGE_DIMS <- c("populism_overall", "liberalism_overall", "pop_ideology_overall",
                "pop_anti_elitism", "pop_people_centrism", "pop_manichean",
                "lib_political", "lib_social", "lib_economic", "lib_financial_market")

# ---- Keyword sets (fixed) and judge descriptions ----
suppressMessages(source("R/3c_Populism_v4_safe.R"))
KW_FIXED <- map(TOPIC_KEYWORDS_V4, ~ sub("\\|+$", "", .x))
KW_FIXED$populism_overall   <- paste(KW_FIXED[c("pop_anti_elitism", "pop_people_centrism", "pop_manichean")],
                                     collapse = "|")
KW_FIXED$liberalism_overall <- paste(KW_FIXED[c("lib_political", "lib_social", "lib_economic",
                                                "lib_financial_market")], collapse = "|")
stopifnot("a keyword set still matches arbitrary text" =
            !any(map_lgl(KW_FIXED, ~ stri_detect_regex("xyz qqq", .x))))

# R/12 descriptions for the 10 sub-dimensions (copied from DIM_DESCRIPTIONS_V4) plus the
# three *_overall dimensions (new; each is the union of its sub-dimension descriptions).
DIM_DESC <- list(
  lib_political        = "political liberalism: democracy, rule of law, courts, parliament, elections, free media, due process, political rights",
  lib_social           = "social liberalism: civil rights, minority protections, gender/LGBTQ+ equality, religious tolerance, pluralism",
  lib_economic         = "economic liberalism: free markets, business competition, trade openness, taxation, deregulation, privatization, industry policy",
  lib_financial_market = "financial-market liberalism: banks, finance, capital, credit, investment, currency, financial regulation, securities, interest rates, financial markets specifically",
  pop_anti_elitism     = "anti-elitism: criticism of elites, the establishment, oligarchs, corrupt politicians, the ruling class",
  pop_people_centrism  = "people-centrism: invoking 'the people', the nation, ordinary citizens, working people, popular will",
  pop_manichean        = "Manichean worldview: framing politics as good-vs-evil, betrayal, enemies, conspiracy, treason, existential struggle",
  pop_ideology_left    = "left-populist ideology: anti-corporate elite framing, redistribution, social equality",
  pop_ideology_right   = "right-populist ideology: national/identity protection, anti-immigration, cultural threat",
  pop_ideology_centrist = "centrist populist framing: generic people-elite appeals without left/right substance",
  populism_overall     = "populism: anti-elitism (criticism of elites, the establishment, corrupt politicians), people-centrism (invoking 'the people', ordinary citizens, popular will) or a Manichean good-vs-evil framing of politics",
  liberalism_overall   = "liberalism: political liberalism (democracy, rule of law, courts, rights), social liberalism (civil rights, minorities, equality, pluralism), economic liberalism (markets, competition, trade, taxation, regulation) or financial-market liberalism (banks, capital, credit, financial regulation)",
  pop_ideology_overall = "the ideological flavour of populism: left (anti-corporate framing, redistribution, social equality), right (national identity, anti-immigration, cultural threat) or centrist (generic people-elite appeals)"
)
IDEO_LRC <- c("pop_ideology_left", "pop_ideology_right", "pop_ideology_centrist")

# ---- Span tables ----
SPAN_SETS <- list(
  claude    = v26_file("validated_spans", "claude"),
  gpt41mini = v26_file("validated_spans", "gpt41mini"),
  gemini    = v26_file("validated_spans", "gemini"))
load_spans <- function(set) {
  if (set == "gemini_june_superseded") {
    alt <- read_parquet(file.path(DIR, "archive", "gemini_june_superseded.parquet"),
                        col_select = c("doc_id", "doc_id_root"))
    read_parquet("data/CMP/poplib_v4_gemini_flash_latest_validated_spans.parquet") |>
      semi_join(alt, by = "doc_id") |> mutate(span_source = "june2026")
  } else read_parquet(SPAN_SETS[[set]])
}
SETS <- c(names(SPAN_SETS), "gemini_june_superseded")

base_checks <- function(sp) {
  sp |>
    mutate(quote_ok = verbatim_found %in% TRUE & !(hallucination_flag %in% TRUE),
           txt = paste(coalesce(quote, ""), coalesce(context, ""))) |>
    group_by(dim_name) |>
    mutate(topic_rx = if (cur_group()$dim_name %in% names(KW_FIXED))
      stri_detect_regex(txt, KW_FIXED[[cur_group()$dim_name]], case_insensitive = TRUE) else NA) |>
    ungroup() |> select(-txt)
}

needs_judge <- function(ck) ck$quote_ok & !(ck$topic_rx %in% TRUE)

# ---- Stage: estimate / judge ----
if (STAGE %in% c("estimate", "judge")) {
  all <- map_dfr(SETS, ~ base_checks(load_spans(.x)) |> mutate(set = .x))
  # L/R/C reuse the stored R/12 answer; spans without one (the rev2 parse-loss re-scores, whose
  # spans got only the deterministic R/12 checks) are judged here with the same descriptions
  pend <- all |> filter(needs_judge(all), dim_name %in% JUDGE_DIMS |
                          (dim_name %in% IDEO_LRC & is.na(judge_topic))) |>
    distinct(dim_name, quote)
  if (file.exists(JUDGE_CACHE)) pend <- pend |> anti_join(read_parquet(JUDGE_CACHE), by = c("dim_name", "quote"))
  # tokens per call: system + user prompt (chars/4) + JSON overhead; output ~8 tokens
  pend <- pend |> mutate(tok_in = (nchar(DIM_DESC[dim_name]) * 2 + nchar(quote) + 420) / 4 + 60)
  est <- pend |> group_by(dim_name) |>
    summarise(pairs = n(), usd = sum(tok_in) * PRICE_IN + n() * 10 * PRICE_OUT, .groups = "drop")
  est <- bind_rows(est, tibble(dim_name = "TOTAL", pairs = sum(est$pairs), usd = sum(est$usd)))
  print(est)
  write_csv(est, file.path(LOG, "34_judge_estimate.csv"))
  if (STAGE == "estimate") quit(save = "no")
  stopifnot("estimate exceeds the judge cap" = sum(pend$tok_in) * PRICE_IN * 1.5 < JUDGE_CAP_USD)

  suppressMessages(source("R/1_Functions.R"))
  judge_one <- function(quote, dim_name) {
    desc <- DIM_DESC[[dim_name]]
    sys_p <- paste0("You are a topic-classification judge. ",
                    "Given a short quote (3-8 words) from a political-party manifesto, ",
                    "answer exactly 'yes' or 'no' (one lowercase word, no punctuation): ",
                    "Is the quote topically about ", desc, "? ",
                    "If the quote is about a different topic (e.g., healthcare for a finance question), answer 'no'. ",
                    "If the quote is generic and not on the topic, answer 'no'.")
    usr_p <- paste0("QUOTE: \"", quote, "\"\nIs this about ", desc, "? Reply yes or no.")
    body <- list(model = JUDGE_MODEL, instructions = sys_p,
                 input = list(list(role = "user", content = usr_p)),
                 max_output_tokens = 20, temperature = 0, top_p = 1, store = FALSE,
                 text = list(format = list(type = "json_schema", name = "judge", strict = TRUE,
                   schema = list(type = "object",
                                 properties = list(answer = list(type = "string", enum = list("yes", "no"))),
                                 required = list("answer"), additionalProperties = FALSE))))
    for (attempt in 1:3) {
      res <- tryCatch({
        req <- httr::POST("https://api.openai.com/v1/responses",
                          body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
                          httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
                                            `Content-Type` = "application/json"),
                          httr::timeout(60))
        httr::stop_for_status(req)
        cont <- httr::content(req, as = "parsed", type = "application/json")
        ans <- jsonlite::fromJSON(cont$output[[1]]$content[[1]]$text)$answer
        tibble(dim_name = dim_name, quote = quote, judge_topic = tolower(ans) == "yes",
               model_requested = JUDGE_MODEL, model_resolved = cont$model %||% NA_character_,
               tokens_in = cont$usage$input_tokens %||% NA_integer_,
               tokens_out = cont$usage$output_tokens %||% NA_integer_,
               judged_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      }, error = function(e) NULL)
      if (!is.null(res)) return(res)
      Sys.sleep(2 * attempt)
    }
    tibble(dim_name = dim_name, quote = quote, judge_topic = NA, model_requested = JUDGE_MODEL,
           model_resolved = NA_character_, tokens_in = NA_integer_, tokens_out = NA_integer_,
           judged_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  }

  library(furrr)
  plan(multisession, workers = 16L)
  spent <- if (file.exists(JUDGE_CACHE)) with(read_parquet(JUDGE_CACHE),
    sum(tokens_in, na.rm = TRUE) * PRICE_IN + sum(tokens_out, na.rm = TRUE) * PRICE_OUT) else 0
  batches <- split(pend |> arrange(dim_name, quote), ceiling(seq_len(nrow(pend)) / 1000))
  for (b in seq_along(batches)) {
    stopifnot("judge cap reached" = spent < JUDGE_CAP_USD)
    x <- batches[[b]]
    out <- future_map2_dfr(x$quote, x$dim_name, judge_one,
                           .options = furrr_options(seed = NULL,
                                                    globals = c("judge_one", "DIM_DESC", "JUDGE_MODEL", "%||%"),
                                                    packages = c("httr", "jsonlite", "tibble")))
    if (!all(out$model_resolved %in% c(JUDGE_MODEL, NA)))
      stop("judge served an unexpected model: ", paste(unique(out$model_resolved), collapse = ", "))
    cache <- if (file.exists(JUDGE_CACHE)) read_parquet(JUDGE_CACHE) else NULL
    write_atomic(bind_rows(cache, out |> filter(!is.na(judge_topic))), JUDGE_CACHE)
    spent <- spent + sum(out$tokens_in, na.rm = TRUE) * PRICE_IN + sum(out$tokens_out, na.rm = TRUE) * PRICE_OUT
    message(sprintf("judge batch %d/%d: %d pairs, %d failed, cumulative USD %.3f",
                    b, length(batches), nrow(out), sum(is.na(out$judge_topic)), spent))
  }
  plan(sequential)
  quit(save = "no")
}

# ---- Stage: checks (no API) ----
cache <- if (file.exists(JUDGE_CACHE)) read_parquet(JUDGE_CACHE) |>
  select(dim_name, quote, judge_new = judge_topic) else
  tibble(dim_name = character(), quote = character(), judge_new = logical())
log_lines <- c(sprintf("R/34 checks | judge cache: %s (%d pairs)",
                       if (file.exists(JUDGE_CACHE)) basename(JUDGE_CACHE) else "none", nrow(cache)))
for (set in SETS) {
  ck <- base_checks(load_spans(set)) |>
    left_join(cache, by = c("dim_name", "quote")) |>
    mutate(judge = case_when(!needs_judge(pick(everything())) ~ NA,
                             dim_name %in% IDEO_LRC & !is.na(judge_topic) ~ judge_topic,   # stored R/12 answer
                             TRUE ~ judge_new),
           judge_source = case_when(is.na(judge) ~ NA_character_,
                                    dim_name %in% IDEO_LRC & !is.na(judge_topic) ~ "R/12 (stored)",
                                    TRUE ~ "R/34 v2026a"),
           topic_ok = (topic_rx %in% TRUE) | (judge %in% TRUE)) |>
    select(doc_id, doc_id_root, dim_name, quote_ok, topic_rx, judge, judge_source, topic_ok,
           r12_quality_verified = quality_verified, span_source) |>
    arrange(doc_id, dim_name)
  stopifnot(!anyDuplicated(ck[c("doc_id", "dim_name")]))
  write_atomic(ck, file.path(PIPE_DERIVED, paste0("span_checks_", set, ".parquet")))
  s <- ck |> group_by(dim_name) |>
    summarise(n = n(),
              judge_missing = sum(quote_ok & !(topic_rx %in% TRUE) & is.na(judge)),
              share_quote_ok = mean(quote_ok), share_topic_rx = mean(topic_rx, na.rm = TRUE),
              share_quote_topic_ok = mean(quote_ok & topic_ok), .groups = "drop")
  log_lines <- c(log_lines, "", paste("==", set), capture.output(print(s, n = 20, width = 200)))
}
writeLines(log_lines, file.path(LOG, "34_span_checks.txt"))
cat(log_lines, sep = "\n")
