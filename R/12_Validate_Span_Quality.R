# ============================================================
# 12_Validate_Span_Quality.R  (BRANCH: pipeline-v4-safe)
#
# DETERMINISTIC PER-SPAN QUALITY VALIDATOR
#
# For every span emitted by the LLM, computes three flags:
#
#   1. hallucination_flag:  TRUE iff the quote is a substring of the
#      system prompt (catches the v1 'market-based finance, prudential
#      oversight' failure mode, where the LLM cited my own prompt back
#      as 'evidence').
#
#   2. verbatim_flag:       TRUE iff the quote appears verbatim in the
#      source chunk text (case-insensitive, whitespace-normalized
#      search via stringi). This is the same tiered search used in
#      R/8_Validate_Spans.R.
#
#   3. topic_flag:          TRUE iff the quote OR its context contains
#      at least one keyword from TOPIC_KEYWORDS_V4 for that dimension
#      (lib_financial_market: bank/finance/capital/credit/invest/...;
#      lib_political: democracy/elect/parliament/...; etc.).
#
# A span is QUALITY-VERIFIED iff it passes all three: not in the prompt,
# verbatim in the manifesto, and topically about the dimension.
#
# The validator is data-only — it does not call any API and does not
# modify scores. It produces a per-span quality table that drives both
# (a) aggregation weighting (Should I trust this score? Drop this span?)
# and (b) reporting in the paper ("X% of spans quality-verified").
#
# Functions exported:
#   - validate_span_quality(quote, context, chunk_text, system_prompt, dim_name)
#   - hallucination_check(quote, system_prompt)
#   - topic_check(quote, context, dim_name, topic_keywords = TOPIC_KEYWORDS_V4)
#   - validate_spans_table(spans_tbl, chunk_text_lookup,
#                          system_prompt, topic_keywords = TOPIC_KEYWORDS_V4)
#
# Note: TOPIC_KEYWORDS_V4 is defined in R/3c_Populism_v4_safe.R.
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(stringi)
})
source("R/1_Functions.R")
if (!exists("TOPIC_KEYWORDS_V4")) source("R/3c_Populism_v4_safe.R")

# ---- 1. Hallucination check ----
#
# A span is hallucinated if its (normalized) quote is a substring of
# the (normalized) system prompt. The normalization collapses whitespace
# and smart-quote variation so the LLM can't sneak around the check by
# inserting a tab or different quote glyph.
hallucination_check <- function(quote, system_prompt) {
  if (is.null(quote) || is.na(quote) || !nzchar(quote)) return(FALSE)
  q  <- norm_soft(quote)
  sp <- norm_soft(system_prompt)
  # Only flag quotes >= 3 words to avoid trivial matches.
  if (length(unlist(strsplit(q, "\\s+"))) < 3L) return(FALSE)
  isTRUE(stri_detect_fixed(sp, q, case_insensitive = TRUE))
}

# ---- 2. Verbatim check (delegates to the same tiered search as R/8) ----
#
# Returns list(found, method, start_in_doc, end_in_doc).
verbatim_check <- function(quote, chunk_text) {
  if (is.null(quote) || is.na(quote) || !nzchar(quote) ||
      is.null(chunk_text) || is.na(chunk_text) || !nzchar(chunk_text))
    return(list(found = FALSE, method = "no_input", start_in_doc = NA_integer_, end_in_doc = NA_integer_))
  pos <- stri_locate_first_fixed(chunk_text, quote, case_insensitive = FALSE)
  if (!is.na(pos[1, 1]))
    return(list(found = TRUE, method = "exact",
                start_in_doc = as.integer(pos[1, 1] - 1L),
                end_in_doc   = as.integer(pos[1, 2])))
  pos <- stri_locate_first_fixed(chunk_text, quote, case_insensitive = TRUE)
  if (!is.na(pos[1, 1]))
    return(list(found = TRUE, method = "iexact",
                start_in_doc = as.integer(pos[1, 1] - 1L),
                end_in_doc   = as.integer(pos[1, 2])))
  qn <- norm_soft(quote); cn <- norm_soft(chunk_text)
  pn <- stri_locate_first_fixed(cn, qn, case_insensitive = TRUE)
  if (!is.na(pn[1, 1])) {
    pat <- gsub("([.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", qn, perl = TRUE)
    pat <- gsub("\\s+", "\\\\s+", pat, perl = TRUE)
    m <- regexpr(paste0("(?i)", pat), chunk_text, perl = TRUE)
    if (m[1] > 0) {
      ml <- attr(m, "match.length")[1]
      return(list(found = TRUE, method = "norm",
                  start_in_doc = as.integer(m[1] - 1L),
                  end_in_doc   = as.integer(m[1] - 1L + ml)))
    }
  }
  list(found = FALSE, method = "not_found", start_in_doc = NA_integer_, end_in_doc = NA_integer_)
}

# ---- 3. Topic relevance ----
#
# A span is topically relevant if the quote OR the surrounding context
# contains at least one keyword from the dimension's regex pattern.
topic_check <- function(quote, context, dim_name, topic_keywords = TOPIC_KEYWORDS_V4) {
  if (!dim_name %in% names(topic_keywords)) return(NA)  # dimensions without topic check
  pat <- topic_keywords[[dim_name]]
  text_to_check <- paste(quote %||% "", context %||% "")
  if (!nzchar(text_to_check)) return(FALSE)
  isTRUE(stri_detect_regex(text_to_check, pat, case_insensitive = TRUE))
}

# ---- Combined per-span validation ----
#
# Returns a 1-row tibble with the three flags + diagnostics.
validate_span_quality <- function(quote, context, chunk_text, system_prompt,
                                  dim_name,
                                  topic_keywords = TOPIC_KEYWORDS_V4) {
  hall <- hallucination_check(quote, system_prompt)
  verb <- verbatim_check(quote, chunk_text)
  topic <- topic_check(quote, context, dim_name, topic_keywords)
  tibble(
    dim_name             = dim_name,
    quote                = as.character(quote %||% NA),
    context              = as.character(context %||% NA),
    hallucination_flag   = hall,
    verbatim_found       = verb$found,
    verbatim_method      = verb$method,
    start_in_doc         = verb$start_in_doc,
    end_in_doc           = verb$end_in_doc,
    topic_flag           = topic,                  # NA if no keyword set for this dim
    # Quality-verified = NOT hallucinated AND verbatim AND topic-relevant
    quality_verified     = !hall & isTRUE(verb$found) & isTRUE(topic)
  )
}

# ---- Batch: validate a whole spans table ----
#
# Expected columns in `spans_tbl`:
#   doc_id, dim_name, quote, context, [chunk_id]
# `chunk_text_lookup` is a named character vector keyed by doc_id (chunk id).
validate_spans_table <- function(spans_tbl, chunk_text_lookup,
                                 system_prompt,
                                 topic_keywords = TOPIC_KEYWORDS_V4) {
  if (nrow(spans_tbl) == 0) return(spans_tbl |>
    mutate(hallucination_flag = logical(0), verbatim_found = logical(0),
           verbatim_method = character(0), topic_flag = logical(0),
           quality_verified = logical(0),
           start_in_doc = integer(0), end_in_doc = integer(0)))
  out <- pmap_dfr(
    list(quote = spans_tbl$quote, context = spans_tbl$context,
         doc_id = spans_tbl$doc_id, dim_name = spans_tbl$dim_name),
    function(quote, context, doc_id, dim_name) {
      chunk_text <- chunk_text_lookup[[doc_id]] %||% NA_character_
      validate_span_quality(quote, context, chunk_text, system_prompt, dim_name, topic_keywords)
    }
  )
  # Drop the dim/quote/context columns we computed (already in spans_tbl)
  out <- out |> select(-dim_name, -quote, -context)
  bind_cols(spans_tbl, out)
}

# ---- LLM-as-judge backstop ----
#
# For spans that fail the regex topic check, ask gpt-4o-mini a single
# yes/no question. This catches multilingual edge cases where the regex
# is too narrow. The judge is deterministic (temperature=0) and the
# result is memoized per (quote, dim_name) pair within a session.

# Brief one-line description per dimension (used in judge prompt).
DIM_DESCRIPTIONS_V4 <- list(
  lib_political        = "political liberalism: democracy, rule of law, courts, parliament, elections, free media, due process, political rights",
  lib_social           = "social liberalism: civil rights, minority protections, gender/LGBTQ+ equality, religious tolerance, pluralism",
  lib_economic         = "economic liberalism: free markets, business competition, trade openness, taxation, deregulation, privatization, industry policy",
  lib_financial_market = "financial-market liberalism: banks, finance, capital, credit, investment, currency, financial regulation, securities, interest rates, financial markets specifically",
  pop_anti_elitism     = "anti-elitism: criticism of elites, the establishment, oligarchs, corrupt politicians, the ruling class",
  pop_people_centrism  = "people-centrism: invoking 'the people', the nation, ordinary citizens, working people, popular will",
  pop_manichean        = "Manichean worldview: framing politics as good-vs-evil, betrayal, enemies, conspiracy, treason, existential struggle",
  pop_ideology_left    = "left-populist ideology: anti-corporate elite framing, redistribution, social equality",
  pop_ideology_right   = "right-populist ideology: national/identity protection, anti-immigration, cultural threat",
  pop_ideology_centrist = "centrist populist framing: generic people-elite appeals without left/right substance"
)

# Session-scoped cache. Reset by clearing .judge_cache.
.judge_cache <- new.env(parent = emptyenv())

llm_judge_topic <- function(quote, dim_name,
                            model = "gpt-4o-mini-2024-07-18",
                            max_retries = 2L) {
  if (is.null(quote) || is.na(quote) || !nzchar(quote)) return(NA)
  desc <- DIM_DESCRIPTIONS_V4[[dim_name]]
  if (is.null(desc)) return(NA)
  cache_key <- paste0(dim_name, "::", quote)
  if (exists(cache_key, envir = .judge_cache, inherits = FALSE))
    return(get(cache_key, envir = .judge_cache))

  sys_p <- paste0(
    "You are a topic-classification judge. ",
    "Given a short quote (3-8 words) from a political-party manifesto, ",
    "answer exactly 'yes' or 'no' (one lowercase word, no punctuation): ",
    "Is the quote topically about ", desc, "? ",
    "If the quote is about a different topic (e.g., healthcare for a finance question), answer 'no'. ",
    "If the quote is generic and not on the topic, answer 'no'."
  )
  usr_p <- paste0("QUOTE: \"", quote, "\"\nIs this about ", desc, "? Reply yes or no.")

  for (attempt in seq_len(max_retries)) {
    res <- tryCatch(
      oa_chat_json(
        system_prompt = sys_p, user_prompt = usr_p,
        model = model,
        json_schema = list(
          type = "object",
          properties = list(answer = list(type = "string", enum = list("yes", "no"))),
          required = list("answer"),         # list-wrap so toJSON serializes as array
          additionalProperties = FALSE
        ),
        temperature = 0, top_p = 1, store = FALSE,
        max_output_tokens = 20
      ),
      error = function(e) NULL
    )
    if (!is.null(res) && is.character(res$text) && nzchar(res$text)) {
      parsed <- tryCatch(jsonlite::fromJSON(res$text), error = function(e) NULL)
      if (!is.null(parsed$answer)) {
        out <- tolower(parsed$answer) == "yes"
        assign(cache_key, out, envir = .judge_cache)
        return(out)
      }
    }
    Sys.sleep(0.5 * attempt)
  }
  # All retries failed -> return NA so caller can decide
  assign(cache_key, NA, envir = .judge_cache)
  NA
}

# Validate spans with optional LLM-judge backstop.
# Spans that PASS the regex topic check are accepted.
# Spans that FAIL the regex go to llm_judge_topic() — if the judge says
# yes, the span gets topic_flag=TRUE and judge_used=TRUE; otherwise FALSE.
validate_spans_table_with_judge <- function(spans_tbl, chunk_text_lookup,
                                            system_prompt,
                                            topic_keywords = TOPIC_KEYWORDS_V4,
                                            use_judge = TRUE,
                                            verbose = TRUE) {
  validated <- validate_spans_table(spans_tbl, chunk_text_lookup,
                                    system_prompt, topic_keywords)
  validated$judge_used   <- FALSE
  validated$judge_topic  <- NA
  if (!use_judge) return(validated)

  # Only ask the judge about spans that:
  #  - are verbatim present in the manifesto
  #  - failed (or have NA) regex topic check
  #  - are not flagged as hallucination
  to_judge_idx <- which(
    isTRUE(validated$verbatim_found) | (is.logical(validated$verbatim_found) & validated$verbatim_found) ,
    (is.na(validated$topic_flag) | !validated$topic_flag),
    !validated$hallucination_flag
  )
  # The above which() pattern is fussy; simplify:
  cond <- validated$verbatim_found &
          (is.na(validated$topic_flag) | !validated$topic_flag) &
          !validated$hallucination_flag
  to_judge_idx <- which(cond)

  if (length(to_judge_idx) == 0L) return(validated)

  if (verbose) message("LLM-judge: classifying ", length(to_judge_idx),
                       " spans that failed regex topic check...")
  for (i in to_judge_idx) {
    j <- llm_judge_topic(validated$quote[i], validated$dim_name[i])
    validated$judge_used[i]  <- TRUE
    validated$judge_topic[i] <- j
    # Topic accepted if regex OR judge says yes
    if (isTRUE(j)) validated$topic_flag[i] <- TRUE
  }
  # Recompute quality_verified with judge results
  validated$quality_verified <- !validated$hallucination_flag &
                                isTRUE(validated$verbatim_found) &
                                isTRUE(validated$topic_flag)
  # The above isTRUE doesn't vectorize; redo:
  validated$quality_verified <- (!validated$hallucination_flag) &
                                (validated$verbatim_found %in% TRUE) &
                                (validated$topic_flag %in% TRUE)
  validated
}

message("R/12_Validate_Span_Quality.R loaded: validate_span_quality(), validate_spans_table(), llm_judge_topic(), validate_spans_table_with_judge().")
