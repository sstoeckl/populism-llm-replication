# ============================================================
# 1_Functions.R
#
# Core utility library for the populism/liberalism manifesto
# scoring pipeline. Provides:
#   - File I/O (Parquet with CSV fallback)
#   - Token counting and chunking (rtiktoken)
#   - OpenAI Responses API JSON call
#   - Single-chunk scoring with retries
#   - Full manifesto scoring with chunking + aggregation
#   - Span repair and validation
#   - JSON-to-tibble conversion
#   - Text normalization helpers
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(httr)
  library(rlang)
})

# ---- Constants ---------------------------------------------------------------

# Span field names used throughout the pipeline (defined once)
SPAN_FIELDS <- c(
  "pop_key_phrases",
  "pop_ideology_key_phrases",
  "lib_key_phrases",
  "pop_ae_spans", "pop_pc_spans", "pop_mw_spans",
  "lib_pol_spans", "lib_soc_spans", "lib_eco_spans", "lib_fin_spans"
)

# Numeric score field names
SCORE_FIELDS <- c(
  "populism_overall", "pop_anti_elitism", "pop_people_centrism", "pop_manichean",
  "pop_ideology_overall", "pop_ideology_left", "pop_ideology_right",
  "pop_ideology_centrist",
  "liberalism_overall", "lib_political", "lib_social", "lib_economic",
  "lib_financial_market",
  "populism_confidence", "pop_ideology_confidence", "liberalism_confidence"
)

# ---- File I/O ----------------------------------------------------------------

#' Create directory recursively (no-op if exists)
#'
#' @param path Character; directory path.
#' @return Invisible path.
ensure_dir <- function(path) {
  stopifnot(is.character(path), length(path) == 1L)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

#' Write a data frame to Parquet (or CSV fallback)
#'
#' @param x Data frame or tibble to write.
#' @param path File path. If ends in .parquet and arrow is available, writes
#'   Parquet; otherwise writes CSV (changing extension to .csv).
safe_write_table <- function(x, path) {
  stopifnot(is.data.frame(x), is.character(path), length(path) == 1L)
  ensure_dir(dirname(path))
  if (requireNamespace("arrow", quietly = TRUE) &&
      grepl("\\.parquet$", path, ignore.case = TRUE)) {
    arrow::write_parquet(x, path)
  } else {
    csv_path <- sub("\\.parquet$", ".csv", path, ignore.case = TRUE)
    readr::write_csv(x, csv_path)
  }
}

#' Read a table from Parquet (or CSV fallback)
#'
#' @param path File path (.parquet preferred). Falls back to .csv if the
#'   parquet file does not exist or arrow is not available.
#' @return A tibble. Returns empty tibble if file not found.
safe_read_table <- function(path) {
  stopifnot(is.character(path), length(path) == 1L)
  use_arrow <- requireNamespace("arrow", quietly = TRUE)
  if (use_arrow && file.exists(path)) {
    return(arrow::read_parquet(path))
  }
  csv_path <- sub("\\.parquet$", ".csv", path, ignore.case = TRUE)
  if (file.exists(csv_path)) {
    return(readr::read_csv(csv_path, show_col_types = FALSE))
  }
  tibble()
}

# ---- Token counting and chunking --------------------------------------------

#' Count tokens using the rtiktoken library
#'
#' @param txt Character string.
#' @return Integer token count.
estimate_tokens <- function(txt) {
  stopifnot(is.character(txt), length(txt) == 1L)
  rtiktoken::get_token_count(txt, model = "gpt-4o-mini")
}

#' Truncate text to a maximum number of tokens
#'
#' @param txt Character string.
#' @param max_tokens Integer; maximum tokens to retain.
#' @return Truncated character string.
truncate_tokens <- function(txt, max_tokens) {
  stopifnot(is.character(txt), length(txt) == 1L)
  stopifnot(is.numeric(max_tokens), max_tokens > 0)
  if (!nzchar(txt)) return("")
  tokens <- rtiktoken::get_tokens(txt, model = "gpt-4o-mini")
  n_keep <- min(length(tokens), as.integer(max_tokens))
  rtiktoken::decode_tokens(tokens[seq_len(n_keep)], model = "gpt-4o-mini")
}

#' Split text into overlapping token-based chunks
#'
#' @param txt Character string to chunk.
#' @param chunk_tokens Integer; max tokens per chunk.
#' @param overlap_tokens Integer; overlap between consecutive chunks.
#' @return Character vector of chunks.
chunk_text <- function(txt, chunk_tokens = 7000, overlap_tokens = 500) {
  stopifnot(is.character(txt), length(txt) == 1L)
  if (!nzchar(txt)) return(character(0))

  tokens <- rtiktoken::get_tokens(txt, model = "gpt-4o-mini")
  n_tok  <- length(tokens)
  if (n_tok <= chunk_tokens) return(txt)

  step   <- chunk_tokens - overlap_tokens
  starts <- seq(1L, n_tok, by = step)
  chunks <- vector("character", length(starts))

  for (i in seq_along(starts)) {
    s <- starts[i]
    e <- min(s + chunk_tokens - 1L, n_tok)

    # Retry decode up to 5 times (rtiktoken can be flaky)
    decoded <- NULL
    for (k in 1:5) {
      decoded <- tryCatch(
        rtiktoken::decode_tokens(tokens[s:e], model = "gpt-4o-mini"),
        error = function(cond) NULL
      )
      if (!is.null(decoded)) break
      Sys.sleep(0.2 * k)
    }
    if (is.null(decoded)) {
      warning(sprintf("decode_tokens() failed after 5 retries at token %d. Returning full text.", s))
      return(txt)
    }

    chunks[i] <- decoded
    if (e == n_tok) break
  }

  # Trim any unused trailing slots (from early break)
  chunks[nzchar(chunks)]
}

# ---- Text normalization helpers ----------------------------------------------

#' Normalize smart quotes and collapse whitespace
#'
#' @param x Character string (or NULL/NA).
#' @return Normalized character string (empty string for NULL/NA).
norm_soft <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  x <- gsub("[\u2018\u2019\u201A\u201B\u2032]", "'", x, perl = TRUE)
  x <- gsub("[\u201C\u201D\u201E\u201F\u2033]", "\"", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

#' Build a case-insensitive regex from a literal quote
#'
#' Escapes regex metacharacters and replaces whitespace with \\s+.
#'
#' @param quote Character string.
#' @return Perl-compatible regex string.
quote_to_relaxed_regex <- function(quote) {
  q <- norm_soft(quote)
  q <- gsub("([.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", q, perl = TRUE)
  q <- gsub("\\s+", "\\\\s+", q, perl = TRUE)
  paste0("(?i)", q)
}

# ---- Span repair and validation ----------------------------------------------

#' Extract a local window around character offsets
#'
#' @param text Full document text.
#' @param start0 0-based start offset.
#' @param end0 0-based end offset.
#' @param tol_chars Integer; characters of padding on each side.
#' @return List with s, e (1-based bounds) and txt (substring).
local_window <- function(text, start0, end0, tol_chars = 60L) {
  n <- nchar(text)
  s <- max(1L, as.integer(start0) + 1L - tol_chars)
  e <- min(n, as.integer(end0) + tol_chars)
  list(s = s, e = e, txt = if (s <= e) substr(text, s, e) else "")
}

#' Check whether a string contains 3 to 8 whitespace-separated words
#'
#' @param s Character string (NULL/NA returns FALSE).
#' @return Logical.
is_3to8_words <- function(s) {
  if (is.null(s) || is.na(s)) return(FALSE)
  toks <- unlist(strsplit(trimws(s), "\\s+"))
  n <- sum(nzchar(toks))
  n >= 3L && n <= 8L
}

#' Try to repair a span if offsets are slightly off or quotes differ
#'
#' Attempts: (1) exact match at offsets, (2) case-insensitive fixed search
#' in a local window, (3) whitespace-relaxed regex search.
#'
#' @param text Full document text.
#' @param sp List with elements quote, start (0-based), end (0-based).
#' @param tol_chars Integer; window padding for search.
#' @return List with span (repaired) and ok (logical).
repair_span <- function(text, sp, tol_chars = 60L) {
  # Validate inputs
  if (is.null(sp$quote) || is.null(sp$start) || is.null(sp$end)) {
    return(list(span = sp, ok = FALSE))
  }
  if (!is.character(sp$quote)) return(list(span = sp, ok = FALSE))

  s <- suppressWarnings(as.integer(sp$start))
  e <- suppressWarnings(as.integer(sp$end))
  if (is.na(s) || is.na(e) || s < 0L || e <= s || e > nchar(text)) {
    return(list(span = sp, ok = FALSE))
  }

  # Step 1: exact match at provided offsets
  q <- substr(text, s + 1L, e)
  if (identical(q, sp$quote) && is_3to8_words(q)) {
    return(list(span = sp, ok = TRUE))
  }

  # Step 2+3: search in local window
  win <- local_window(text, s, e, tol_chars = tol_chars)
  if (nchar(win$txt) == 0L) return(list(span = sp, ok = FALSE))

  # 2a: exact case-insensitive fixed search
  pos <- regexpr(sp$quote, win$txt, ignore.case = TRUE, fixed = TRUE)
  if (pos[1] > 0L) {
    new_s <- win$s + pos[1] - 1L
    new_e <- new_s + nchar(sp$quote) - 1L
    new_q <- substr(text, new_s, new_e)
    if (is_3to8_words(new_q)) {
      sp$start <- new_s - 1L
      sp$end   <- new_e
      return(list(span = sp, ok = TRUE))
    }
  }

  # 2b: whitespace-relaxed regex (also handles smart quote differences)
  pattern <- quote_to_relaxed_regex(sp$quote)
  m <- regexpr(pattern, win$txt, perl = TRUE)
  if (m[1] > 0L) {
    ml    <- attr(m, "match.length")[1]
    new_s <- win$s + m[1] - 1L
    new_e <- new_s + ml - 1L
    new_q <- substr(text, new_s, new_e)
    if (is_3to8_words(new_q)) {
      sp$start <- new_s - 1L
      sp$end   <- new_e
      return(list(span = sp, ok = TRUE))
    }
  }

  list(span = sp, ok = FALSE)
}

#' Strict validation of a single span (after repair)
#'
#' Checks that the substring at [start+1, end] matches quote exactly
#' and has 3-8 words.
#'
#' @param text Full document text.
#' @param sp List with quote, start, end.
#' @return Logical.
valid_span <- function(text, sp) {
  if (is.null(sp$quote) || is.null(sp$start) || is.null(sp$end)) return(FALSE)
  s <- suppressWarnings(as.integer(sp$start))
  e <- suppressWarnings(as.integer(sp$end))
  if (is.na(s) || is.na(e) || s < 0L || e <= s || e > nchar(text)) return(FALSE)
  q <- substr(text, s + 1L, e)
  identical(q, sp$quote) && is_3to8_words(q)
}

#' Coerce a spans object to a list of span lists
#'
#' Handles NULL, data.frame, or list-of-lists input.
#'
#' @param spans NULL, data.frame, or list.
#' @return List of span lists.
as_list_of_spans <- function(spans) {
  if (is.null(spans)) return(list())
  if (is.data.frame(spans)) {
    return(lapply(seq_len(nrow(spans)), function(i) as.list(spans[i, , drop = FALSE])))
  }
  if (is.list(spans)) return(spans)
  list(as.list(spans))
}

#' Validate and repair an array of spans
#'
#' Checks that (1) the correct number of spans is present, (2) each span
#' can be repaired, (3) repaired spans pass strict validation, and
#' (4) spans do not overlap.
#'
#' @param text Full document text.
#' @param spans Span array (list-of-lists, data.frame, or NULL).
#' @param required_n Integer; expected number of spans.
#' @param tol_chars Integer; window padding for repair.
#' @return Logical (TRUE = all valid). Has attr "spans" with repaired spans.
validate_spanset <- function(text, spans, required_n, tol_chars = 60L) {
  spans_l <- as_list_of_spans(spans)

  if (length(spans_l) != required_n) {
    res <- FALSE
    attr(res, "spans") <- spans_l
    return(res)
  }

  # Repair each span
  repaired <- lapply(spans_l, function(sp) repair_span(text, sp, tol_chars))
  ok_vec   <- vapply(repaired, `[[`, logical(1), "ok")
  spans_r  <- lapply(repaired, `[[`, "span")

  # Strict check (verbatim + 3-8 words)
  ok_vec <- ok_vec & vapply(spans_r, function(sp) valid_span(text, sp), logical(1))
  if (!all(ok_vec)) {
    res <- FALSE
    attr(res, "spans") <- spans_r
    return(res)
  }

  # Check non-overlap
  rng <- t(vapply(spans_r, function(sp) c(as.integer(sp$start), as.integer(sp$end)), c(0L, 0L)))
  ord <- order(rng[, 1])
  rng <- rng[ord, , drop = FALSE]
  no_overlap <- if (nrow(rng) > 1L) all(rng[-1, 1] >= rng[-nrow(rng), 2]) else TRUE

  res <- isTRUE(no_overlap)
  attr(res, "spans") <- spans_r
  res
}

# ---- List / tibble helpers ---------------------------------------------------

#' Create an empty span placeholder
#'
#' @return List with quote="", start=0, end=0, context="".
empty_span <- function() {
  list(quote = "", start = 0L, end = 0L, context = "")
}

#' Recursively flatten a named list with dotted key names
#'
#' @param x Named list.
#' @param prefix Optional character prefix for keys.
#' @param expand_vectors Logical; if TRUE, atomic vectors get expanded to
#'   separate columns (name_1, name_2, ...).
#' @return Flat named list suitable for tibble construction.
flatten_named <- function(x, prefix = NULL, expand_vectors = TRUE) {
  stopifnot(is.list(x))
  out <- list()
  nms <- names(x) %||% rep("", length(x))

  for (i in seq_along(x)) {
    key  <- if (nzchar(nms[i])) nms[i] else paste0("V", i)
    name <- if (is.null(prefix)) key else paste(prefix, key, sep = ".")
    val  <- x[[i]]

    if (is.list(val) && !is.null(names(val))) {
      # Named sub-list: recurse
      out <- c(out, flatten_named(val, name, expand_vectors))
    } else if (is.atomic(val) && length(val) == 1L) {
      out[[name]] <- val
    } else if (is.atomic(val) && length(val) > 1L) {
      if (expand_vectors) {
        for (j in seq_along(val)) out[[paste0(name, "_", j)]] <- val[[j]]
      } else {
        out[[name]] <- list(val)
      }
    } else {
      out[[name]] <- list(val)
    }
  }
  out
}

#' Build a single-row tibble from scored output with list-columns for spans
#'
#' Scalar fields become regular columns; span fields (list-of-lists or
#' data.frames) become list-columns wrapping a tibble per span field.
#'
#' @param out Named list from JSON parsing.
#' @param span_fields Character vector of field names to treat as span arrays.
#' @return A one-row tibble.
make_row_tibble <- function(out, span_fields = SPAN_FIELDS) {
  spans_present <- intersect(names(out), span_fields)
  scalars       <- setdiff(names(out), spans_present)

  # Unbox length-1 lists in scalar fields
  scalar_vals <- lapply(out[scalars], function(x) {
    if (is.list(x) && length(x) == 1L && !is.null(x[[1]]) && !is.list(x[[1]])) {
      x[[1]]
    } else {
      x
    }
  })

  # Build span list-columns (each element is a tibble)
  empty_span_df <- tibble(
    quote = character(), start = integer(), end = integer(), context = character()
  )

  span_cols <- setNames(vector("list", length(spans_present)), spans_present)
  for (nm in spans_present) {
    sp <- out[[nm]]

    sp_df <- if (is.null(sp)) {
      empty_span_df
    } else if (is.data.frame(sp)) {
      tibble(
        quote   = as.character(sp$quote   %||% character(nrow(sp))),
        start   = as.integer(sp$start     %||% integer(nrow(sp))),
        end     = as.integer(sp$end       %||% integer(nrow(sp))),
        context = as.character(sp$context  %||% character(nrow(sp)))
      )
    } else if (is.list(sp) && length(sp) > 0L) {
      purrr::map_dfr(sp, function(s1) {
        tibble(
          quote   = as.character(s1$quote   %||% ""),
          start   = as.integer(s1$start     %||% 0L),
          end     = as.integer(s1$end       %||% 0L),
          context = as.character(s1$context  %||% "")
        )
      })
    } else {
      empty_span_df
    }

    span_cols[[nm]] <- list(sp_df)
  }

  tibble(!!!scalar_vals, !!!span_cols)
}

# ---- OpenAI API --------------------------------------------------------------

#' Call OpenAI Responses API with strict JSON schema
#'
#' Uses the /v1/responses endpoint (NOT /v1/chat/completions).
#' Reproducibility is achieved via temperature=0 and a pinned model snapshot.
#' NOTE: The Responses API does NOT support the `seed` parameter (unlike
#' Chat Completions). Determinism relies on temperature=0 + top_p=1.
#'
#' @param system_prompt Character; system instructions.
#' @param user_prompt Character; user message content.
#' @param model Character; model name (pinned snapshot for reproducibility).
#' @param json_schema List; JSON schema for structured output (or NULL).
#' @param temperature Numeric; sampling temperature (0 = deterministic).
#' @param max_output_tokens Integer; response token budget.
#' @param top_p Numeric; nucleus sampling parameter (1 = no truncation).
#' @param store Logical; if TRUE, OpenAI stores the response for later retrieval.
#' @param timeout_s Numeric; HTTP timeout in seconds.
#' @return List with text (response text), usage, system_fingerprint, and body.
oa_chat_json <- function(system_prompt, user_prompt,
                         model = "gpt-4o-mini-2024-07-18",
                         json_schema = NULL,
                         temperature = 0,
                         max_output_tokens = 5000,
                         top_p = 1,
                         store = TRUE,
                         timeout_s = 180) {

  key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(key)) cli::cli_abort("OPENAI_API_KEY environment variable is not set.")

  body <- list(
    model             = model,
    instructions      = system_prompt,
    input             = list(list(role = "user", content = user_prompt)),
    max_output_tokens = max_output_tokens,
    temperature       = temperature,
    top_p             = top_p,
    store             = store
  )

  if (!is.null(json_schema)) {
    body$text <- list(
      format = list(
        type   = "json_schema",
        name   = "poplib_v2",
        schema = json_schema,
        strict = TRUE
      )
    )
  }

  req <- httr::POST(
    url  = "https://api.openai.com/v1/responses",
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    httr::add_headers(
      Authorization  = paste("Bearer", key),
      `Content-Type` = "application/json"
    ),
    httr::timeout(timeout_s)
  )
  httr::stop_for_status(req)

  cont <- httr::content(req, as = "parsed", type = "application/json")

  # Log system_fingerprint for reproducibility auditing

  sys_fp <- cont$system_fingerprint %||% NA_character_
  if (!is.na(sys_fp)) {
    message("[oa_chat_json] system_fingerprint: ", sys_fp)
  }

  list(
    text               = cont$output[[1]]$content[[1]]$text,
    usage              = cont$usage %||% list(),
    system_fingerprint = sys_fp,
    body               = body
  )
}

# ---- Scoring functions -------------------------------------------------------

#' Collapse character vector, removing NAs, for chunk aggregation
#'
#' @param x Character vector.
#' @param sep Separator string.
#' @return Single character string, or NA if empty.
collapse_maybe_chr <- function(x, sep = " | ") {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  paste(unique(x), collapse = sep)
}

#' Set all scores to zero and fill placeholder spans (unscorable fallback)
#'
#' @param out Named list of scored output.
#' @return Modified list with status="unscorable" and zeroed scores.
coerce_unscorable <- function(out) {
  out$status <- "unscorable"
  for (nm in intersect(names(out), SCORE_FIELDS)) {
    out[[nm]] <- 0L
  }
  es3 <- rep(list(empty_span()), 3L)
  out$pop_key_phrases          <- es3
  out$pop_ideology_key_phrases <- es3
  out$lib_key_phrases          <- rep(list(empty_span()), 4L)
  out$pop_ae_spans <- es3
  out$pop_pc_spans <- es3
  out$pop_mw_spans <- es3
  out$lib_pol_spans <- list(empty_span())
  out$lib_soc_spans <- list(empty_span())
  out$lib_eco_spans <- list(empty_span())
  out$lib_fin_spans <- list(empty_span())
  out
}

#' Build the fallback unscorable row for a failed chunk
#'
#' @param doc_id Character; document/chunk identifier.
#' @return One-row tibble with all fields set to zero/empty.
unscorable_row <- function(doc_id) {
  es3 <- rep(list(empty_span()), 3L)
  out <- list(
    schema_version               = "poplib_v2",
    doc_id                       = doc_id,
    status                       = "unscorable",
    populism_overall             = 0L,
    pop_anti_elitism             = 0L,
    pop_people_centrism          = 0L,
    pop_manichean                = 0L,
    pop_ideology_overall         = 0L,
    pop_ideology_left            = 0L,
    pop_ideology_right           = 0L,
    pop_ideology_centrist        = 0L,
    liberalism_overall           = 0L,
    lib_political                = 0L,
    lib_social                   = 0L,
    lib_economic                 = 0L,
    lib_financial_market         = 0L,
    pop_key_phrases              = list(es3),
    pop_ideology_key_phrases     = list(es3),
    lib_key_phrases              = list(rep(list(empty_span()), 4L)),
    pop_ae_spans                 = list(es3),
    pop_pc_spans                 = list(es3),
    pop_mw_spans                 = list(es3),
    lib_pol_spans                = list(list(empty_span())),
    lib_soc_spans                = list(list(empty_span())),
    lib_eco_spans                = list(list(empty_span())),
    lib_fin_spans                = list(list(empty_span())),
    pop_justification            = "",
    pop_ideology_justification   = "",
    lib_justification            = "",
    populism_confidence          = 0L,
    pop_ideology_confidence      = 0L,
    liberalism_confidence        = 0L
  )
  tibble::as_tibble_row(out)
}

#' Score a single text chunk via the OpenAI API with retries
#'
#' Sends a chunk to the API, validates JSON, and converts to a one-row tibble.
#' On failure after max_retries, returns an unscorable fallback row.
#'
#' @param chunk_text Character; the text chunk to score.
#' @param doc_id_in Character; identifier for this chunk.
#' @param system_prompt Character; system instructions.
#' @param user_prompt_fn Function(chunk_text, doc_id) -> character.
#' @param json_schema List; strict JSON schema.
#' @param model Character; OpenAI model name.
#' @param max_retries Integer; number of API attempts.
#' @param backoff Numeric; base for exponential backoff (seconds).
#' @param max_output_tokens Integer; response token budget.
#' @return List with df (one-row tibble) and body (request body or NULL).
score_chunk_strict <- function(chunk_text, doc_id_in,
                               system_prompt, user_prompt_fn, json_schema,
                               model = "gpt-4o-mini-2024-07-18",
                               max_retries = 3,
                               backoff = 5,
                               max_output_tokens = 5000) {

  for (att in seq_len(max_retries)) {
    if (att > 1L) Sys.sleep(backoff^(att - 1))

    res <- tryCatch(
      oa_chat_json(
        system_prompt     = system_prompt,
        user_prompt       = user_prompt_fn(chunk_text, doc_id = doc_id_in),
        model             = model,
        json_schema       = json_schema,
        temperature       = 0,
        max_output_tokens = max_output_tokens
      ),
      error = function(e) NULL
    )
    if (is.null(res) || is.null(res$text)) next

    # Validate JSON
    ok_json <- tryCatch(jsonlite::validate(res$text), error = function(e) FALSE)
    if (!isTRUE(ok_json)) next

    out <- tryCatch(
      jsonlite::fromJSON(res$text, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.list(out)) next

    out$doc_id <- doc_id_in
    df <- make_row_tibble(out)
    return(list(df = df, body = res$body))
  }

  # All retries exhausted: return unscorable fallback
  list(df = unscorable_row(doc_id_in), body = NULL)
}

#' Score a full manifesto document (chunked if necessary)
#'
#' If the text fits within full_text_token_cap, scores it as a single chunk.
#' Otherwise, splits into overlapping chunks and aggregates: numeric fields
#' are averaged, character fields collapsed, list-columns concatenated.
#'
#' @param text Character; full manifesto text.
#' @param doc_id Character; document identifier.
#' @param system_prompt Character; system instructions.
#' @param user_prompt_fn Function(chunk_text, doc_id) -> character.
#' @param json_schema List; strict JSON schema.
#' @param full_text_token_cap Integer; token threshold for chunking.
#' @param chunk_tokens Integer; tokens per chunk.
#' @param overlap_tokens Integer; overlap between chunks.
#' @param model Character; OpenAI model name.
#' @param max_output_tokens Integer; response token budget.
#' @return List with raw (per-chunk tibble), agg (aggregated tibble), and
#'   body (list of request bodies).
score_manifesto_doc <- function(text, doc_id,
                                system_prompt, user_prompt_fn, json_schema,
                                full_text_token_cap = 20000L,
                                chunk_tokens = 10000L,
                                overlap_tokens = 2000L,
                                model = "gpt-4o-mini-2024-07-18",
                                max_output_tokens = 5000L) {

  # Handle empty text
  if (!nzchar(text)) {
    empty <- tibble(doc_id = doc_id, status = "unscorable")
    return(list(
      raw  = empty,
      agg  = tibble(doc_id = doc_id, n_chunks = 0L, status = "unscorable"),
      body = list()
    ))
  }

  # Decide: single pass or chunked
  est <- tryCatch(estimate_tokens(text), error = function(e) Inf)
  chunks <- if (is.finite(est) && est <= full_text_token_cap) {
    list(text)
  } else {
    as.list(chunk_text(text, chunk_tokens, overlap_tokens))
  }
  if (length(chunks) == 0L) chunks <- list(text)

  # Score each chunk
  pieces <- purrr::imap(chunks, function(ch, idx) {
    score_chunk_strict(
      chunk_text        = ch,
      doc_id_in         = paste0(doc_id, "_ch", idx),
      system_prompt     = system_prompt,
      user_prompt_fn    = user_prompt_fn,
      json_schema       = json_schema,
      model             = model,
      max_output_tokens = max_output_tokens
    )
  })

  bodies <- purrr::map(pieces, "body")
  dfs    <- purrr::map(pieces, "df")

  raw <- dplyr::bind_rows(dfs) %>%
    dplyr::mutate(doc_id_root = doc_id, chunk_id = dplyr::row_number())

  # Aggregate across chunks
  num_cols  <- names(which(vapply(raw, is.numeric, TRUE)))
  char_cols <- names(which(vapply(raw, is.character, TRUE)))
  list_cols <- names(which(vapply(raw, is.list, TRUE)))

  agg <- raw %>%
    dplyr::summarise(
      doc_id   = dplyr::first(doc_id_root),
      n_chunks = dplyr::n(),
      dplyr::across(dplyr::all_of(num_cols),  ~ mean(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(char_cols), ~ collapse_maybe_chr(.x)),
      dplyr::across(dplyr::all_of(list_cols), ~ list(purrr::flatten(.x))),
      status = if (all(tolower(dplyr::coalesce(.data$status, "ok")) == "ok")) "ok" else "unscorable"
    )

  out <- list(raw = raw, agg = agg, body = bodies)
  save(out, file = sprintf("data/CMP/raw/%s.RData", doc_id))
  out
}
