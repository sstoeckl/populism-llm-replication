# ============================================================
# 1_batch.R -- OpenAI Batch API helpers
#
# Provides functions to:
#   1. Upload a JSONL file to the OpenAI Files API
#   2. Create a batch job from the uploaded file
#   3. Poll batch status / wait for completion
#   4. Download output and error JSONL files
#   5. Parse the Responses-API-style batch output
#   6. Validate a JSONL file before submission
#
# Usage (interactive section at bottom):
#   - Set OPENAI_API_KEY in .Renviron
#   - Set `jsonl_path` to your request file
#   - Run the numbered steps sequentially
# ============================================================

library(httr)
library(jsonlite)
library(glue)

# ---- Config ----

OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
stopifnot(nzchar(OPENAI_API_KEY))

BETA_HEADER <- c(`OpenAI-Beta` = "batches=v1")

# ---- 1. Upload JSONL ----

#' Upload a JSONL file to OpenAI Files API (purpose = "batch").
#' Returns the file_id string.
batch_upload_jsonl <- function(jsonl_path, api_key = OPENAI_API_KEY) {
  stopifnot(file.exists(jsonl_path))
  resp <- POST(
    url = "https://api.openai.com/v1/files",
    add_headers(Authorization = paste("Bearer", api_key), BETA_HEADER),
    body = list(
      purpose = "batch",
      file    = upload_file(jsonl_path, type = "application/jsonl")
    ),
    encode = "multipart"
  )
  stop_for_status(resp)
  content(resp, as = "parsed", type = "application/json")$id
}

# ---- 2. Create Batch Job ----

#' Create a batch job from an uploaded file.
#' Returns the batch_id string.
batch_create <- function(input_file_id,
                         endpoint = "/v1/responses",
                         completion_window = "24h",
                         api_key = OPENAI_API_KEY) {
  body <- list(
    input_file_id    = input_file_id,
    endpoint         = endpoint,
    completion_window = completion_window
  )
  resp <- POST(
    url = "https://api.openai.com/v1/batches",
    add_headers(
      Authorization  = paste("Bearer", api_key),
      BETA_HEADER,
      `Content-Type` = "application/json"
    ),
    body = toJSON(body, auto_unbox = TRUE, null = "null")
  )
  stop_for_status(resp)
  content(resp, as = "parsed", type = "application/json")$id
}

# ---- 3. Batch Status / Wait ----

#' Retrieve current batch status (single call).
batch_status <- function(batch_id, api_key = OPENAI_API_KEY) {
  resp <- GET(
    url = glue("https://api.openai.com/v1/batches/{batch_id}"),
    add_headers(Authorization = paste("Bearer", api_key), BETA_HEADER)
  )
  stop_for_status(resp)
  content(resp, as = "parsed", type = "application/json")
}

#' Poll batch_status() until terminal state or timeout.
#' Returns the final status object.
batch_wait <- function(batch_id,
                       poll_every = 10,
                       max_wait   = 24 * 3600,
                       api_key    = OPENAI_API_KEY,
                       verbose    = TRUE) {
  t0 <- Sys.time()
  terminal <- c("completed", "failed", "cancelled", "cancelling")
  repeat {
    s <- batch_status(batch_id, api_key = api_key)
    if (verbose) message(sprintf("[%s] %s: %s", Sys.time(), batch_id, s$status))
    if (s$status %in% terminal) return(s)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (elapsed > max_wait) stop("batch_wait: timeout after ", round(elapsed), "s")
    Sys.sleep(poll_every)
  }
}

# ---- 4. Download Files ----

#' Download a single file by file_id to dest_path. Returns dest_path.
batch_download_file <- function(file_id, dest_path, api_key = OPENAI_API_KEY) {
  resp <- GET(
    url = glue("https://api.openai.com/v1/files/{file_id}/content"),
    add_headers(Authorization = paste("Bearer", api_key))
  )
  stop_for_status(resp)
  writeBin(content(resp, as = "raw"), dest_path)
  dest_path
}

#' Download output and error JSONL from a completed batch.
#' Returns a named list with output_path and/or error_path.
batch_download_results <- function(batch_status_obj,
                                   dest_dir = "batch/results",
                                   api_key  = OPENAI_API_KEY) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  out <- list()

  if (!is.null(batch_status_obj$output_file_id)) {
    path <- file.path(dest_dir, paste0(batch_status_obj$id, "_output.jsonl"))
    batch_download_file(batch_status_obj$output_file_id, path, api_key)
    out$output_path <- path
  }
  if (!is.null(batch_status_obj$error_file_id)) {
    path <- file.path(dest_dir, paste0(batch_status_obj$id, "_errors.jsonl"))
    batch_download_file(batch_status_obj$error_file_id, path, api_key)
    out$error_path <- path
  }
  out
}

# ---- 5. Parse Batch Output ----

#' Parse a Responses-API-style batch output JSONL into a data.frame.
#' Each line contains custom_id + response -> body -> output -> content -> text.
#' The text field is expected to be valid JSON, which is parsed and flattened.
parse_batch_output_responses <- function(output_jsonl_path) {
  lines <- readLines(output_jsonl_path, warn = FALSE)

  rows <- lapply(lines, function(line) {
    x <- tryCatch(fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(x)) return(data.frame(doc_id = NA_character_, status = NA_character_,
                                       stringsAsFactors = FALSE))

    cid <- x$custom_id %||% NA_character_

    # Extract the text payload from the nested response structure
    txt <- tryCatch(
      x$response$body$output[[1]]$content[[1]]$text %||%
        x$response$body$output[[1]]$content[[1]]$refusal,
      error = function(e) NULL
    )

    parsed <- NULL
    if (is.character(txt) && nzchar(txt)) {
      parsed <- tryCatch(fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
    }

    if (is.null(parsed)) {
      data.frame(doc_id = cid, status = NA_character_, stringsAsFactors = FALSE)
    } else {
      as.data.frame(rbind(c(doc_id = cid, parsed)), stringsAsFactors = FALSE)
    }
  })

  do.call(rbind, rows)
}

# ---- 6. Validate JSONL Before Submission ----

#' Check that every line in a JSONL file is valid for the Batch API:
#' must have method="POST", url="/v1/responses", body=object, unique custom_id.
validate_jsonl_for_batches <- function(path) {
  lines <- readLines(path, warn = FALSE)
  ids <- character(0)

  for (i in seq_along(lines)) {
    rec <- tryCatch(
      fromJSON(lines[i], simplifyVector = FALSE),
      error = function(e) stop("Line ", i, ": not valid JSON")
    )
    # Required fields
    for (k in c("method", "url", "body")) {
      if (is.null(rec[[k]])) stop("Line ", i, ": missing '", k, "'")
    }
    if (!identical(rec$method, "POST")) stop("Line ", i, ": method must be 'POST'")
    if (!identical(rec$url, "/v1/responses")) stop("Line ", i, ": url must be '/v1/responses'")
    if (!is.list(rec$body)) stop("Line ", i, ": body must be an object")

    # Unique custom_id
    if (!is.null(rec$custom_id)) {
      if (rec$custom_id %in% ids) stop("Line ", i, ": duplicate custom_id '", rec$custom_id, "'")
      ids <- c(ids, rec$custom_id)
    }
  }
  message("OK: ", length(lines), " requests, all valid with unique custom_ids.")
  invisible(TRUE)
}

# ---- Interactive Usage ----
# Uncomment and set jsonl_path to run interactively.
# jsonl_path <- getOption("openai.batch_jsonl_path", "batch/jobs/requests.jsonl")
#
# # Quick token estimate
# sum(ceiling(nchar(readLines(jsonl_path, warn = FALSE), type = "bytes") / 4))
#
# # Validate
# validate_jsonl_for_batches(jsonl_path)
#
# # Step 1: Upload
# file_id <- batch_upload_jsonl(jsonl_path)
# cat("file_id:", file_id, "\n")
#
# # Step 2: Create batch
# batch_id <- batch_create(file_id, endpoint = "/v1/responses", completion_window = "24h")
# cat("batch_id:", batch_id, "\n")
#
# # Step 3: Wait (or poll manually with batch_status())
# s <- batch_wait(batch_id, poll_every = 10, max_wait = 24 * 3600)
# # s <- batch_status(batch_id)
# s$status
#
# # Step 4: Download results
# paths <- batch_download_results(s, dest_dir = "batch/results")
# paths
#
# # Step 5: Parse output
# if (!is.null(paths$output_path)) {
#   results_df <- parse_batch_output_responses(paths$output_path)
#   head(results_df)
# }
# if (!is.null(paths$error_path)) {
#   message("Errors saved to: ", paths$error_path)
# }
