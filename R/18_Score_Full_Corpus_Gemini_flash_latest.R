# ============================================================
# 18_Score_Full_Corpus_Gemini_flash_latest.R   (BRANCH: pipeline-v4-safe)
#
# FULL-CORPUS V4 SAFE SCORING — gemini-flash-latest (Google AI Studio).
# Cross-LLM family #3 alongside Sonnet (R/15b) and gpt-4.1-mini (R/16).
#
# REPLACES R/17 (gemini-2.5-flash). Matched-pair pilot showed flash-latest
# at mean |gap| 0.32 / rho 0.81 vs Sonnet, while gemini-2.5-flash with
# thinkingBudget=4096 was 1.91 / 0.72 (THINKING HURT calibration).
# flash-latest also has no hidden thinking tokens (pilot: 0 thoughts).
#
# Robustness features mirror R/14/R/15b/R/16:
#   - Per-chunk retry with exponential backoff
#   - Per-batch RData checkpoints (every 100 manifestos)
#   - Resume capability
#   - furrr parallel execution (4 workers)
#
# Outputs (SEPARATE from R/17 so we keep both runs for comparison):
#   data/CMP/v4_gemini_flash_latest_checkpoints/batch_NNNN.RData
#   data/CMP/poplib_v4_gemini_flash_latest_raw.parquet
#   data/CMP/poplib_v4_gemini_flash_latest_validated_spans.parquet
#   data/CMP/poplib_v4_gemini_flash_latest_agg.parquet
#
# Cost: ~$31 (pilot per-call $0.0077 × ~4000 calls)
# Wall: ~3-5 h with 4 parallel workers
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(readxl); library(lubridate)
  library(jsonlite); library(httr); library(furrr)
})
source("R/1_Functions.R")
source("R/3c_Populism_v4_safe.R")
source("R/12_Validate_Span_Quality.R")

if (!nzchar(Sys.getenv("GEMINI_API_KEY"))) stop("GEMINI_API_KEY missing")

# ============================================================
# Configuration
# ============================================================

N_WORKERS              <- 4L
CHECKPOINT_EVERY       <- 100L
CHECKPOINT_DIR         <- "data/CMP/v4_gemini_flash_latest_checkpoints/"
MAX_RETRIES_PER_CHUNK  <- 5L
RETRY_BASE_SLEEP_SEC   <- 2
FULL_TEXT_TOKEN_CAP    <- 18000L
CHUNK_TOKENS           <- 20000L
OVERLAP_TOKENS         <- 2000L
MODEL_GEMINI           <- "gemini-flash-latest"

ensure_dir(CHECKPOINT_DIR)

V4_DIMS <- c("pop_anti_elitism","pop_people_centrism","pop_manichean","populism_overall",
             "pop_ideology_left","pop_ideology_right","pop_ideology_centrist","pop_ideology_overall",
             "lib_political","lib_social","lib_economic","lib_financial_market","liberalism_overall")

# ============================================================
# Convert poplib_v4_schema to Gemini-compatible format
# Gemini uses OpenAPI 3.0 subset:
#   - type values UPPERCASE ("OBJECT","STRING","INTEGER","ARRAY","BOOLEAN","NUMBER")
#   - no "additionalProperties: false" (Gemini rejects)
#   - nullable via "nullable: true" instead of type union
# ============================================================

to_gemini_schema <- function(node) {
  if (!is.list(node)) return(node)
  # Translate type
  if (!is.null(node$type)) {
    t <- node$type
    if (is.character(t) && length(t) > 1) {
      # Was c("integer","null") in our schema — strip null
      keep <- setdiff(t, "null")
      node$type <- toupper(keep[1])
      if ("null" %in% t) node$nullable <- TRUE
    } else if (is.character(t)) {
      node$type <- toupper(t)
    }
  }
  # Strip unsupported keys
  node$additionalProperties <- NULL
  # Recurse into properties / items
  for (k in names(node)) node[[k]] <- to_gemini_schema(node[[k]])
  node
}

gemini_v4_schema <- to_gemini_schema(poplib_v4_schema)

gemini_v4_chat <- function(system_prompt, user_prompt, model = MODEL_GEMINI,
                           max_output_tokens = 12000L, timeout_s = 180) {
  key <- Sys.getenv("GEMINI_API_KEY")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model,
                ":generateContent?key=", key)
  body <- list(
    systemInstruction = list(parts = list(list(text = system_prompt))),
    contents = list(list(parts = list(list(text = user_prompt)))),
    generationConfig = list(
      temperature = 0,
      maxOutputTokens = max_output_tokens,
      responseMimeType = "application/json",
      responseSchema = gemini_v4_schema,
      # Disable Gemini 2.5's default "thinking" mode (eats output token budget
      # before producing the JSON) — we want all 12k tokens for the actual schema fill
      thinkingConfig = list(thinkingBudget = 0)
    )
  )
  req <- httr::POST(url, content_type_json(),
                    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
                    httr::timeout(timeout_s))
  if (httr::status_code(req) >= 400)
    stop("Gemini API error: ", httr::content(req, "text", encoding = "UTF-8"))
  cont <- httr::content(req, "parsed", type = "application/json")
  txt <- cont$candidates[[1]]$content$parts[[1]]$text
  if (is.null(txt) || !nzchar(txt)) stop("Gemini returned empty text")
  list(text = txt, usage = cont$usageMetadata)
}

score_chunk_v4_gemini <- function(chunk_text, doc_id_in, country, year,
                                  max_retries = MAX_RETRIES_PER_CHUNK,
                                  base_sleep = RETRY_BASE_SLEEP_SEC) {
  sysp <- system_prompt_v4_safe(country, year)
  usp  <- user_prompt_v4(chunk_text, doc_id_in)
  for (attempt in seq_len(max_retries)) {
    res <- tryCatch(gemini_v4_chat(sysp, usp), error = function(e) list(text = NA, err = conditionMessage(e)))
    if (is.character(res$text) && nzchar(res$text)) {
      parsed <- tryCatch(fromJSON(res$text, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed)) return(list(parsed = parsed, attempts = attempt))
    }
    Sys.sleep(base_sleep * (2^(attempt - 1)))
  }
  list(parsed = NULL, attempts = max_retries, last_error = res$err %||% "unknown")
}

# Defensive flatten (Gemini may also return flat ints for "overall" dims)
flatten_parsed_v4_gemini <- function(parsed, chunk_doc_id, doc_id_root, chunk_id, chunk_text) {
  row <- tibble(doc_id = chunk_doc_id, doc_id_root = doc_id_root, chunk_id = chunk_id,
                schema_version = parsed$schema_version %||% NA_character_)
  for (d in V4_DIMS) {
    obj <- parsed[[d]]
    if (is.list(obj)) {
      row[[paste0(d,"_score")]]   <- if (is.null(obj$score)) NA_real_ else as.numeric(obj$score)
      row[[paste0(d,"_status")]]  <- as.character(obj$evidence_status %||% NA)
      row[[paste0(d,"_quote")]]   <- as.character(obj$span$quote   %||% NA)
      row[[paste0(d,"_context")]] <- as.character(obj$span$context %||% NA)
    } else if (length(obj) > 0L && !is.na(suppressWarnings(as.numeric(obj[1])))) {
      row[[paste0(d,"_score")]]   <- suppressWarnings(as.numeric(obj[1]))
      row[[paste0(d,"_status")]]  <- "verified"
      row[[paste0(d,"_quote")]]   <- NA_character_
      row[[paste0(d,"_context")]] <- NA_character_
    } else {
      row[[paste0(d,"_score")]] <- NA_real_; row[[paste0(d,"_status")]] <- NA_character_
      row[[paste0(d,"_quote")]] <- NA_character_; row[[paste0(d,"_context")]] <- NA_character_
    }
  }
  row$populism_confidence       <- as.numeric(parsed$populism_confidence %||% NA)
  row$pop_ideology_confidence   <- as.numeric(parsed$pop_ideology_confidence %||% NA)
  row$liberalism_confidence     <- as.numeric(parsed$liberalism_confidence %||% NA)
  row$pop_justification         <- as.character(parsed$pop_justification %||% NA)
  row$pop_ideology_justification<- as.character(parsed$pop_ideology_justification %||% NA)
  row$lib_justification         <- as.character(parsed$lib_justification %||% NA)
  spans <- map_dfr(V4_DIMS, function(d) {
    obj <- parsed[[d]]
    if (!is.list(obj)) return(tibble())
    q <- obj$span$quote %||% ""
    if (!nzchar(q)) return(tibble())
    tibble(doc_id = chunk_doc_id, doc_id_root = doc_id_root, chunk_id = chunk_id,
           dim_name = d, quote = q, context = obj$span$context %||% "",
           chunk_text = chunk_text)
  })
  list(row = row, spans = spans)
}

score_manifesto_v4_gemini <- function(doc_id_root, text, country, year) {
  est <- tryCatch(estimate_tokens(text), error = function(e) Inf)
  chunks <- if (is.finite(est) && est <= FULL_TEXT_TOKEN_CAP) list(text)
            else as.list(chunk_text(text, CHUNK_TOKENS, OVERLAP_TOKENS))
  if (length(chunks) == 0L) chunks <- list(text)
  rows <- vector("list", length(chunks)); spans <- vector("list", length(chunks))
  errors <- character()
  for (ci in seq_along(chunks)) {
    cdi <- paste0(doc_id_root, "_ch", ci)
    sr <- score_chunk_v4_gemini(chunks[[ci]], cdi, country, year)
    if (is.null(sr$parsed)) {
      errors <- c(errors, paste0(cdi, ": ", sr$last_error %||% "no_parse"))
      rows[[ci]] <- tibble(doc_id = cdi, doc_id_root = doc_id_root, chunk_id = ci,
                           schema_version = NA_character_, scoring_error = sr$last_error %||% "no_parse")
      next
    }
    f <- flatten_parsed_v4_gemini(sr$parsed, cdi, doc_id_root, ci, chunks[[ci]])
    rows[[ci]] <- f$row; spans[[ci]] <- f$spans
  }
  list(raw = bind_rows(rows), spans = bind_rows(spans), errors = errors, doc_id_root = doc_id_root)
}

# ============================================================
# Load data + resume
# ============================================================

message("=== Full-corpus v4 Gemini 2.5 Flash driver ===")
message("Timestamp: ", Sys.time(), " | model: ", MODEL_GEMINI)

dat <- read_parquet("data/CMP/full_data.parquet") |>
  filter(!is.na(manifesto_id), !is.na(text)) |>
  mutate(date = as.Date(date)) |>
  distinct(doc_id, .keep_all = TRUE)

existing_files <- list.files(CHECKPOINT_DIR, pattern = "^batch_\\d+\\.RData$", full.names = TRUE)
already_done <- character()
if (length(existing_files) > 0) {
  for (f in existing_files) {
    e <- new.env(); load(f, envir = e)
    # IMPORTANT: only treat a manifesto as "done" if it has at least one
    # successfully scored chunk (no scoring_error and a real schema_version).
    # Otherwise quota-failed chunks would block re-scoring forever.
    raw <- e$batch_result$raw
    if (nrow(raw) == 0) next
    if ("scoring_error" %in% names(raw)) {
      good <- raw |> filter(is.na(scoring_error), !is.na(schema_version))
    } else {
      good <- raw |> filter(!is.na(schema_version))
    }
    already_done <- c(already_done, unique(good$doc_id_root))
  }
  already_done <- unique(already_done)
  message("Resume: ", length(already_done), " manifestos already SUCCESSFULLY scored (failures will be retried)")
}
to_score <- dat |> filter(!doc_id %in% already_done)
message("Remaining to score: ", nrow(to_score))

if (nrow(to_score) > 0) {
  plan(multisession, workers = N_WORKERS)
  message("Parallel plan: ", N_WORKERS, " workers")
  batch_idx_start <- length(existing_files) + 1L
  n_batches <- ceiling(nrow(to_score) / CHECKPOINT_EVERY)
  for (b in seq_len(n_batches)) {
    batch_idx <- batch_idx_start + b - 1L
    s <- (b-1L)*CHECKPOINT_EVERY + 1L; e <- min(b*CHECKPOINT_EVERY, nrow(to_score))
    batch_rows <- to_score[s:e, ]
    message(sprintf("\n--- Gemini batch %d (%d-%d of %d) ---", batch_idx, s, e, nrow(to_score)))
    t0 <- Sys.time()
    results <- future_map(
      seq_len(nrow(batch_rows)),
      function(i) {
        r <- batch_rows[i, ]
        score_manifesto_v4_gemini(r$doc_id, r$text, r$countryname, year(r$date))
      },
      .options = furrr_options(
        globals = c("batch_rows", "MODEL_GEMINI","MAX_RETRIES_PER_CHUNK","RETRY_BASE_SLEEP_SEC",
                    "FULL_TEXT_TOKEN_CAP","CHUNK_TOKENS","OVERLAP_TOKENS","V4_DIMS",
                    "score_manifesto_v4_gemini","score_chunk_v4_gemini","flatten_parsed_v4_gemini",
                    "gemini_v4_chat","gemini_v4_schema","to_gemini_schema",
                    "system_prompt_v4_safe","user_prompt_v4","poplib_v4_schema",
                    "estimate_tokens","chunk_text","truncate_tokens","%||%"),
        packages = c("tibble","dplyr","purrr","tidyr","httr","jsonlite","rtiktoken","lubridate"),
        seed = TRUE
      ), .progress = TRUE
    )
    batch_result <- list(raw = bind_rows(map(results, "raw")),
                        spans = bind_rows(map(results, "spans")),
                        errors = unlist(map(results, "errors")), timestamp = Sys.time())
    save(batch_result, file = sprintf("%sbatch_%04d.RData", CHECKPOINT_DIR, batch_idx))
    el <- Sys.time() - t0
    message(sprintf("  batch %d: %.1f %s | rows %d | spans %d | err %d",
                    batch_idx, as.numeric(el), attr(el,"units"),
                    nrow(batch_result$raw), nrow(batch_result$spans), length(batch_result$errors)))
  }
  plan(sequential)
}

# ============================================================
# Combine + validate + aggregate (same logic as R/15b)
# ============================================================

message("\n=== Combining Gemini batches ===")
all_files <- list.files(CHECKPOINT_DIR, pattern = "^batch_\\d+\\.RData$", full.names = TRUE)
raw_list <- list(); spans_list <- list(); err_list <- character()
for (f in all_files) {
  e <- new.env(); load(f, envir = e)
  raw_list   <- c(raw_list,   list(e$batch_result$raw))
  spans_list <- c(spans_list, list(e$batch_result$spans))
  err_list   <- c(err_list,   e$batch_result$errors %||% character())
}
raw_gem   <- bind_rows(raw_list)
spans_gem <- bind_rows(spans_list)
write_parquet(raw_gem, "data/CMP/poplib_v4_gemini_flash_latest_raw.parquet")
message("  raw=", nrow(raw_gem), "  spans=", nrow(spans_gem), "  errors=", length(err_list))

message("\n=== Validating spans (regex + judge) ===")
chunk_text_lookup <- setNames(spans_gem$chunk_text, spans_gem$doc_id)
sp_for_check <- system_prompt_v4_safe("X", 2020L)
validated <- validate_spans_table_with_judge(
  spans_gem |> select(doc_id, doc_id_root, dim_name, quote, context),
  chunk_text_lookup, sp_for_check, TOPIC_KEYWORDS_V4,
  use_judge = TRUE, verbose = TRUE)
write_parquet(validated, "data/CMP/poplib_v4_gemini_flash_latest_validated_spans.parquet")

message("\n=== Aggregating ===")
qv <- validated |> group_by(doc_id, dim_name) |>
  summarise(quality_verified = any(quality_verified, na.rm = TRUE), .groups = "drop")
aggregate_dim_gem <- function(dim_name) {
  sc <- paste0(dim_name,"_score"); st <- paste0(dim_name,"_status")
  if (!(sc %in% names(raw_gem))) return(NULL)
  raw_gem |>
    select(doc_id, doc_id_root, score = all_of(sc), status = all_of(st)) |>
    left_join(qv |> filter(dim_name == !!dim_name) |> select(doc_id, quality_verified), by = "doc_id") |>
    mutate(quality_verified = coalesce(quality_verified, FALSE),
           eligible = quality_verified & status %in% c("verified","mixed") & !is.na(score)) |>
    group_by(doc_id_root) |>
    summarise("{dim_name}_score" := if (any(eligible)) mean(score[eligible], na.rm = TRUE) else NA_real_,
              .groups = "drop")
}
agg_gem <- reduce(compact(map(V4_DIMS, aggregate_dim_gem)), full_join, by = "doc_id_root") |>
  rename(doc_id = doc_id_root)
write_parquet(agg_gem, "data/CMP/poplib_v4_gemini_flash_latest_agg.parquet")
message("Saved data/CMP/poplib_v4_gemini_flash_latest_agg.parquet")
message("Done.")
