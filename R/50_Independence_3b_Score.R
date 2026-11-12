# ============================================================
# 50_Independence_3b_Score.R
#
# TEST 3, step b — separate-call rescoring of the stratified subsample.
#
# Arms (model x prompt):
#   gpt41mini x {joint, pop, lib}  on all 200 manifestos
#   sonnet    x {pop, lib, joint}  on the nested 60
#   joint = production v4-safe prompt (test-retest noise floor)
#   pop   = populism + populist ideology only (no liberalism text)
#   lib   = liberalism only (no populism / ideology text)
#
# Identical to production: pinned models, temperature 0 (top_p 1 for
# OpenAI), Responses API / Messages tool_use, strict schema (subset),
# chunking (<= 18K tokens => 1 chunk; else 20K windows, 2K overlap),
# user prompt "DOC_ID: ..\nTEXT:\n..", 5 retries with backoff.
#
# Crash safety: one RDS per (arm, manifesto) under
#   output/independence/cache/t3_raw/<model>_<arm>/<doc_id>.rds
# re-running skips finished manifestos. Actual cost is tallied from the
# API usage fields; the run stops before an arm if the projected total
# exceeds BUDGET_USD.
#
# Usage:  Rscript R/50_Independence_3b_Score.R [arm ...]
#         (default: all arms in ARMS order)
# ============================================================

source("R/50_Independence_0_Setup.R")
source("R/50_Independence_3_Prompts.R")
suppressPackageStartupMessages({ library(furrr); library(jsonlite); library(httr) })

BUDGET_USD <- 25
MODEL_GPT    <- "gpt-4.1-mini-2025-04-14"
MODEL_CLAUDE <- "claude-sonnet-4-6"
PRICE <- list(gpt41mini = c(inp = 0.40, out = 1.60), sonnet = c(inp = 3.00, out = 15.00))
WORKERS <- c(gpt41mini = 4L, sonnet = 2L)        # modest: keys are shared
FULL_TEXT_TOKEN_CAP <- 18000L; CHUNK_TOKENS <- 20000L; OVERLAP_TOKENS <- 2000L
MAX_RETRIES <- 5L; BASE_SLEEP <- 3

ARMS <- tribble(
  ~arm_id,            ~model,      ~prompt,
  "gpt41mini_joint",  "gpt41mini", "joint",
  "gpt41mini_pop",    "gpt41mini", "pop",
  "gpt41mini_lib",    "gpt41mini", "lib",
  "sonnet_pop",       "sonnet",    "pop",
  "sonnet_lib",       "sonnet",    "lib",
  "sonnet_joint",     "sonnet",    "joint"
)
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) ARMS <- ARMS |> filter(arm_id %in% args)

RAW_DIR <- file.path(IND_CACHE, "t3_raw")

prompt_fun <- list(joint = system_prompt_v4_safe, pop = system_prompt_pop_only, lib = system_prompt_lib_only)
schema_of  <- list(joint = poplib_v4_schema, pop = poplib_v4_poponly_schema, lib = poplib_v4_libonly_schema)
dims_of    <- list(joint = c(POP_ONLY_DIMS, LIB_ONLY_DIMS), pop = POP_ONLY_DIMS, lib = LIB_ONLY_DIMS)

# ---- Claude call (mirrors R/15b anthropic_v4_chat; schema passed in) ----
strip_null_types <- function(node) {
  if (is.list(node)) {
    if (!is.null(node$type) && is.character(node$type) && all(node$type %in% c("integer", "null")))
      node$type <- "integer"
    for (nm in names(node)) node[[nm]] <- strip_null_types(node[[nm]])
  }
  node
}
anthropic_chat_schema <- function(system_prompt, user_prompt, schema, model = MODEL_CLAUDE,
                                  max_output_tokens = 5000, timeout_s = 180) {
  tool <- list(name = "submit_poplib_v4",
               description = "Submit populism/liberalism scores under the v4 schema with per-dimension evidence_status. Use null for score when evidence_status is no_evidence.",
               input_schema = strip_null_types(schema))
  body <- list(model = model, max_tokens = max_output_tokens, temperature = 0,
               system = system_prompt, tools = list(tool),
               tool_choice = list(type = "tool", name = "submit_poplib_v4"),
               messages = list(list(role = "user", content = user_prompt)))
  req <- httr::POST("https://api.anthropic.com/v1/messages",
    httr::add_headers("x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"), "anthropic-version" = "2023-06-01",
                      "content-type" = "application/json"),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    encode = "raw", httr::timeout(timeout_s))
  if (httr::status_code(req) >= 400)
    stop("Anthropic API error ", httr::status_code(req), ": ",
         substr(httr::content(req, "text", encoding = "UTF-8"), 1, 300))
  cont <- httr::content(req, "parsed", type = "application/json")
  tu <- NULL
  for (b in cont$content) if (identical(b$type, "tool_use")) { tu <- b; break }
  if (is.null(tu)) stop("Claude did not return a tool_use block.")
  list(parsed = tu$input,
       usage = list(input_tokens = cont$usage$input_tokens %||% NA, output_tokens = cont$usage$output_tokens %||% NA))
}

# ---- One chunk, with retry; returns parsed JSON + token usage ----
score_chunk_arm <- function(chunk, doc_id_in, country, year, model, prompt) {
  sysp <- prompt_fun[[prompt]](country, year)
  usp  <- user_prompt_v4(chunk, doc_id_in)
  last_err <- "unknown"
  for (attempt in seq_len(MAX_RETRIES)) {
    res <- tryCatch({
      if (model == "gpt41mini") {
        r <- suppressMessages(oa_chat_json(system_prompt = sysp, user_prompt = usp, model = MODEL_GPT,
                                           json_schema = schema_of[[prompt]], temperature = 0, top_p = 1,
                                           store = TRUE, max_output_tokens = 5000L))
        list(parsed = jsonlite::fromJSON(r$text, simplifyVector = FALSE),
             usage = list(input_tokens = r$usage$input_tokens %||% NA, output_tokens = r$usage$output_tokens %||% NA))
      } else {
        anthropic_chat_schema(sysp, usp, schema_of[[prompt]])
      }
    }, error = function(e) { last_err <<- conditionMessage(e); NULL })
    if (!is.null(res$parsed)) return(c(res, attempts = attempt))
    Sys.sleep(BASE_SLEEP * 2^(attempt - 1))
  }
  list(parsed = NULL, usage = list(input_tokens = 0, output_tokens = 0), error = last_err)
}

# Flatten parsed JSON (same column layout as production raw parquet)
flatten_arm <- function(parsed, dims, chunk_doc_id, doc_id_root, chunk_id) {
  row <- tibble(doc_id = chunk_doc_id, doc_id_root = doc_id_root, chunk_id = chunk_id,
                schema_version = as.character(parsed$schema_version %||% NA))
  for (d in dims) {
    obj <- parsed[[d]]
    if (is.list(obj)) {
      row[[paste0(d, "_score")]]   <- if (is.null(obj$score)) NA_real_ else as.numeric(obj$score)
      row[[paste0(d, "_status")]]  <- as.character(obj$evidence_status %||% NA)
      row[[paste0(d, "_quote")]]   <- as.character(obj$span$quote %||% NA)
      row[[paste0(d, "_context")]] <- as.character(obj$span$context %||% NA)
    } else if (length(obj) > 0L && !is.na(suppressWarnings(as.numeric(obj[1])))) {
      # Claude occasionally returns a flat integer (handled as in R/15b)
      row[[paste0(d, "_score")]] <- suppressWarnings(as.numeric(obj[1]))
      row[[paste0(d, "_status")]] <- "verified"
      row[[paste0(d, "_quote")]] <- NA_character_; row[[paste0(d, "_context")]] <- NA_character_
    } else {
      row[[paste0(d, "_score")]] <- NA_real_; row[[paste0(d, "_status")]] <- NA_character_
      row[[paste0(d, "_quote")]] <- NA_character_; row[[paste0(d, "_context")]] <- NA_character_
    }
  }
  for (cf in c("populism_confidence", "pop_ideology_confidence", "liberalism_confidence"))
    row[[cf]] <- as.numeric(parsed[[cf]] %||% NA)
  for (jf in c("pop_justification", "pop_ideology_justification", "lib_justification"))
    row[[jf]] <- as.character(parsed[[jf]] %||% NA)
  row
}

score_manifesto_arm <- function(doc_id_root, text, country, year, model, prompt, out_file) {
  if (file.exists(out_file)) return(invisible(NULL))
  est <- tryCatch(estimate_tokens(text), error = function(e) Inf)
  chunks <- if (is.finite(est) && est <= FULL_TEXT_TOKEN_CAP) list(text)
            else as.list(chunk_text(text, CHUNK_TOKENS, OVERLAP_TOKENS))
  if (length(chunks) == 0L) chunks <- list(text)
  rows <- list(); usage <- tibble(); errors <- character()
  for (ci in seq_along(chunks)) {
    cid <- paste0(doc_id_root, "_ch", ci)
    sr <- score_chunk_arm(chunks[[ci]], cid, country, year, model, prompt)
    usage <- bind_rows(usage, tibble(chunk_id = ci, input_tokens = as.numeric(sr$usage$input_tokens %||% NA),
                                     output_tokens = as.numeric(sr$usage$output_tokens %||% NA)))
    if (is.null(sr$parsed)) {
      errors <- c(errors, paste0(cid, ": ", sr$error))
      rows[[ci]] <- tibble(doc_id = cid, doc_id_root = doc_id_root, chunk_id = ci, scoring_error = sr$error)
      next
    }
    rows[[ci]] <- flatten_arm(sr$parsed, dims_of[[prompt]], cid, doc_id_root, ci) |>
      mutate(chunk_text = chunks[[ci]])
  }
  res <- list(raw = bind_rows(rows), usage = usage, errors = errors, timestamp = Sys.time())
  # only persist complete successes so failures are retried on the next run
  if (!length(errors)) saveRDS(res, out_file)
  invisible(res)
}

arm_cost <- function(arm_dir, model) {
  fs <- list.files(arm_dir, pattern = "\\.rds$", full.names = TRUE)
  if (!length(fs)) return(0)
  u <- map_dfr(fs, ~ readRDS(.x)$usage)
  sum(u$input_tokens, na.rm = TRUE) / 1e6 * PRICE[[model]][["inp"]] +
    sum(u$output_tokens, na.rm = TRUE) / 1e6 * PRICE[[model]][["out"]]
}

# ---- Data ----
samp <- read_csv(file.path(IND_CACHE, "t3_sample.csv"), show_col_types = FALSE)
txt  <- read_parquet("data/CMP/full_data.parquet",
                     col_select = c("doc_id", "text", "countryname", "date")) |>
  filter(!is.na(text)) |> distinct(doc_id, .keep_all = TRUE) |>
  mutate(year = lubridate::year(as.Date(date)))
work <- samp |> select(doc_id, in_claude) |> inner_join(txt, by = "doc_id")
# Smoke test: SMOKE_N=2 restricts to the first N Claude-sample manifestos
SMOKE_N <- as.integer(Sys.getenv("SMOKE_N", "0"))
if (SMOKE_N > 0) work <- work |> filter(in_claude) |> slice_head(n = SMOKE_N)

# Ex-ante estimate per arm (from 3a) for budget projection
est <- read_csv(file.path(IND_TAB, "t3_cost_estimate.csv"), show_col_types = FALSE) |>
  filter(model != "TOTAL") |>
  mutate(arm_id = paste0(if_else(model == "gpt", "gpt41mini", "sonnet"), "_", arm))

for (a in seq_len(nrow(ARMS))) {
  arm <- ARMS[a, ]
  arm_dir <- file.path(RAW_DIR, arm$arm_id); dir.create(arm_dir, recursive = TRUE, showWarnings = FALSE)
  # spend across all arms on disk (also those not requested this run)
  all_arm_dirs <- list.dirs(RAW_DIR, recursive = FALSE)
  spent_all <- sum(map_dbl(all_arm_dirs, ~ arm_cost(.x, if (grepl("^sonnet", basename(.x))) "sonnet" else "gpt41mini")))
  todo <- work |> filter(arm$model == "gpt41mini" | in_claude) |>
    filter(!file.exists(file.path(arm_dir, paste0(doc_id, ".rds"))))
  proj <- est$usd[est$arm_id == arm$arm_id] * nrow(todo) /
          max(1, nrow(work |> filter(arm$model == "gpt41mini" | in_claude)))
  message(sprintf("\n=== Arm %s: %d to score | spent so far $%.2f | projected for arm $%.2f ===",
                  arm$arm_id, nrow(todo), spent_all, proj))
  if (spent_all + proj > BUDGET_USD) {
    message("STOP: projected total exceeds budget of $", BUDGET_USD, ". Skipping remaining arms.")
    break
  }
  if (!nrow(todo)) next
  plan(multisession, workers = WORKERS[[arm$model]])
  t0 <- Sys.time()
  res <- future_map(seq_len(nrow(todo)), function(i) {
    r <- todo[i, ]
    tryCatch(score_manifesto_arm(r$doc_id, r$text, r$countryname, r$year, arm$model, arm$prompt,
                                 file.path(arm_dir, paste0(r$doc_id, ".rds"))),
             error = function(e) list(errors = conditionMessage(e)))
  }, .options = furrr_options(
    globals = c("todo", "arm", "arm_dir", "score_manifesto_arm", "score_chunk_arm", "flatten_arm",
                "anthropic_chat_schema", "strip_null_types", "oa_chat_json", "prompt_fun", "schema_of",
                "dims_of", "system_prompt_v4_safe", "system_prompt_pop_only", "system_prompt_lib_only",
                ".hdr_common", ".no_evidence_block", ".mixed_and_hallu_head", ".hallu_tail", ".pop_defs",
                ".lib_defs", ".output_head", ".conf_meta", "user_prompt_v4",
                "estimate_tokens", "chunk_text", "%||%", "MODEL_GPT", "MODEL_CLAUDE",
                "FULL_TEXT_TOKEN_CAP", "CHUNK_TOKENS", "OVERLAP_TOKENS", "MAX_RETRIES", "BASE_SLEEP"),
    packages = c("tibble", "dplyr", "purrr", "httr", "jsonlite", "rtiktoken", "lubridate"),
    seed = TRUE), .progress = FALSE)
  plan(sequential)
  n_err <- sum(map_int(res, ~ length(.x$errors %||% character())))
  message(sprintf("  arm %s done in %.1f min | manifestos with errors: %d | arm cost $%.2f",
                  arm$arm_id, as.numeric(Sys.time() - t0, units = "mins"), n_err,
                  arm_cost(arm_dir, arm$model)))
  if (n_err) print(unlist(map(res, "errors"))[1:min(5, n_err)])
}

# ---- Actual cost ledger ----
ledger <- map_dfr(list.dirs(RAW_DIR, recursive = FALSE), function(d) {
  m <- if (grepl("^sonnet", basename(d))) "sonnet" else "gpt41mini"
  fs <- list.files(d, pattern = "\\.rds$", full.names = TRUE)
  u <- map_dfr(fs, ~ readRDS(.x)$usage)
  tibble(arm = basename(d), n_manifestos = length(fs), n_calls = nrow(u),
         input_tokens = sum(u$input_tokens, na.rm = TRUE), output_tokens = sum(u$output_tokens, na.rm = TRUE),
         usd = arm_cost(d, m))
})
ledger <- bind_rows(ledger, tibble(arm = "TOTAL", n_manifestos = NA, n_calls = sum(ledger$n_calls),
                                   input_tokens = sum(ledger$input_tokens), output_tokens = sum(ledger$output_tokens),
                                   usd = sum(ledger$usd)))
print(ledger)
write_tab(ledger, "t3_cost_actual", digits = 2, caption = "Actual API usage and cost of separate-call rescoring.")
