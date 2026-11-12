# ============================================================
# 31_CMP2026a_Model_Probe.R
#
# Compatibility probe for the CMP update scoring run (R/32):
# checks that each production model ID used by R/15b / R/16 / R/18 (and the
# R/12 judge) is still served and records what the API reports it resolves
# to. gemini-flash-latest is a floating alias: the response's modelVersion
# and the models.get metadata identify the model currently behind it.
#
# Output: data/CMP/update_<VER>/model_probe.parquet (one row per model,
#         appended per run with a timestamp)
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(httr); library(jsonlite); library(arrow)
})

VER     <- Sys.getenv("CMP_UPDATE_VER", "2026a")
OUT     <- file.path("data/CMP", paste0("update_", VER), "model_probe.parquet")
PROBE_Q <- "Reply with the single word: ok"

safe_parse <- function(req) tryCatch(content(req, "parsed", type = "application/json"),
                                     error = function(e) list())

probe_anthropic <- function(model) {
  hdr <- add_headers("x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
                     "anthropic-version" = "2023-06-01", "content-type" = "application/json")
  meta <- safe_parse(GET(paste0("https://api.anthropic.com/v1/models/", model), hdr))
  body <- list(model = model, max_tokens = 5, temperature = 0,
               messages = list(list(role = "user", content = PROBE_Q)))
  req  <- POST("https://api.anthropic.com/v1/messages", hdr,
               body = toJSON(body, auto_unbox = TRUE), encode = "raw", timeout(60))
  ct <- safe_parse(req)
  tibble(provider = "anthropic", requested = model, http_status = status_code(req),
         resolved = ct$model %||% NA_character_,
         meta_id = meta$id %||% NA_character_,
         meta_info = paste0(meta$display_name %||% "", " | created ", meta$created_at %||% ""),
         error = if (status_code(req) >= 400) toJSON(ct$error, auto_unbox = TRUE) else NA_character_)
}

probe_openai <- function(model) {
  hdr <- add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
                     `Content-Type` = "application/json")
  meta <- safe_parse(GET(paste0("https://api.openai.com/v1/models/", model), hdr))
  body <- list(model = model, input = PROBE_Q, max_output_tokens = 16,
               temperature = 0, store = FALSE)
  req <- POST("https://api.openai.com/v1/responses", hdr,
              body = toJSON(body, auto_unbox = TRUE), encode = "raw", timeout(60))
  ct <- safe_parse(req)
  tibble(provider = "openai", requested = model, http_status = status_code(req),
         resolved = ct$model %||% NA_character_,
         meta_id = meta$id %||% NA_character_,
         meta_info = paste0("owned_by ", meta$owned_by %||% "", " | created ",
                            if (!is.null(meta$created)) format(as.POSIXct(meta$created, origin = "1970-01-01")) else ""),
         error = if (status_code(req) >= 400) toJSON(ct$error, auto_unbox = TRUE) else NA_character_)
}

probe_gemini <- function(model) {
  key  <- Sys.getenv("GEMINI_API_KEY")
  base <- "https://generativelanguage.googleapis.com/v1beta/models/"
  meta <- safe_parse(GET(paste0(base, model, "?key=", key)))
  body <- list(contents = list(list(parts = list(list(text = PROBE_Q)))),
               generationConfig = list(temperature = 0, maxOutputTokens = 16,
                                       thinkingConfig = list(thinkingBudget = 0)))
  req <- POST(paste0(base, model, ":generateContent?key=", key), content_type_json(),
              body = toJSON(body, auto_unbox = TRUE), timeout(60))
  ct <- safe_parse(req)
  tibble(provider = "google", requested = model, http_status = status_code(req),
         resolved = ct$modelVersion %||% NA_character_,
         meta_id = meta$name %||% NA_character_,
         meta_info = paste0(meta$displayName %||% "", " | version ", meta$version %||% "",
                            " | ", substr(meta$description %||% "", 1, 160)),
         error = if (status_code(req) >= 400) toJSON(ct$error, auto_unbox = TRUE) else NA_character_)
}

probe <- bind_rows(
  probe_anthropic("claude-sonnet-4-6"),           # R/15b MODEL_CLAUDE
  probe_openai("gpt-4.1-mini-2025-04-14"),        # R/16 MODEL
  probe_openai("gpt-4o-mini-2024-07-18"),         # R/12 judge
  probe_gemini("gemini-flash-latest")             # R/18 MODEL_GEMINI (floating alias)
) |> mutate(probed_at = Sys.time())

print(probe |> select(-meta_info), width = 200)
cat("\n"); walk2(probe$requested, probe$meta_info, ~ cat(.x, ":", .y, "\n"))

dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
if (file.exists(OUT)) probe <- bind_rows(read_parquet(OUT), probe)
write_parquet(probe, OUT)
