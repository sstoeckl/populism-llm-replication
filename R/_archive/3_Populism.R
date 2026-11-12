# ============================================================
# 3_Populism.R
#
# GPT-4o-mini scoring pipeline for populism and liberalism
# dimensions in CMP manifestos.
#
# Pipeline:
#   1. Source helper functions (R/1_Functions.R)
#   2. Load full_data.parquet
#   3. Define strict JSON schema (poplib_v2) and prompts
#   4. Score each manifesto via score_manifesto_doc()
#   5. Save individual RData files to data/CMP/raw/
#   6. Aggregate chunk-level results to manifesto level
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(furrr)
})
source("R/1_Functions.R")

# ---- Paths ----

base_dir <- "data/CMP"
ensure_dir(base_dir)
ensure_dir(file.path(base_dir, "raw"))

paths <- list(
  full          = file.path(base_dir, "full_data.parquet"),
  bundle_calls  = file.path(base_dir, "bundle_calls.parquet"),
  bundle_raw    = file.path(base_dir, "bundle_raw_json.parquet"),
  bundle_scores = file.path(base_dir, "bundle_scores.parquet"),
  offsets_pop   = file.path(base_dir, "offsets_pop.parquet"),
  offsets_lib   = file.path(base_dir, "offsets_lib.parquet"),
  adjusted_out  = file.path(base_dir, "poplib_adjusted.parquet"),
  manifesto_raw = file.path(base_dir, "poplib_manifesto_raw.parquet")
)

# ---- Load data ----

dat_raw <- safe_read_table(paths$full)

required_cols <- c(
  "doc_id", "party", "partyname", "countryname",
  "date_num", "date", "election_id", "text", "token_count"
)
missing_cols <- setdiff(required_cols, names(dat_raw))
if (length(missing_cols)) {
  stop("full_data missing columns: ", paste(missing_cols, collapse = ", "))
}

dat_raw$date <- as.Date(dat_raw$date)

dat <- dat_raw |>
  filter(!is.na(manifesto_id), !is.na(text))

message("Loaded ", nrow(dat), " manifestos with non-missing text.")

# ---- Model & token budget configuration ----

WORKERS <- max(1L, parallel::detectCores() - 1L)
# Pinned model snapshot for reproducibility (avoids silent upgrades)
MODEL   <- "gpt-4o-mini-2024-07-18"

# Context window budgeting (conservative for ~128k window)
MAX_CONTEXT_TOKENS    <- 120000L
COMPLETION_BUDGET_TOK <- 5000L
SAFETY_MARGIN_TOK     <- 2000L
PROMPT_BUDGET_TOK     <- MAX_CONTEXT_TOKENS - COMPLETION_BUDGET_TOK - SAFETY_MARGIN_TOK
FULL_TEXT_TOKEN_CAP   <- 18000L   # if text fits, send as single call

# Chunking defaults (used only when text exceeds FULL_TEXT_TOKEN_CAP)
CHUNK_TOKENS   <- 20000L
OVERLAP_TOKENS <- 2000L

# ---- Strict JSON schema (poplib_v2) ----
#
# Schema for structured output from OpenAI API.
# Defines scoring fields for:
#   - Populism (overall + 3 sub-dimensions, 0-10 integer)
#   - Populist ideology (overall + left/right/centrist, 0-10 integer)
#   - Liberalism (overall + 4 sub-dimensions, 0-10 integer)
#   - Extractive evidence (span arrays with verbatim quotes + offsets)
#   - Justifications (free-text, 1 sentence each)
#   - Confidence scores (0-100 integer)

# Schema building blocks
int10  <- function() list(type = "integer", minimum = 0, maximum = 10)
int100 <- function() list(type = "integer", minimum = 0, maximum = 100)

span_obj <- function() list(
  type = "object",
  properties = list(
    quote   = list(type = "string"),              # 3-8 words, verbatim from text
    start   = list(type = "integer", minimum = 0),# 0-based char offset (inclusive)
    end     = list(type = "integer", minimum = 0),# 0-based char offset (inclusive), end >= start+1
    context = list(type = "string")               # ~60 chars surrounding the quote
  ),
  required = c("quote", "start", "end", "context"),
  additionalProperties = FALSE
)

span_arr <- function(no) list(
  type = "array", minItems = no, maxItems = no, items = span_obj()
)

poplib_schema <- list(
  type = "object",
  properties = list(
    schema_version = list(type = "string", enum = list("poplib_v2")),
    doc_id         = list(type = "string"),
    status         = list(type = "string", enum = list("ok", "unscorable")),

    # Populism (0..10)
    populism_overall     = int10(),
    pop_anti_elitism     = int10(),
    pop_people_centrism  = int10(),
    pop_manichean        = int10(),

    # Populist ideology flavor (0..10 per channel + overall)
    pop_ideology_overall  = int10(),
    pop_ideology_left     = int10(),
    pop_ideology_right    = int10(),
    pop_ideology_centrist = int10(),

    # Liberalism (0..10)
    liberalism_overall    = int10(),
    lib_political         = int10(),
    lib_social            = int10(),
    lib_economic          = int10(),
    lib_financial_market  = int10(),

    # Extractive evidence: verbatim spans with offsets + context
    pop_key_phrases          = span_arr(3),   # 3 overall populism quotes
    pop_ideology_key_phrases = span_arr(3),   # 1x left, 1x right, 1x centrist
    lib_key_phrases          = span_arr(4),   # 1x pol, 1x soc, 1x eco, 1x fin

    # Sub-dimension spans: populism (3 each)
    pop_ae_spans = span_arr(3),  # anti-elitism
    pop_pc_spans = span_arr(3),  # people-centrism
    pop_mw_spans = span_arr(3),  # manichaean worldview

    # Sub-dimension spans: liberalism (1 each)
    lib_pol_spans = span_arr(1),
    lib_soc_spans = span_arr(1),
    lib_eco_spans = span_arr(1),
    lib_fin_spans = span_arr(1),

    # Justifications (1 concise sentence each)
    pop_justification          = list(type = "string"),
    pop_ideology_justification = list(type = "string"),
    lib_justification          = list(type = "string"),

    # Confidence (0..100)
    populism_confidence       = int100(),
    pop_ideology_confidence   = int100(),
    liberalism_confidence     = int100()
  ),
  required = c(
    "schema_version", "doc_id", "status",
    "populism_overall", "pop_anti_elitism", "pop_people_centrism", "pop_manichean",
    "pop_ideology_overall", "pop_ideology_left", "pop_ideology_right", "pop_ideology_centrist",
    "liberalism_overall", "lib_political", "lib_social", "lib_economic", "lib_financial_market",
    "pop_key_phrases", "pop_ideology_key_phrases", "lib_key_phrases",
    "pop_ae_spans", "pop_pc_spans", "pop_mw_spans",
    "lib_pol_spans", "lib_soc_spans", "lib_eco_spans", "lib_fin_spans",
    "pop_justification", "pop_ideology_justification", "lib_justification",
    "populism_confidence", "pop_ideology_confidence", "liberalism_confidence"
  ),
  additionalProperties = FALSE
)

# ---- Prompt templates ----

system_prompt_fn <- function(country, year) paste(
  "You are an expert political text coder. Read the whole TEXT holistically.",
  "Classify POPULISM (3 sub-dimensions), POPULIST IDEOLOGY (left/right/centrist), and LIBERALISM (4 sub-dimensions).",
  "Return STRICT JSON exactly matching the provided json_schema. Do not add or rename fields.",
  "",
  "MULTILINGUAL & CONTEXT:",
  "• The manifesto can be in German, English, French, Italian or other European languages.",
  "• Infer meaning from context (negation, irony, stance) – do NOT rely on keywords.",
  "• Quote detection must be semantic: recognize who speaks about whom (people vs elite, institutions, minorities, markets, etc.).",
  "",
  "POPULISM (0..10 each; POPPA-aligned):",
  "• Anti-elitism: elites/establishment framed as corrupt, self-serving, out of touch, or enemies of 'the people'.",
  "• People-centrism: 'the people' invoked as a unified, virtuous in-group whose will must prevail.",
  "• Manichaean worldview: moralized good-vs-evil struggle; betrayal, treason, conspiracy, existential stakes.",
  "• populism_overall = rounded mean of the three subscores, adjusted ±1 if breadth/centrality clearly warrants.",
  "",
  "POPULIST IDEOLOGY (0..10 each):",
  "• pop_ideology_left: left-populist framing (e.g., anti-corporate elite, redistribution, social equality).",
  "• pop_ideology_right: national/identity protectionism, anti-immigration, cultural threat narratives.",
  "• pop_ideology_centrist: deliberate non-positioning left/right; generic people–elite appeals without clear programmatic stance.",
  "• pop_ideology_overall = your overall strength of populist ideological flavor.",
  "",
  "LIBERALISM (0..10 each, do not infer one from another):",
  "• lib_political: democracy, rule of law, checks & balances, media freedom, due process, individual political rights.",
  "• lib_social: civil rights, minority protections, gender/LGBTQ+ equality, pluralism/tolerance.",
  "• lib_economic: free markets, competition, free enterprise, deregulation/privatization, tax efficiency, trade openness.",
  "• lib_financial_market: market-based finance, prudential oversight, investor/creditor rights, transparent rules.",
  "• liberalism_overall = rounded mean of the four liberal subscores.",
  "",
  "EXTRACTIVE EVIDENCE (MANDATORY):",
  "• For each spans field, return EXACTLY the required number of quotes, 3–8 words, verbatim from TEXT, with 0-based {start,end} char offsets and a short 'context' (~±60 chars).",
  "• Quotes must be distinct and non-overlapping. No paraphrase, no invented text.",
  "• Provide:",
  "  – pop_ae_spans: 3 quotes for anti-elitism;",
  "  – pop_pc_spans: 3 quotes for people-centrism;",
  "  – pop_mw_spans: 3 quotes for manichaean worldview;",
  "  – lib_pol_spans/lib_soc_spans/lib_eco_spans/lib_fin_spans: 1 quote each;",
  "  – pop_key_phases: 3 strongest overall populism quotes;",
  "  – pop_ideology_key_phrases: 3 quotes: one LEFT, one RIGHT, one CENTRIST framing.",
  "",
  "JUSTIFICATIONS:",
  "• pop_justification, pop_ideology_justification, lib_justification: 1 crisp sentence each, grounded in the extracted quotes, explaining the scores.",
  "",
  "REFUSAL / UNSCORABLE:",
  "• If any required spans field cannot be populated with valid verbatim quotes (3–8 words), set status=\"unscorable\" and set ALL numeric scores and confidences to 0. Still return arrays with placeholder empty spans to satisfy the schema.",
  "",
  "CONFIDENCE (0..100):",
  "• High (80–100) if multiple, clear cues across the text; Medium (40–79); Low (0–39) if weak/ambiguous.",
  "",
  "OUTPUT HYGIENE:",
  "• Use only the provided TEXT; do not import external facts.",
  "• Return STRICT JSON for the provided json_schema (no extra keys).",
  "",
  paste0("META: Country=", country, "; ElectionYear=", year, ". Base scoring solely on textual evidence.")
)

user_prompt_fn <- function(chunk_text, doc_id) {
  paste0("DOC_ID: ", doc_id, "\nTEXT:\n", chunk_text)
}

# ---- Scoring worker ----

worker <- function(i) {
  row <- dat[i, ]
  cache_path <- sprintf("data/CMP/raw/%s.RData", row$doc_id)

  # Skip if already scored
  if (file.exists(cache_path)) {
    load(cache_path)
    return(out)
  }

  message(sprintf("[%d/%d] Scoring %s (%s, %s)",
                  i, nrow(dat), row$doc_id, row$countryname, row$date))

  score_manifesto_doc(
    text              = row$text,
    doc_id            = row$doc_id,
    system_prompt     = system_prompt_fn(row$countryname, year(row$date)),
    user_prompt_fn    = user_prompt_fn,
    json_schema       = poplib_schema,
    full_text_token_cap = FULL_TEXT_TOKEN_CAP,
    chunk_tokens      = CHUNK_TOKENS,
    overlap_tokens    = OVERLAP_TOKENS,
    model             = MODEL,
    max_output_tokens = COMPLETION_BUDGET_TOK
  )
}

safe_worker <- possibly(worker, otherwise = NULL, quiet = FALSE)

# ---- Execute scoring loop ----

plan(sequential)   # switch to plan(multisession, workers = WORKERS) for parallel
message("Scoring ", nrow(dat), " manifestos (", WORKERS, " cores available)...")

results <- future_map(
  seq_len(nrow(dat)),
  safe_worker,
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

# Save full results list for debugging / restart
results_path <- file.path(base_dir, "poplib_results.RData")
save(results, file = results_path)
message("Raw results saved to ", results_path)

# ---- Aggregate results ----

# Extract chunk-level (raw) and manifesto-level (agg) tibbles
raw_list <- map(results, "raw")
agg_list <- map(results, "agg")

raw_bind <- bind_rows(raw_list)
agg_bind <- bind_rows(agg_list)

message("Aggregation complete: ",
        nrow(raw_bind), " chunk-level rows, ",
        nrow(agg_bind), " manifesto-level rows.")

# Confidence-weighted aggregation from chunks to manifesto level
pop_score_cols <- c("populism_overall", "pop_anti_elitism",
                    "pop_people_centrism", "pop_manichean")
lib_score_cols <- c("liberalism_overall", "lib_political",
                    "lib_social", "lib_economic", "lib_financial_market")
ideo_score_cols <- c("pop_ideology_overall", "pop_ideology_left",
                     "pop_ideology_right", "pop_ideology_centrist")

raw_agg <- raw_bind |>
  group_by(doc_id_root) |>
  summarise(
    across(all_of(pop_score_cols),
           ~ weighted.mean(.x, populism_confidence, na.rm = TRUE)),
    across(all_of(lib_score_cols),
           ~ weighted.mean(.x, liberalism_confidence, na.rm = TRUE)),
    across(all_of(ideo_score_cols),
           ~ weighted.mean(.x, pop_ideology_confidence, na.rm = TRUE)),
    across(contains("confidence"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  # Ideology direction index: positive = right-leaning, negative = left-leaning
  mutate(
    pop_ideology_direction = (pop_ideology_right - pop_ideology_left) /
      (pop_ideology_right + pop_ideology_left + pop_ideology_centrist)
  ) |>
  left_join(
    dat |> select(doc_id, date, countryname, party, partyname),
    by = c("doc_id_root" = "doc_id")
  ) |>
  select(doc_id = doc_id_root, date, countryname, partyname,
         populism_overall, liberalism_overall, pop_ideology_overall,
         everything()) |>
  arrange(countryname, date)

message("Final aggregated dataset: ", nrow(raw_agg), " manifestos.")

# ---- Save aggregated output ----

writexl::write_xlsx(raw_agg, file.path(base_dir, "poplib_agg_results.xlsx"))
message("Aggregated results saved to ", file.path(base_dir, "poplib_agg_results.xlsx"))
