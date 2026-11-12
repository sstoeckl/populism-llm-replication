# ============================================================
# 50_Independence_3a_Sample.R
#
# TEST 3, step a — stratified subsample + ex-ante cost estimate.
#
# Sample (deterministic, seed 20261007):
#   N_GPT = 200 manifestos for GPT-4.1-mini, stratified by
#     populism tercile (baseline cross-model mean; equal allocation)
#     x decade (proportional), spreading across countries by sampling
#     within cells with weight 1 / (# manifestos of that country in cell).
#   ~85% single-chunk (<= 18K tokens, cheap and weighting-free),
#   ~15% multi-chunk with 2-3 chunks (exercises chunk aggregation).
#   N_CLAUDE = 60 nested sub-sample (20 per tercile, 50 single + 10 multi).
#
# Cost estimate uses exact tiktoken counts of system prompts + chunk text,
# assumed output tokens per call, and list prices (USD / 1M tokens):
#   gpt-4.1-mini-2025-04-14: 0.40 in / 1.60 out
#   claude-sonnet-4-6:       3.00 in / 15.00 out (x1.25 tokenizer factor)
# ============================================================

source("R/50_Independence_0_Setup.R")
source("R/50_Independence_3_Prompts.R")

SEED <- 20261007L
N_GPT <- 200L; N_GPT_MULTI <- 30L
N_CLAUDE <- 60L; N_CLAUDE_MULTI <- 10L
FULL_TEXT_TOKEN_CAP <- 18000L

base <- load_baseline()
txt  <- read_parquet("data/CMP/full_data.parquet",
                     col_select = c("doc_id", "text", "countryname", "date")) |>
  filter(!is.na(text)) |> distinct(doc_id, .keep_all = TRUE)
nch <- read_parquet("website/data/raw_gpt41mini.parquet", col_select = c("doc_id_root")) |>
  count(doc_id_root, name = "n_chunks")

pool <- base |>
  filter(is.finite(populism_overall), is.finite(liberalism_overall),
         is.finite(populism_overall_gpt41mini), is.finite(liberalism_overall_gpt41mini)) |>
  inner_join(nch, by = c("doc_id" = "doc_id_root")) |>
  semi_join(txt, by = "doc_id") |>
  mutate(pop_tercile = ntile(populism_overall, 3),
         decade = pmax(1950L, 10L * (year %/% 10L)))        # pool 1940s into 1950s
message("Pool: ", nrow(pool), " manifestos")

draw <- function(d, n_target, seed) {
  set.seed(seed)
  # proportional allocation to decade within tercile, equal across terciles;
  # largest-remainder rounding (tercile quotas differ by at most 1)
  quota <- diff(round(seq(0, n_target, length.out = 4)))
  alloc <- d |> count(pop_tercile, decade, name = "n_cell") |>
    group_by(pop_tercile) |>
    mutate(q = quota[pop_tercile],
           target = q * n_cell / sum(n_cell), k = floor(target), rem = target - k) |>
    arrange(pop_tercile, desc(rem)) |>
    mutate(k = k + as.integer(row_number() <= q - sum(floor(target)))) |>
    ungroup() |> select(pop_tercile, decade, k)
  d |> inner_join(alloc, by = c("pop_tercile", "decade")) |>
    add_count(pop_tercile, decade, countryname, name = "n_ctry_cell") |>
    group_by(pop_tercile, decade) |>
    group_modify(~ slice_sample(.x, n = min(.x$k[1], nrow(.x)), weight_by = 1 / .x$n_ctry_cell)) |>
    ungroup()
}

single <- pool |> filter(n_chunks == 1)
multi  <- pool |> filter(n_chunks %in% 2:3)
samp_gpt <- bind_rows(draw(single, N_GPT - N_GPT_MULTI, SEED) |> mutate(stratum = "single"),
                      draw(multi, N_GPT_MULTI, SEED + 1L) |> mutate(stratum = "multi")) |>
  select(doc_id, countryname, party, partyname, year, decade, pop_tercile, n_chunks, stratum)

samp_claude <- bind_rows(
  draw(pool |> semi_join(samp_gpt |> filter(stratum == "single"), by = "doc_id"),
       N_CLAUDE - N_CLAUDE_MULTI, SEED + 2L),
  draw(pool |> semi_join(samp_gpt |> filter(stratum == "multi"), by = "doc_id"),
       N_CLAUDE_MULTI, SEED + 3L)) |>
  pull(doc_id)
samp_gpt <- samp_gpt |> mutate(in_claude = doc_id %in% samp_claude)

message(sprintf("GPT sample: %d (%d multi) | countries: %d | Claude subsample: %d",
                nrow(samp_gpt), sum(samp_gpt$stratum == "multi"),
                n_distinct(samp_gpt$countryname), sum(samp_gpt$in_claude)))
print(count(samp_gpt, pop_tercile, stratum))
print(count(samp_gpt, decade))

# ---- Cost estimate ----
tok <- samp_gpt |>
  left_join(txt |> select(doc_id, text), by = "doc_id") |>
  mutate(chunk_tok = map(text, function(t) {
    est <- estimate_tokens(t)
    ch <- if (est <= FULL_TEXT_TOKEN_CAP) list(t) else as.list(chunk_text(t, 20000L, 2000L))
    map_int(ch, estimate_tokens)
  })) |>
  mutate(n_calls = lengths(chunk_tok), text_tok = map_dbl(chunk_tok, sum))

SYS_TOK <- c(joint = estimate_tokens(system_prompt_v4_safe("X", 2000)),
             pop   = estimate_tokens(system_prompt_pop_only("X", 2000)),
             lib   = estimate_tokens(system_prompt_lib_only("X", 2000)))
OUT_TOK <- c(joint = 1500, pop = 1000, lib = 900)       # conservative assumptions
PRICE <- list(gpt = c(inp = 0.40, out = 1.60), claude = c(inp = 3.00, out = 15.00))
CLAUDE_TOK_FACTOR <- 1.25

est_arm <- function(d, arm, model) {
  f <- if (model == "claude") CLAUDE_TOK_FACTOR else 1
  inp <- sum(d$text_tok + d$n_calls * (SYS_TOK[[arm]] + 30)) * f
  out <- sum(d$n_calls) * OUT_TOK[[arm]]
  tibble(model = model, arm = arm, n_docs = nrow(d), n_calls = sum(d$n_calls),
         input_Mtok = inp / 1e6, output_Mtok = out / 1e6,
         usd = inp / 1e6 * PRICE[[model]][["inp"]] + out / 1e6 * PRICE[[model]][["out"]])
}
cost <- bind_rows(
  est_arm(tok, "joint", "gpt"), est_arm(tok, "pop", "gpt"), est_arm(tok, "lib", "gpt"),
  est_arm(filter(tok, in_claude), "pop", "claude"), est_arm(filter(tok, in_claude), "lib", "claude"),
  est_arm(filter(tok, in_claude), "joint", "claude"))
cost <- bind_rows(cost, tibble(model = "TOTAL", arm = "", n_docs = NA, n_calls = sum(cost$n_calls),
                               input_Mtok = sum(cost$input_Mtok), output_Mtok = sum(cost$output_Mtok),
                               usd = sum(cost$usd)))
print(cost)
write_tab(cost, "t3_cost_estimate", digits = 3,
          caption = "Ex-ante cost estimate for separate-call rescoring.")
write_csv(samp_gpt, file.path(IND_CACHE, "t3_sample.csv"))
write_tab(samp_gpt |> count(countryname, name = "n") |> arrange(desc(n)), "t3_sample_countries", digits = 0)
message("Saved sample to ", file.path(IND_CACHE, "t3_sample.csv"))
