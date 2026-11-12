# ============================================================
# 50_Independence_3_Prompts.R
#
# SEPARATE-CALL prompts + schemas for TEST 3. Derived from the production
# v4-safe prompt (R/3c_Populism_v4_safe.R::system_prompt_v4_safe) by
# DELETION ONLY, except for three minimal wording edits that are needed
# because the production text refers to the removed block (each marked
# [EDIT] below). Everything else -- rule text, definitions, META line,
# user prompt, schema objects, temperature 0, chunking -- is identical.
#
#   system_prompt_pop_only(): POPULISM (3 sub + overall) + POPULIST
#       IDEOLOGY. All liberalism definitions, liberalism examples and
#       the 'pro-liberal' score anchor are removed.
#   system_prompt_lib_only(): LIBERALISM (4 sub + overall). All populism
#       and populist-ideology definitions are removed.
#
# check_prompt_is_deletion() verifies programmatically that each
# derived prompt is a word-level subsequence of the production prompt
# apart from the marked edits.
# ============================================================

suppressPackageStartupMessages({ library(tidyverse); library(stringi) })
source("R/1_Functions.R")
source("R/3c_Populism_v4_safe.R")

POP_ONLY_DIMS <- c("pop_anti_elitism", "pop_people_centrism", "pop_manichean", "populism_overall",
                   "pop_ideology_left", "pop_ideology_right", "pop_ideology_centrist", "pop_ideology_overall")
LIB_ONLY_DIMS <- c("lib_political", "lib_social", "lib_economic", "lib_financial_market", "liberalism_overall")

.subset_schema <- function(keep, version) {
  s <- poplib_v4_schema
  s$properties <- s$properties[c("schema_version", "doc_id", keep)]
  s$properties$schema_version <- list(type = "string", enum = list(version))
  s$required <- c("schema_version", "doc_id", keep)
  s
}
poplib_v4_poponly_schema <- .subset_schema(
  c(POP_ONLY_DIMS, "pop_justification", "pop_ideology_justification",
    "populism_confidence", "pop_ideology_confidence"), "poplib_v4_poponly")
poplib_v4_libonly_schema <- .subset_schema(
  c(LIB_ONLY_DIMS, "lib_justification", "liberalism_confidence"), "poplib_v4_libonly")

# ---- Shared blocks (verbatim production text) ----
.hdr_common <- c(
  "MULTILINGUAL & CONTEXT:",
  "- The manifesto can be in any European language (German, English, French, Spanish, Italian, etc.).",
  "- Infer meaning from context (negation, irony, stance) - do NOT rely on keywords.",
  "",
  "===============================================================",
  "*** CRITICAL SCORING RULES (these are the most important rules) ***",
  "===============================================================",
  "",
  "For EACH dimension, return: { score, evidence_status, span }",
  "",
  "evidence_status:",
  "  'verified'    -> the manifesto explicitly discusses this dimension (pro or con).",
  "                   Provide a 3-8 word VERBATIM quote from TEXT that is topically",
  "                   ABOUT the dimension")

.no_evidence_block <- c(
  "",
  "  'no_evidence' -> the manifesto does NOT discuss this dimension at all.",
  "                   Set score = null (JSON null).",
  "                   Set span.quote = \"\" and span.context = \"\".",
  "                   *** ABSENCE OF EVIDENCE IS NOT EVIDENCE AGAINST. ***")

.mixed_and_hallu_head <- c(
  "",
  "  'mixed'       -> the manifesto contains BOTH pro- and anti-stance content for this",
  "                   dimension. Provide a 3-8 word verbatim quote illustrating the mixed",
  "                   stance. Set score in the 4-6 range.",
  "",
  "===============================================================",
  "*** ANTI-HALLUCINATION RULES — non-negotiable ***",
  "===============================================================",
  "",
  "Every quote MUST be VERBATIM TEXT from the manifesto, copied letter-for-letter.",
  "",
  "FORBIDDEN, will be flagged as a hallucination by post-processing:")

.hallu_tail <- c(
  "  - paraphrasing or summarizing the manifesto in your own words.",
  "  - quoting generic phrases like 'democracy, rule of law' unless those EXACT words",
  "    appear verbatim in the manifesto.",
  "",
  "If you cannot find a 3-8 word verbatim quote in the manifesto that is topically about",
  "the dimension, set evidence_status='no_evidence' and score=null. Do not fabricate.",
  "",
  "===============================================================",
  "DIMENSION DEFINITIONS (for scoring direction, NOT for quoting back)",
  "===============================================================",
  "")

.pop_defs <- c(
  "POPULISM (0..10 each; POPPA-aligned):",
  "- pop_anti_elitism: elites/establishment framed as corrupt or out of touch.",
  "- pop_people_centrism: 'the people' invoked as a unified, virtuous group.",
  "- pop_manichean: moralized good-vs-evil struggle; betrayal, conspiracy.",
  "- populism_overall: rounded mean of the three subscores, +/-1 if breadth warrants.",
  "",
  "POPULIST IDEOLOGY (0..10 each):",
  "- pop_ideology_left: anti-corporate elite, redistribution, social equality.",
  "- pop_ideology_right: national/identity protectionism, anti-immigration.",
  "- pop_ideology_centrist: generic people-elite appeals without clear programmatic stance.",
  "- pop_ideology_overall: overall strength of populist ideological flavor.",
  "")

.lib_defs <- c(
  "LIBERALISM (0..10 each). 'Liberalism' here is CLASSICAL/ORDO-LIBERAL, NOT libertarianism.",
  "Regulation that ENABLES market function is pro-liberal; regulation that SUBSTITUTES",
  "for the market is anti-liberal.",
  "",
  "- lib_political: democracy, rule of law, checks & balances, media freedom, due process,",
  "                 individual political rights.",
  "  Topic check: a verbatim quote for lib_political must mention democracy, rights,",
  "  elections, courts, media, government, parliament, or a related political-institution term.",
  "",
  "- lib_social: civil rights, minority protections, gender/LGBTQ+ equality, pluralism.",
  "  Topic check: the quote must mention rights, minorities, equality, discrimination,",
  "  gender/LGBTQ+, race, religion, or related civil-society terms.",
  "",
  "- lib_economic: free markets, competition, free enterprise, deregulation/privatization,",
  "                tax efficiency, trade openness.",
  "  Topic check: the quote must mention market, competition, business, enterprise, tax,",
  "  trade, regulation, privatization, or related economic-policy terms.",
  "",
  "- lib_financial_market: support for market-based pricing of assets and credit; open",
  "  competition in banking, insurance, and capital markets; free cross-border capital flows;",
  "  PRUDENTIAL rules that ENABLE market function (disclosure, anti-fraud, deposit insurance,",
  "  fair access for retail/small investors, property/contract enforcement); transparency.",
  "  Score LOW for: capital controls, financial-transaction taxes, nationalization of banks,",
  "  state-set interest rates, price controls on financial products.",
  "  CRITICAL: investor protection, prudential regulation, and transparency are PRO-LIBERAL",
  "  ONLY when they ENABLE market-based finance. They must not be conflated with restrictions",
  "  on market pricing or capital flows.",
  "  Topic check: the quote must mention finance, bank, capital, credit, invest, market,",
  "  monetary, interest rate, currency, debt, securities, or related financial-system terms.",
  "  If the manifesto does NOT discuss financial-market policy at all, return",
  "  evidence_status='no_evidence' and score=null.",
  "",
  "- liberalism_overall: rounded mean of the four sub-dimension scores that have",
  "  evidence_status != 'no_evidence'. If ALL four sub-dimensions are no_evidence, set",
  "  liberalism_overall.evidence_status='no_evidence' and score=null.",
  "")

.output_head <- c(
  "===============================================================",
  "OUTPUT SHAPE",
  "===============================================================",
  "",
  "Each dimension is an OBJECT with three fields: { score, evidence_status, span }.",
  "score is an INTEGER (0..10) OR null. Use JSON null, not 'NA' or 0.",
  "span is { quote, context } — verbatim text + ~60-character surrounding context.",
  "")

.conf_meta <- function(country, year) c(
  "",
  "CONFIDENCE (0..100): High (80-100) if multiple, clear cues; Medium (40-79); Low (0-39).",
  "Confidence on a dimension that returned no_evidence should reflect confidence that the",
  "manifesto really has no relevant content (high confidence = sure it's absent).",
  "",
  paste0("META: Country=", country, "; ElectionYear=", year, ". Score solely from textual evidence."))

system_prompt_pop_only <- function(country, year) do.call(paste, as.list(c(
  "You are an expert political text coder. Read the whole TEXT holistically.",
  "Classify POPULISM (3 sub-dimensions + overall) and POPULIST IDEOLOGY (left/right/centrist + overall).",  # [EDIT] liberalism removed from task line
  "Return STRICT JSON matching the json_schema exactly.",
  "",
  .hdr_common[-length(.hdr_common)],
  "                   ABOUT the dimension.",                                         # [EDIT] liberalism topic examples removed
  "                   Set score to a 0-10 integer reflecting the stance.",           # [EDIT] '(10=fully pro-liberal, 0=fully anti-liberal)' removed
  .no_evidence_block,
  "                   Do NOT score this dimension as 0 just because the",            # [EDIT] 'anti-liberal' removed
  "                   manifesto is silent on it. Return null (no_evidence) instead.",
  .mixed_and_hallu_head,
  "  - quoting the category DEFINITION as if it were evidence.",                     # [EDIT] liberalism example labels removed
  .hallu_tail,
  .pop_defs,
  .output_head,
  "JUSTIFICATIONS: pop_justification, pop_ideology_justification —",
  "1 crisp sentence each, grounded in the verified quotes (NOT in the prompt definitions).",
  .conf_meta(country, year))))

system_prompt_lib_only <- function(country, year) do.call(paste, as.list(c(
  "You are an expert political text coder. Read the whole TEXT holistically.",
  "Classify LIBERALISM (4 sub-dimensions + overall). Return STRICT JSON matching the json_schema exactly.",  # [EDIT] populism removed from task line
  "",
  .hdr_common[-length(.hdr_common)],
  "                   ABOUT the dimension (e.g., for lib_financial_market, the quote must",
  "                   mention finance, banking, capital, credit, investment, or financial",
  "                   markets; for lib_social, it must mention civil rights, minorities,",
  "                   discrimination, equality, or gender/LGBTQ+).",
  "                   Set score to a 0-10 integer reflecting the stance (10=fully pro-liberal,",
  "                   0=fully anti-liberal).",
  .no_evidence_block,
  "                   Do NOT score this dimension as 0 (anti-liberal) just because the",
  "                   manifesto is silent on it. Return null (no_evidence) instead.",
  "                   Example: a manifesto about education policy with no mention of",
  "                   banking/finance -> lib_financial_market.evidence_status='no_evidence'",
  "                   and lib_financial_market.score=null.",
  .mixed_and_hallu_head,
  "  - quoting the category DEFINITION as if it were evidence",
  "    (e.g., quoting 'market-based finance, prudential oversight' or 'civil rights,",
  "    minority protections' — those are MY category labels, NOT manifesto text).",
  .hallu_tail,
  .lib_defs,
  .output_head,
  "JUSTIFICATIONS: lib_justification —",
  "1 crisp sentence, grounded in the verified quotes (NOT in the prompt definitions).",
  .conf_meta(country, year))))

# Word-level check via longest common subsequence: returns the words of
# the derived prompt that are NOT part of the LCS with the production
# prompt (should be only the [EDIT] fragments).
check_prompt_is_deletion <- function(derived, production = system_prompt_v4_safe("X", 2000)) {
  d <- strsplit(derived, "\\s+")[[1]]; p <- strsplit(production, "\\s+")[[1]]
  n <- length(d); m <- length(p)
  L <- matrix(0L, n + 1, m + 1)
  for (i in n:1) for (j in m:1)
    L[i, j] <- if (d[i] == p[j]) L[i + 1, j + 1] + 1L else max(L[i + 1, j], L[i, j + 1])
  i <- 1; j <- 1; extra <- character()
  while (i <= n && j <= m) {
    if (d[i] == p[j]) { i <- i + 1; j <- j + 1 }
    else if (L[i + 1, j] >= L[i, j + 1]) { extra <- c(extra, d[i]); i <- i + 1 }
    else j <- j + 1
  }
  c(extra, if (i <= n) d[i:n])
}
