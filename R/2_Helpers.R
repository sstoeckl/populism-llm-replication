# ============================================================
# 2_Helpers.R
#
# Corpus-preparation helpers called by R/2_Manifesto_Prep.R (2025-1 corpus,
# Nov 2025) and R/30_CMP2026a_Prep.R (MPDS2026a / corpus 2026-1).
#
# PROVENANCE. R/2 called cmp_date_to_Date(), clean_manifesto_text(),
# estimate_token_count(), make_election_id() and make_doc_id(), but their
# definitions were never committed. They were reconstructed on 2026-10-07 in
# R/30_CMP2026a_Prep.R (then R/23_CMP_Update_Prep.R, lines 75-91) and verified
# there against the stored data/CMP/full_data.parquet:
#   - re-cleaning the 2026-1 corpus text reproduces 94.9% of the 3,327 stored
#     June texts byte for byte; the residual are texts the Manifesto Project
#     revised upstream (131 normalisation-only, 38 substantive, 3 re-codes);
#   - token_count is reproduced exactly where the text is identical.
# Extracted into this file on 2026-10-24 (Phase 2); the bodies are unchanged.
#
# make_election_id() follows the inline form used in R/30. make_doc_id() is
# only used for MPDS rows without a corpus document (no manifesto_id); those
# rows never enter the scoring pool (filter !is.na(manifesto_id)), so its
# exact original form does not affect any score.
# ============================================================

# CMP date YYYYMM -> first-of-month Date
cmp_date_to_Date <- function(x) as.Date(sprintf("%06d-01", as.integer(x)), format = "%Y%m-%d")

# Quasi-sentences joined by a space; bullets removed; runs of whitespace collapsed
# with base-R \\s (NBSP kept, no trimming, [n] footnote markers kept).
clean_manifesto_text <- function(x) {
  t <- gsub("•", "", paste(x, collapse = " "), fixed = TRUE)
  gsub("\\s+", " ", t)
}

# Rough word-based token proxy stored in full_data$token_count (0.75 x words).
# Not the chunking tokenizer: chunking uses estimate_tokens() (rtiktoken) in R/1.
estimate_token_count <- function(x) 0.75 * stringr::str_count(x, "\\S+")

# "Country Name" + Date -> "CountryName_YYYYMMDD"
make_election_id <- function(countryname, date) {
  paste0(gsub(" ", "", countryname), "_", format(as.Date(date), "%Y%m%d"))
}

# Fallback document id for MPDS rows without a corpus document
make_doc_id <- function(countryname = "", party, date_num) paste0(party, "_", date_num)
