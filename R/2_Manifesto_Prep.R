# ============================================================
# 2_Manifesto_Prep.R
#
# Load CMP (Comparative Manifesto Project) data via manifestoR,
# extract manifesto text and token counts, join with election
# metadata, and save to Parquet.
#
# Requires:
#   - MANIFESTO_API_KEY in .Renviron
#   - MP_EMAIL / MP_PASSWORD in .Renviron (optional, for PDF download)
#   - source("R/1_Functions.R") for helper functions
#
# Output:
#   - data/CMP/mpds.parquet           (election metadata)
#   - data/CMP/full_corpus_df.parquet (corpus-level metadata)
#   - data/CMP/full_data.parquet      (final joined dataset)
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(manifestoR)
  library(lubridate)
})

source("R/1_Functions.R")

# ---- Config ----

manifestoR::mp_setapikey(Sys.getenv("MANIFESTO_API_KEY"))

base_dir <- "data/CMP"
ensure_dir(base_dir)

paths <- list(
  mpds     = file.path(base_dir, "mpds.parquet"),
  corpusdf = file.path(base_dir, "full_corpus_df.parquet"),
  full     = file.path(base_dir, "full_data.parquet"),
  pdf_dir  = file.path(base_dir, "pdfs"),
  pdf_log  = file.path(base_dir, "pdf_download_log.csv")
)

# ---- 1. Load MPDS (Main Dataset) ----

message("Loading Manifesto main dataset ...")
mpds <- manifestoR::mp_maindataset(cache = TRUE)

# Drop the per* percentage columns -- keep only election/party metadata
el_data <- mpds %>%
  select(-contains("per")) %>%
  rename(date_num = date) %>%
  # CMP date format (e.g. 196009) -> proper Date column
  mutate(date = cmp_date_to_Date(date_num))

safe_write_table(el_data, paths$mpds)
message("  Saved: ", paths$mpds, " (", nrow(el_data), " rows)")

# ---- 2. Load Corpora ----

message("Loading corpora ...")
full_corpus    <- manifestoR::mp_corpus(mpds, cache = TRUE)
full_corpus_df <- manifestoR::mp_corpus_df(mpds, cache = TRUE, tibble_metadata = "all")

safe_write_table(full_corpus_df, paths$corpusdf)
message("  Saved: ", paths$corpusdf)

# ---- 3. Extract Text and Token Counts ----

message("Extracting text from ", length(full_corpus), " manifesto documents ...")
pb <- txtProgressBar(min = 0, max = length(full_corpus), style = 3)

data_rows <- vector("list", length(full_corpus))
for (i in seq_along(full_corpus)) {
  setTxtProgressBar(pb, i)

  m <- meta(full_corpus[[i]])

  # Lookup manifesto_id, id, url_original from corpus metadata
  meta_df <- full_corpus_df %>%
    filter(date == m$date, party == m$party) %>%
    select(manifesto_id:id) %>%
    distinct()

  text_i <- clean_manifesto_text(manifestoR::content(full_corpus[[i]]))
  tok_i  <- estimate_token_count(text_i)

  # Build doc_id: prefer manifesto_id, fall back to party_date
  did <- if ("manifesto_id" %in% names(meta_df)) {
    meta_df$manifesto_id[1]
  } else {
    make_doc_id(countryname = "", party = m$party, date_num = m$date)
  }

  url_orig <- if ("url_original" %in% names(meta_df)) meta_df$url_original[1] else NA_character_

  data_rows[[i]] <- tibble(
    manifesto_id = if ("manifesto_id" %in% names(meta_df)) meta_df$manifesto_id[1] else NA_character_,
    id           = if ("id" %in% names(meta_df)) meta_df$id[1] else NA_character_,
    party        = m$party,
    date_num     = m$date,
    text         = text_i,
    token_count  = tok_i,
    pdf_url      = url_orig,
    doc_id       = did
  )
}
close(pb)

data_tbl <- bind_rows(data_rows)
message("  Extracted ", nrow(data_tbl), " documents")

# ---- 4. Join Metadata with Text ----

full_data <- el_data %>%
  left_join(data_tbl, by = c("party", "date_num")) %>%
  mutate(
    date        = cmp_date_to_Date(date_num),
    election_id = make_election_id(countryname, date),
    # Fallback doc_id if missing
    doc_id = if_else(is.na(doc_id) | !nzchar(doc_id),
                     paste0(party, "_", date_num),
                     doc_id)
  ) %>%
  relocate(doc_id, manifesto_id, id, party, partyname, countryname,
           date_num, date, election_id, text, token_count, pdf_url)

safe_write_table(full_data, paths$full)
message("  Saved: ", paths$full, " (", nrow(full_data), " rows)")

# ---- Sanity Checks ----

n_with_text <- sum(!is.na(full_data$text) & nzchar(full_data$text))
message("  Documents with non-empty text: ", n_with_text, " / ", nrow(full_data))

stopifnot("full_data should have rows" = nrow(full_data) > 0)
stopifnot("doc_id must be non-missing"  = all(!is.na(full_data$doc_id)))

# ---- 5. Optional: PDF Download ----

if (nzchar(Sys.getenv("MP_EMAIL")) && nzchar(Sys.getenv("MP_PASSWORD"))) {
  message("Attempting login + PDF download ...")
  # NOTE: mp_cookiejar_login() and download_with_cookiejar() must be defined
  # in 1_Functions.R. They are currently missing -- add them if PDF download
  # is needed.
  jar <- tryCatch(mp_cookiejar_login(), error = function(e) {
    warning("Login failed, skipping PDF download: ", conditionMessage(e))
    NULL
  })
  if (!is.null(jar)) {
    urls <- full_data$pdf_url[!is.na(full_data$pdf_url)]
    if (length(urls) > 0) {
      pdf_log <- download_with_cookiejar(urls, jar = jar, save_dir = paths$pdf_dir)
      readr::write_csv(pdf_log, paths$pdf_log)
      message("  PDF log: ", paths$pdf_log)
    } else {
      message("  No pdf_url found in full_data.")
    }
  }
} else {
  message("No MP_EMAIL/MP_PASSWORD set -- PDF download skipped.")
}

message("Done. full_data contains: ", paste(names(full_data)[1:12], collapse = ", "), ", ...")
