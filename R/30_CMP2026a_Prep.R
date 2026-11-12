# ============================================================
# 30_CMP2026a_Prep.R
#
# Incremental CMP update: identify manifestos that are NEW or CHANGED in
# the latest Manifesto Project release relative to the corpus the v4
# cross-LLM run was scored on (data/CMP/full_data.parquet, built by R/2
# from MPDS2025a + corpus 2025-1 on 2025-11-11).
#
# Inclusion rules are IDENTICAL to R/2 + the scoring drivers (R/15b/16/18):
#   - all rows of the main dataset (MPDS) are kept as election metadata
#   - texts = every document returned by mp_corpus(mpds) for the version
#   - join on (party, date_num); doc_id = manifesto_id
#   - scoring pool = !is.na(manifesto_id) & !is.na(text), distinct(doc_id)
#
# R/2 calls clean_manifesto_text(), estimate_token_count(),
# cmp_date_to_Date() and make_election_id() which were never committed.
# They were reconstructed here (now in R/2_Helpers.R) and VERIFIED against full_data.parquet
# (section 3: re-cleaned 2025-1 text must be byte-identical to the stored text).
#
# Does NOT overwrite any existing file. Outputs (VER = new MPDS version):
#   data/CMP/mpds_<VER>_full.parquet           full MPDS incl. per* codes
#   data/CMP/mpds_<VER>.parquet                election metadata (no per*), as R/2
#   data/CMP/full_data_<VER>_meta.parquet      full new metadata (all MPDS rows,
#                                              no text) + update_status flag
#   data/CMP/full_data_update_<VER>.parquet    new + changed manifestos, with text
#   data/CMP/update_<VER>/text_changes.parquet per-doc diff stats for changed text
#   data/CMP/update_<VER>/meta_changes.parquet long table of changed metadata cells
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(manifestoR)
  library(lubridate)
  library(arrow)
})

source("R/1_Functions.R")

# ---- Config ----

OLD_FULL     <- "data/CMP/full_data.parquet"
OLD_CORPUS   <- "20250904101936"     # tag 2025-1 (used by R/2 on 2025-11-11)

key <- Sys.getenv("MANIFESTO_API_KEY")
if (!nzchar(key)) key <- Sys.getenv("MANIFESTO_KEY")
if (!nzchar(key)) stop("MANIFESTO_API_KEY / MANIFESTO_KEY missing")
manifestoR::mp_setapikey(key = key)

core_versions   <- manifestoR::mp_coreversions()
corpus_versions <- manifestoR::mp_corpusversions()
NEW_MPDS   <- tail(core_versions$datasets.id, 1)          # e.g. "MPDS2026a"
NEW_CORPUS <- tail(corpus_versions$name, 1)               # e.g. "20260904101355"
VER        <- sub("^MPDS", "", NEW_MPDS)                  # e.g. "2026a"
message("Latest MPDS: ", NEW_MPDS, " | latest corpus: ", NEW_CORPUS,
        " (", tail(corpus_versions$tag, 1), ")")

base_dir <- "data/CMP"
upd_dir  <- file.path(base_dir, paste0("update_", VER))
ensure_dir(upd_dir)

paths <- list(
  mpds_full = file.path(base_dir, paste0("mpds_", VER, "_full.parquet")),
  mpds      = file.path(base_dir, paste0("mpds_", VER, ".parquet")),
  meta      = file.path(base_dir, paste0("full_data_", VER, "_meta.parquet")),
  update    = file.path(base_dir, paste0("full_data_update_", VER, ".parquet")),
  txt_chg   = file.path(upd_dir, "text_changes.parquet"),
  meta_chg  = file.path(upd_dir, "meta_changes.parquet")
)
paths$txt_cache <- file.path(upd_dir, paste0("corpus_texts_", NEW_CORPUS, ".parquet"))
# mpds_* and the text cache are written by this script only; reuse if present
existing <- unlist(paths[c("meta", "update", "txt_chg", "meta_chg")])
existing <- existing[file.exists(existing)]
if (length(existing)) stop("Refusing to overwrite: ", paste(existing, collapse = ", "))

# ---- Reconstructed R/2 helpers (verified in section 3) ----
# cmp_date_to_Date(), clean_manifesto_text(), estimate_token_count() now live in
# R/2_Helpers.R (extracted 2026-10-24; bodies unchanged).
source("R/2_Helpers.R")

corpus_texts <- function(corpus) {
  map_dfr(seq_along(corpus), function(i) {
    m <- meta(corpus[[i]])
    raw <- paste(manifestoR::content(corpus[[i]]), collapse = " ")
    txt <- clean_manifesto_text(raw)
    tibble(
      manifesto_id = m$manifesto_id %||% NA_character_,
      id           = as.character(m$id %||% NA),
      party        = as.numeric(m$party),
      date_num     = as.numeric(m$date),
      text         = txt,
      token_count  = estimate_token_count(txt),
      pdf_url      = m$url_original %||% NA_character_,
      doc_id       = m$manifesto_id %||% NA_character_,
      text_raw     = raw
    )
  })
}

# ---- 1. New main dataset (with per-codes) ----

message("Loading ", NEW_MPDS, " ...")
if (file.exists(paths$mpds_full)) {
  mpds_new <- read_parquet(paths$mpds_full)
} else {
  mpds_new <- manifestoR::mp_maindataset(version = NEW_MPDS, cache = TRUE)
  safe_write_table(mpds_new, paths$mpds_full)
}
message("  Saved: ", paths$mpds_full, " (", nrow(mpds_new), " x ", ncol(mpds_new), ")")

el_new <- mpds_new %>%
  select(-contains("per")) %>%            # identical to R/2 (also drops pervote etc.)
  rename(date_num = date) %>%
  mutate(date = cmp_date_to_Date(date_num))
if (!file.exists(paths$mpds)) safe_write_table(el_new, paths$mpds)

# ---- 2. New corpus texts ----

message("Loading corpus ", NEW_CORPUS, " ...")
manifestoR::mp_use_corpus_version(NEW_CORPUS)
if (file.exists(paths$txt_cache)) {
  txt_new <- read_parquet(paths$txt_cache)
} else {
  corpus_new <- manifestoR::mp_corpus(mpds_new, cache = TRUE)
  txt_new <- corpus_texts(corpus_new)
  safe_write_table(txt_new, paths$txt_cache)
}
# (re-)apply the cleaning to the cached raw text
txt_new <- txt_new %>%
  mutate(text = map_chr(text_raw, clean_manifesto_text),
         token_count = estimate_token_count(text)) %>%
  select(-text_raw)
message("  documents with text: ", nrow(txt_new))

full_new <- el_new %>%
  left_join(txt_new, by = c("party", "date_num")) %>%
  mutate(
    date        = cmp_date_to_Date(date_num),
    election_id = paste0(gsub(" ", "", countryname), "_", format(date, "%Y%m%d")),   # = R/2 make_election_id()
    doc_id      = if_else(is.na(doc_id) | !nzchar(doc_id), paste0(party, "_", date_num), doc_id)
  ) %>%
  relocate(doc_id, manifesto_id, id, party, partyname, countryname,
           date_num, date, election_id, text, token_count, pdf_url)

# ---- 3. Verify the reconstructed cleaning against the old run ----
# NOTE: the API no longer serves the Nov-2025 text for revised documents even when
# pinned to 2025-1, so the check is: share of the 3,327 old texts reproduced
# byte-for-byte by the 2026-1 text under the reconstructed cleaning. A wrong rule
# would fail on (nearly) every document; revised documents are the residual.

old <- read_parquet(OLD_FULL) %>% mutate(date = as.Date(date))
old_pool <- old %>% filter(!is.na(manifesto_id), !is.na(text)) %>% distinct(doc_id, .keep_all = TRUE)
chk <- old_pool %>% select(doc_id, text_old = text, tc_old = token_count) %>%
  inner_join(txt_new %>% distinct(doc_id, .keep_all = TRUE) %>% select(doc_id, text, token_count), by = "doc_id")
share_ident <- mean(chk$text_old == chk$text)
share_tc    <- mean(abs(chk$tc_old - chk$token_count) < 1e-9)
message(sprintf("  cleaning reproduction (stored 2025-1 vs 2026-1): text %.1f%% identical; token_count %.1f%% (n=%d)",
                100 * share_ident, 100 * share_tc, nrow(chk)))
stopifnot("reconstructed cleaning must reproduce the bulk of stored texts" = share_ident > 0.9)

# ---- 4. Classify new / changed / unchanged / removed ----

new_pool <- full_new %>% filter(!is.na(manifesto_id), !is.na(text)) %>% distinct(doc_id, .keep_all = TRUE)

# Text change metrics (relative length change + token-set Jaccard)
jaccard_words <- function(a, b) {
  wa <- unique(strsplit(a, " ", fixed = TRUE)[[1]]); wb <- unique(strsplit(b, " ", fixed = TRUE)[[1]])
  length(intersect(wa, wb)) / length(union(wa, wb))
}
txt_cmp <- old_pool %>% select(doc_id, text_old = text) %>%
  inner_join(new_pool %>% select(doc_id, text_new = text), by = "doc_id") %>%
  filter(text_old != text_new) %>%
  mutate(nchar_old = nchar(text_old), nchar_new = nchar(text_new),
         rel_len_change = (nchar_new - nchar_old) / nchar_old,
         word_jaccard   = map2_dbl(text_old, text_new, jaccard_words),
         # identical up to Unicode NFC, U+FFFD repair, [n] markers and whitespace?
         norm_only      = map2_lgl(text_old, text_new, function(a, b) {
           nz <- function(x) gsub("\\s+", "", gsub("\\[\\d+\\]|\ufffd", "", stringi::stri_trans_nfc(x)))
           nz(a) == nz(b) })) %>%
  select(-text_old, -text_new)

# Metadata changes on columns shared by both versions (excluding text / version stamps)
meta_cols <- setdiff(intersect(names(old_pool), names(new_pool)),
                     c("doc_id", "text", "token_count", "datasetversion", "corpusversion", "pdf_url"))
both <- inner_join(old_pool %>% select(doc_id, all_of(meta_cols)),
                   new_pool %>% select(doc_id, all_of(meta_cols)),
                   by = "doc_id", suffix = c(".old", ".new"))
meta_chg <- map_dfr(setdiff(meta_cols, "doc_id"), function(v) {
  a <- both[[paste0(v, ".old")]]; b <- both[[paste0(v, ".new")]]
  if (is.numeric(a) && is.numeric(b)) diff <- !((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) < 1e-8))
  else diff <- !((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & as.character(a) == as.character(b)))
  tibble(doc_id = both$doc_id[diff], variable = v,
         old = as.character(a[diff]), new = as.character(b[diff]))
})

# Party re-codes: a doc_id that disappears and a "new" doc_id in the same
# country / election date / party name are the same manifesto under a new CMP
# party code. They keep their old scores (old_doc_id records the mapping).
recoded <- old_pool %>% filter(!doc_id %in% new_pool$doc_id) %>%
  select(old_doc_id = doc_id, countryname, date_num, partyname, text_old = text) %>%
  inner_join(new_pool %>% filter(!doc_id %in% old_pool$doc_id) %>%
               select(doc_id, countryname, date_num, partyname, text_new = text),
             by = c("countryname", "date_num", "partyname")) %>%
  mutate(same_text = text_old == text_new,
         word_jaccard = map2_dbl(text_old, text_new, jaccard_words)) %>%
  select(doc_id, old_doc_id, same_text, word_jaccard)
stopifnot(!anyDuplicated(recoded$doc_id), !anyDuplicated(recoded$old_doc_id))

status_tbl <- tibble(doc_id = union(old_pool$doc_id, new_pool$doc_id)) %>%
  left_join(recoded %>% select(doc_id, old_doc_id, same_text), by = "doc_id") %>%
  mutate(update_status = case_when(
    !is.na(old_doc_id) & same_text  ~ "recoded",
    !is.na(old_doc_id)              ~ "recoded_changed_text",
    doc_id %in% recoded$old_doc_id  ~ "recoded_old_id",
    !doc_id %in% old_pool$doc_id    ~ "new",
    !doc_id %in% new_pool$doc_id    ~ "removed",
    doc_id %in% txt_cmp$doc_id      ~ "changed_text",
    doc_id %in% meta_chg$doc_id     ~ "changed_meta",
    TRUE                            ~ "unchanged")) %>%
  select(-same_text) %>%
  left_join(txt_cmp %>% select(doc_id, norm_only), by = "doc_id") %>%
  mutate(text_change_type = case_when(
    update_status %in% c("changed_text", "recoded_changed_text") & norm_only %in% TRUE ~ "normalisation_only",
    update_status %in% c("changed_text", "recoded_changed_text") ~ "substantive",
    TRUE ~ "none")) %>%
  select(-norm_only)

message("  status counts: ", paste(names(table(status_tbl$update_status)),
                                   table(status_tbl$update_status), sep = "=", collapse = " | "))

# ---- 5. Write outputs ----

full_meta <- full_new %>% select(-text) %>%
  left_join(status_tbl, by = "doc_id") %>%
  mutate(has_text = doc_id %in% new_pool$doc_id,
         mpds_version = NEW_MPDS, corpus_version = NEW_CORPUS)
safe_write_table(full_meta, paths$meta)

update_tbl <- new_pool %>%
  inner_join(status_tbl %>% filter(update_status %in% c("new", "changed_text", "changed_meta",
                                                       "recoded", "recoded_changed_text")),
             by = "doc_id") %>%
  mutate(mpds_version = NEW_MPDS, corpus_version = NEW_CORPUS)
safe_write_table(update_tbl, paths$update)
safe_write_table(txt_cmp, paths$txt_chg)
safe_write_table(meta_chg, paths$meta_chg)
safe_write_table(status_tbl, file.path(upd_dir, "update_status.parquet"))

message("  Saved: ", paths$update, " (", nrow(update_tbl), " rows: ",
        sum(update_tbl$update_status == "new"), " new, ",
        sum(update_tbl$update_status == "changed_text"), " changed text, ",
        sum(update_tbl$update_status == "changed_meta"), " changed metadata only, ",
        sum(grepl("^recoded", update_tbl$update_status)), " party re-codes)")

# ---- Sanity checks ----
stopifnot(all(!is.na(update_tbl$doc_id)), !anyDuplicated(update_tbl$doc_id))
stopifnot("old scoring pool should be 3,327" = nrow(old_pool) == 3327L)
print(summary(warnings()))
message("Done.")
