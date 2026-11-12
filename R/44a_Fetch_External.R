# ============================================================
# 44a_Fetch_External.R
#
# Downloads the public external party data used by R/44 (no API keys) into data/external/ and
# records URL, DOI / version, retrieval time, size and md5 in data/external/SOURCES.csv.
# Network stage: run once (`Rscript run_all.R fetch_external`); R/44 reads only the archived
# copies, so the chain stays deterministic. Existing files are not overwritten; a fetched file with
# the registered md5 leaves SOURCES.csv unchanged (the register ships in the public package, PLLM-0266).
#
#   ches_1999-2024_meansV2.csv   Chapel Hill Expert Survey trend file 1999-2024, party means
#                                (Jolly et al. 2022; Bakker et al. 2024), chesdata.eu
#   parlgov_view_party.csv       ParlGov development version, party table incl. left_right and
#                                the CMP party id (Döring, Huber & Manow), parlgov.org
#   poppa_2018_party_means.tab   POPPA, Populism and Political Parties Expert Survey 2018
#                                (Meijers & Zaslove), party means; Harvard Dataverse
#                                doi:10.7910/DVN/8NEL7B, version 2.0, file party_means.tab (id 3788780)
#   gps_2019_party_v2_1.tab      Global Party Survey 2019 (Norris), party-level file V2.1, Apr 2020
#                                (Stata version as ingested by Dataverse, tab-separated); Harvard
#                                Dataverse doi:10.7910/DVN/WMGTNS, version 3.0, id 3788853. DOI taken
#                                from globalpartysurvey.org (link "Harvard's Dataverse")
# PopuList 4.0 and Partyfacts are already archived in output/independence/cache/external/
# (fetched 2026-10-07 by R/50_Independence_4).
#
#   rfpopi_final_dataset.csv     RFPOPI random-forest populism indicators (Celico, Rode & Rodriguez
#                                Carreno 2024, European Journal of Political Economy,
#                                doi:10.1016/j.ejpoleco.2024.102529): populism_rhet / populism_ide per
#                                party and election (V-Party based), Partyfacts id pf_party_id. Static
#                                copy of the dataset offered through the Shiny app
#                                acelico.shinyapps.io/rfpopi_app: GitHub acelico/RFPOPI-Populism-indicators,
#                                file "data/RFPOPI final dataset.csv", commit 2870ebb94df9 (2024-02-16).
#                                Semicolon-separated, UTF-8 with BOM, decimal comma in the seat/vote
#                                columns. Licence (README): use for research and non-profit purposes
#                                with proper citation of the paper above.
# ============================================================

DIR <- "data/external"
dir.create(DIR, recursive = TRUE, showWarnings = FALSE)
SRC <- data.frame(
  file = c("ches_1999-2024_meansV2.csv", "parlgov_view_party.csv", "poppa_2018_party_means.tab", "gps_2019_party_v2_1.tab",
           "rfpopi_final_dataset.csv"),
  url  = c("https://github.com/chesdata/chesdata.github.io/releases/download/ches-trend/1999-2024_CHES_dataset_meansV2.csv",
           "https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_party.csv",
           "https://dataverse.harvard.edu/api/access/datafile/3788780",
           "https://dataverse.harvard.edu/api/access/datafile/3788853",
           "https://raw.githubusercontent.com/acelico/RFPOPI-Populism-indicators/2870ebb94d/data/RFPOPI%20final%20dataset.csv"),
  doi_version = c("", "", "doi:10.7910/DVN/8NEL7B v2.0 (party_means.tab)",
                  "doi:10.7910/DVN/WMGTNS v3.0 (Global Party Survey by Party Stata V2_1_Apr_2020.tab)",
                  paste("GitHub acelico/RFPOPI-Populism-indicators commit 2870ebb94df9 (2024-02-16), file data/RFPOPI final dataset.csv;",
                        "static copy of the RFPOPI dataset of the Shiny app acelico.shinyapps.io/rfpopi_app;",
                        "licence: research and non-profit use with citation of Celico, Rode and Rodriguez Carreno (2024),",
                        "European Journal of Political Economy, doi:10.1016/j.ejpoleco.2024.102529")),
  stringsAsFactors = FALSE)
log_f <- file.path(DIR, "SOURCES.csv")
old <- if (file.exists(log_f)) read.csv(log_f, stringsAsFactors = FALSE) else NULL
added_col <- !is.null(old) && !"doi_version" %in% names(old)
if (added_col) old$doi_version <- SRC$doi_version[match(old$file, SRC$file)]
rows <- list()
for (i in seq_len(nrow(SRC))) {
  f <- file.path(DIR, SRC$file[i])
  if (file.exists(f)) { message("exists, kept: ", f); next }
  tmp <- paste0(f, ".tmp")
  utils::download.file(SRC$url[i], tmp, mode = "wb", quiet = TRUE)
  stopifnot(file.rename(tmp, f))
  # replication package (PLLM-0266): the register ships, the files do not. A fetched file whose md5 equals
  # the registered one leaves the register byte-identical (it is a manifest input); a different md5
  # (changed upstream) is appended as a new row and reported.
  reg <- if (!is.null(old)) old$md5[old$file == SRC$file[i]] else character(0)
  if (length(reg) && unname(tools::md5sum(f)) %in% reg) { message("fetched, md5 as registered: ", f); next }
  if (length(reg)) warning("md5 of ", f, " differs from the register (changed upstream); new row appended", call. = FALSE)
  rows[[length(rows) + 1]] <- data.frame(file = SRC$file[i], url = SRC$url[i],
                                         retrieved_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                         bytes = file.size(f), md5 = unname(tools::md5sum(f)),
                                         doi_version = SRC$doi_version[i])
}
if (length(rows) || added_col) write.csv(rbind(old, do.call(rbind, rows)), log_f, row.names = FALSE)
print(read.csv(log_f))
