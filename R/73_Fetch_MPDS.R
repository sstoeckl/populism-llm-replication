# ============================================================
# 73_Fetch_MPDS.R  (stage `fetch_mpds`; network + Manifesto Project API key; run once)
#
# The public replication package does not redistribute Manifesto Project data (terms of use).
# This stage fetches the MPDS2026a main dataset with the user's own key and rebuilds the public
# metadata file from it, so that the public-mode chain runs from the package plus this step:
#   data/CMP/mpds_2026a_full.parquet            MPDS2026a main dataset incl. per* codes; its md5 is listed in
#                                               MANIFEST.md5 and was reproduced byte for byte on 2026-11-11
#   data/CMP/v2026a_public/meta_2026a.parquet   = the metadata file of the full mode (R/72 selection),
#                                               rebuilt from meta_2026a_keys.parquet (our ids and text
#                                               flags, shipped) joined to MPDS on (party, date)
# Existing files are not overwritten. Key: MANIFESTO_API_KEY or MANIFESTO_KEY in .Renviron
# (free academic registration at manifesto-project.wzb.eu).
# ============================================================
suppressPackageStartupMessages({ library(manifestoR); library(arrow); library(dplyr) })
Sys.setenv(PIPE_PUBLIC = "1")                       # paths of the public package
source("R/0_Config.R"); source("R/1_Functions.R")

MPDS <- "data/CMP/mpds_2026a_full.parquet"
if (file.exists(MPDS)) {
  message("exists, kept: ", MPDS)
} else {
  key <- Sys.getenv("MANIFESTO_API_KEY"); if (!nzchar(key)) key <- Sys.getenv("MANIFESTO_KEY")
  if (!nzchar(key)) stop("MANIFESTO_API_KEY / MANIFESTO_KEY missing (.Renviron)")
  manifestoR::mp_setapikey(key = key)
  m <- manifestoR::mp_maindataset(version = "MPDS2026a", cache = FALSE)
  dir.create(dirname(MPDS), recursive = TRUE, showWarnings = FALSE)
  safe_write_table(m, MPDS)
  message("fetched MPDS2026a: ", nrow(m), " x ", ncol(m), " -> ", MPDS)
}
man <- readLines(file.path("output/v2026a", "MANIFEST.md5"), warn = FALSE)
exp <- sub("  .*$", "", grep(paste0("  ", MPDS, "$"), man, value = TRUE))
got <- unname(tools::md5sum(MPDS))
if (length(exp) == 1) message(sprintf("md5 %s (manifest %s): %s", got, exp, if (got == exp) "identical" else "DIFFERENT; R/71 --public will stop"))

KEYS <- file.path(PIPE_IN, "meta_2026a_keys.parquet"); META <- file.path(PIPE_IN, "meta_2026a.parquet")
if (file.exists(META)) { message("exists, kept: ", META) } else {
  mp <- read_parquet(MPDS)
  meta <- read_parquet(KEYS) |>
    mutate(date_num = as.integer(format(date, "%Y%m"))) |>
    left_join(mp |> select(party, date, partyname, partyabbrev, countryname, edate, parfam, rile, markeco, planeco),
              by = c("party", "date_num" = "date")) |>
    select(doc_id, manifesto_id, party, partyname, partyabbrev, countryname, date, edate, parfam, rile,
           token_count, has_text, update_status, text_change_type, markeco, planeco)
  write_atomic(meta, META)
  message("rebuilt ", META, ": ", nrow(meta), " rows, md5 ", unname(tools::md5sum(META)))
}
