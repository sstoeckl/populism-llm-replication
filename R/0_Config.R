# ============================================================
# 0_Config.R
#
# Paths and version stamps shared by the v2026a chain (R/34 checks onward).
#
#   PIPE_IN       canonical, read-only inputs: merged raw chunk files, R/12 validated
#                 spans, coverage, gap chunks, archive/, the R/34 judge cache
#   PIPE_DERIVED  derived data written by the chain (span checks, baseline)
#   PIPE_OUT      tables, figures, csv, logs
#   PIPE_PUBLIC   if set: PIPE_IN = data/CMP/v2026a_public (redacted, R/72), derived and
#                 outputs default to data/CMP/v2026a_public/derived and output/v2026a_public
#
# The verify script (R/71) points PIPE_DERIVED and PIPE_OUT at a scratch directory, reruns
# the chain and diffs the results against output/v2026a/MANIFEST.md5.
# ============================================================

# PIPE_PUBLIC=1: run from the redacted public inputs (R/72) instead of the canonical ones
PIPE_PUBLIC  <- nzchar(Sys.getenv("PIPE_PUBLIC"))
PIPE_IN      <- if (PIPE_PUBLIC) "data/CMP/v2026a_public" else "data/CMP/v2026a"
PIPE_DERIVED <- Sys.getenv("PIPE_DERIVED", if (PIPE_PUBLIC) "data/CMP/v2026a_public/derived" else "data/CMP/v2026a")
PIPE_OUT     <- Sys.getenv("PIPE_OUT", if (PIPE_PUBLIC) "output/v2026a_public" else "output/v2026a")
# MPDS2026a metadata (no text) and the MPDS2026a main dataset incl. per* codes. In public mode the
# metadata file is rebuilt by R/73 from the shipped keys file and the MPDS dataset the user fetches.
PIPE_META    <- if (PIPE_PUBLIC) file.path(PIPE_IN, "meta_2026a.parquet") else "data/CMP/full_data_2026a_meta.parquet"
PIPE_MPDS    <- "data/CMP/mpds_2026a_full.parquet"   # in public mode fetched by the user with R/73 (stage fetch_mpds), not redistributed
# The June 2026 baseline, used only for the old-vs-new comparison of R/40 (optional input:
# if absent, e.g. in public mode, that comparison is skipped with a message)
PIPE_JUNE_BASELINE <- "website/data/manifesto_scores.parquet"
# Frozen copy of the paper source lines used by R/44 and R/49 (printed numbers, claim line
# numbers); written once by R/48_Freeze_Paper.R, so edits to the paper never break verify
PIPE_PAPER_LINES <- "data/CMP/v2026a/paper_main_v2_1ca04ab71ce6ca111f1c0af62f51228a.csv"

# Revision-aware input files. rev2 (2026-10-24): Claude rows of the 363 parse-loss chunks
# replaced by the patched-flattener rescore (R/32 parseloss + R/35); the original rows are in
# archive/dropped_rows_claude_rev2.parquet. GPT and Gemini are unchanged.
V26_REV <- c(claude = "rev2", gpt41mini = "", gemini = "")     # current revision per model
v26_file <- function(kind, stem) {
  f <- file.path(PIPE_IN, sprintf("%s_%s%s.parquet", kind, stem,
                                  if (nzchar(V26_REV[[stem]])) paste0("_", V26_REV[[stem]]) else ""))
  if (!file.exists(f)) stop("input missing: ", f)
  f
}

PIPE_VERSION <- list(
  baseline       = "v2026a",
  corpus         = "MPDS2026a / corpus 2026-1 (20260904101355)",
  # requested IDs; what is known about the version actually served (see CODEBOOK.md §2):
  #   claude-sonnet-4-6 is an undated alias; resolved id logged for 2026-10 calls, not for June
  #   gpt-4.1-mini-2025-04-14 is a dated snapshot; resolved id logged for 2026-10 calls, not for June
  #   Gemini: June rows requested the floating alias gemini-flash-latest (version not logged,
  #   inferred gemini-3.5-flash from an anchor test); 2026-10 rows gemini-3.5-flash, logged
  models         = "claude-sonnet-4-6 (undated alias); gpt-4.1-mini-2025-04-14; Gemini: gemini-flash-latest in June (inferred gemini-3.5-flash), gemini-3.5-flash in 2026-10 (logged)",
  judge          = "gpt-4o-mini-2024-07-18"
)

for (d in file.path(PIPE_OUT, c("tables", "figures", "csv", "logs")))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
dir.create(PIPE_DERIVED, recursive = TRUE, showWarnings = FALSE)

# Atomic write: temp file in the same directory, then rename
write_atomic <- function(x, path, writer = arrow::write_parquet, ...) {
  tmp <- paste0(path, ".tmp")
  writer(x, tmp, ...)
  stopifnot(file.rename(tmp, path))
  invisible(path)
}
# Deterministic CSV: fixed column order as given, numbers with 10 significant digits
write_csv_det <- function(x, path) {
  x <- as.data.frame(x)
  for (j in which(vapply(x, function(v) inherits(v, c("Date", "POSIXt", "factor")), logical(1))))
    x[[j]] <- as.character(x[[j]])
  quote_cols <- which(vapply(x, is.character, logical(1)))
  num <- vapply(x, is.double, logical(1))
  x[num] <- lapply(x[num], function(v) ifelse(is.finite(v), trimws(formatC(v, digits = 10, format = "g")), NA))
  write_atomic(x, path, writer = function(d, p)
    utils::write.csv(d, p, row.names = FALSE, na = "", quote = if (length(quote_cols)) quote_cols else FALSE))
}

# Deterministic PDF: cairo writes the wall-clock time into /CreationDate (and /ModDate);
# overwrite the 14 timestamp digits in place with a fixed date (same length, so the xref
# offsets stay valid). Everything else in a cairo PDF is a function of the plot.
pdf_fix_dates <- function(path) {
  b <- readBin(path, "raw", file.info(path)$size)
  for (key in c("/CreationDate (D:", "/ModDate (D:")) {
    pos <- grepRaw(key, b, fixed = TRUE, all = TRUE)
    for (p in pos) b[(p + nchar(key)):(p + nchar(key) + 13)] <- charToRaw("20000101000000")
  }
  writeBin(b, path)
}
save_pdf <- function(plot, path, width, height, ...) {
  tmp <- paste0(path, ".tmp.pdf")
  ggplot2::ggsave(tmp, plot, width = width, height = height, device = grDevices::cairo_pdf, ...)
  pdf_fix_dates(tmp)
  stopifnot(file.rename(tmp, path))
  invisible(path)
}
