# ============================================================
# 34b_Span_Checks_Public.R
#
# Public-mode replacement of the `checks` stage (R/34): the span checks need the full quotes
# and contexts, which the public inputs do not carry, so the archived flags shipped in
# data/CMP/v2026a_public/span_checks_*.parquet (written by R/72 from the R/34 output) are
# copied byte for byte into PIPE_DERIVED.
# ============================================================
Sys.setenv(PIPE_PUBLIC = "1")
source("R/0_Config.R")
fs <- list.files(PIPE_IN, "^span_checks_.*\\.parquet$", full.names = TRUE)
stopifnot(length(fs) == 4L)
for (f in fs) stopifnot(file.copy(f, file.path(PIPE_DERIVED, basename(f)), overwrite = TRUE))
message("R/34b: copied ", length(fs), " span-check files to ", PIPE_DERIVED)
