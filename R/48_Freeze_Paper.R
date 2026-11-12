# ============================================================
# 48_Freeze_Paper.R
#
# Freezes the paper source that R/44 and R/49 compare against (the printed Nov-2025 numbers and
# the line numbers of the claims) into a fixed chain input:
#   data/CMP/v2026a/paper_main_v2_<md5 of main_v2.tex>.csv   columns line, label, text
# `label` is the nearest preceding \label{...} (or section title). Run once per paper version
# (`Rscript run_all.R freeze_paper`); R/0_Config.R names the frozen file (PIPE_PAPER_LINES).
# Later edits to paper/ therefore never change chain outputs or break R/71_Verify.R.
# Refuses to overwrite.
# ============================================================
src <- "paper/R0/main_v2.tex"
md5 <- unname(tools::md5sum(src))
out <- sprintf("data/CMP/v2026a/paper_main_v2_%s.csv", md5)
if (file.exists(out)) { message("exists, kept: ", out); quit(save = "no") }
txt <- readLines(src, warn = FALSE, encoding = "UTF-8")
lab <- character(length(txt)); cur <- ""
for (i in seq_along(txt)) {
  m <- regmatches(txt[i], regexpr("\\\\label\\{[^}]+\\}|\\\\(sub)*section\\*?\\{[^}]+\\}", txt[i]))
  if (length(m)) cur <- m
  lab[i] <- cur
}
x <- data.frame(line = seq_along(txt), label = lab, text = txt, stringsAsFactors = FALSE)
tmp <- paste0(out, ".tmp"); utils::write.csv(x, tmp, row.names = FALSE, fileEncoding = "UTF-8"); stopifnot(file.rename(tmp, out))
message("frozen: ", out, " (", nrow(x), " lines)")
