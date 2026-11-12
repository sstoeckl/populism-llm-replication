# ============================================================
# 71_Verify.R — exact-replication check of the v2026a chain
#
#   Rscript R/71_Verify.R            rerun the default chain (no API calls) into a scratch
#                                    directory and diff every file against MANIFEST.md5
#   Rscript R/71_Verify.R --keep     keep the scratch directory
#   Rscript R/71_Verify.R --public   run the public stages from the redacted inputs (R/72,
#                                    PIPE_PUBLIC=1) and diff their outputs against the same
#                                    manifest; outputs of recheck / ideology / ind2 / a13 / compare, the quote-class
#                                    table of oa and the full numbers.tex / 46_numbers.csv are not produced publicly
#                                    and are listed separately (numbers_public.* are produced and compared)
#
# Two certification levels (chain-review #3):
#   NUMERIC/TEXT  csv, tex, md, json, txt: every number is written with 10 significant digits by
#                 write_csv_det() / formatC, so these files must be byte-identical in ANY
#                 environment with the pinned package versions;
#   BINARY        pdf, parquet, rds: byte identity is guaranteed only in the SAME environment
#                 (R and package versions as in output/v2026a/env/packages.csv). If the
#                 environment differs, parquet files are compared by content instead.
# The environment check runs first and is reported separately from "numbers differ".
#
# Steps: (0) R and package versions vs output/v2026a/env/packages.csv;
#        (1) every INPUT must have its manifest md5 (else stop: the inputs changed);
#        (2) run_all.R default stages (except `manifest`) with PIPE_OUT / PIPE_DERIVED
#            pointed at the scratch directory;
#        (3) md5 of every DERIVED/ and OUT/ file vs the manifest; files missing on either
#            side are reported. Exit status 1 if anything differs.
# Report: output/v2026a/logs/71_verify_<timestamp>.txt
# ============================================================

MAN_FILE <- "output/v2026a/MANIFEST.md5"
PIPE_DERIVED_REF <- "data/CMP/v2026a"   # reference copies of DERIVED files (content comparison only)
keep <- "--keep" %in% commandArgs(trailingOnly = TRUE)
public <- "--public" %in% commandArgs(trailingOnly = TRUE)
NOT_PUBLIC <- "^DERIVED/argentina_|^OUT/argentina/|^OUT/(validation/a13/|validation/independence/(tables/t2|cache/t2_)|COMPARISON_v2026a\\.md$|csv/49_|csv/44_|csv/36_|csv/40_baseline_old_vs_new\\.csv$|IDEOLOGY_DIAGNOSIS\\.md$|tables/tab_ideo_external\\.tex$|tables/oa_ideo_levels_external\\.tex$|tables/tab_external_validity\\.tex$|figures/ideo_pop_joint\\.pdf$|csv/45_right_(quote_classes|protect_quotes|score_by_class)\\.csv$|tables/oa_right_quotes\\.tex$|csv/46_numbers\\.csv$|numbers\\.tex$|tables/tab_external_matches\\.tex$|tables/tab_external_validity_by_model\\.tex$|tables/oa_interaction_robustness\\.tex$|csv/47_|tables/oa_quote_lengths\\.tex$|tables/oa_validation_by_model\\.tex$|tables/oa9_|tables/oa10_)"
stopifnot(file.exists(MAN_FILE), file.exists("run_all.R"))

man <- readLines(MAN_FILE)
man <- man[!startsWith(man, "#") & nzchar(man)]
man <- data.frame(md5 = substr(man, 1, 32), path = substring(man, 35), stringsAsFactors = FALSE)
man$kind <- ifelse(startsWith(man$path, "OUT/"), "out", ifelse(startsWith(man$path, "DERIVED/"), "derived", "input"))

report <- character()
say <- function(...) { l <- sprintf(...); message(l); report <<- c(report, l) }

# ---- 0. Environment ----
env_ref <- "output/v2026a/env/packages.csv"
env_diff <- character()
if (file.exists(env_ref)) {
  pk <- utils::read.csv(env_ref, stringsAsFactors = FALSE)
  cur <- vapply(pk$package, function(p) if (p == "R") paste(R.version$major, R.version$minor, sep = ".") else
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else "missing", "")
  env_diff <- sprintf("%s %s -> %s", pk$package, pk$version, cur)[cur != pk$version]
}
ENV_SAME <- file.exists(env_ref) && !length(env_diff)
say("environment: %s", if (!file.exists(env_ref)) "reference env/packages.csv missing (treated as different)" else
  if (ENV_SAME) "same as the reference (R and all package versions)" else
    paste("DIFFERS from the reference:", paste(env_diff, collapse = "; ")))

# ---- 1. Inputs unchanged ----
inp <- man[man$kind == "input", ]
if (public) {          # canonical inputs are not part of the public package
  pub_files <- list.files("data/CMP/v2026a_public", recursive = TRUE, full.names = TRUE)
  say("public mode: %d redacted input files in data/CMP/v2026a_public (md5 listed in the report)", length(pub_files))
  report <- c(report, sprintf("  %s  %s", unname(tools::md5sum(pub_files)), pub_files))
  # restricted inputs (manifesto text, long quotes, the paper source) are not in the public package
  inp <- inp[!startsWith(inp$path, "data/CMP/v2026a/") &
               !grepl("checkpoints|corpus_texts|validated_spans|main_v2|20251111-poplib_results|full_data\\.parquet", inp$path) &
               # public mode reads metadata and MPDS from data/CMP/v2026a_public and skips the June comparison
               # full_data_2026a_meta is replaced by the rebuilt meta_2026a.parquet (R/73); mpds_2026a_full is fetched by R/73 and checked
               !grepl("full_data_2026a_meta|website/data/manifesto_scores", inp$path) &
               # the Argentina excursion (CNE pdfs, OCR text, out-of-sample chunk scores with quotes) is not in the public package
               !grepl("^data/external/argentina/|^data/CMP/argentina_oos/", inp$path), ]
}
now <- ifelse(file.exists(inp$path), unname(tools::md5sum(inp$path)), NA_character_)
bad_in <- inp$path[is.na(now) | now != inp$md5]
say("inputs: %d checked, %d changed or missing", nrow(inp), length(bad_in))
if (length(bad_in)) { say("  changed: %s", bad_in); stop("inputs differ from the manifest; outputs cannot be compared") }

# ---- 2. Rerun the chain into scratch ----
scratch <- tempfile("verify_v2026a_")
out <- file.path(scratch, "out"); der <- file.path(scratch, "derived")
dir.create(out, recursive = TRUE); dir.create(der, recursive = TRUE)
# stage list read from run_all.R (DEFAULT / PUBLIC_STAGES), so that the verify always reruns the chain that run_all.R runs
# (PLLM-0268: a hard-coded copy missed the stages added 2026-11-11); `manifest` is excluded, the manifest is the reference
ra <- readLines("run_all.R", warn = FALSE)
stage_vec <- function(name) {
  i <- grep(sprintf("^%s <- c\\(", name), ra); stopifnot(length(i) == 1)
  j <- i; while (!grepl("\\)\\s*(#.*)?$", ra[j])) j <- j + 1
  eval(parse(text = sub(sprintf("^%s <- ", name), "", paste(ra[i:j], collapse = " "))))
}
stages <- setdiff(stage_vec(if (public) "PUBLIC_STAGES" else "DEFAULT"), "manifest")
say("rerunning stages %s into %s", paste(stages, collapse = ", "), scratch)
t0 <- Sys.time()
rc <- system2("Rscript", c("run_all.R", stages), env = c(paste0("PIPE_OUT=", out), paste0("PIPE_DERIVED=", der),
                                                       if (public) "PIPE_PUBLIC=1"))
say("chain exit status %d after %.1f min", rc, as.numeric(difftime(Sys.time(), t0, units = "mins")))
if (rc != 0) stop("chain failed; logs in ", file.path(out, "logs"))

# ---- 3. Compare ----
loc <- function(p) ifelse(startsWith(p, "OUT/"), file.path(out, substring(p, 5)), file.path(der, substring(p, 9)))
res <- man[man$kind != "input", ]
res$local <- loc(res$path)
res$md5_new <- ifelse(file.exists(res$local), unname(tools::md5sum(res$local)), NA_character_)
res$status <- ifelse(is.na(res$md5_new), "MISSING", ifelse(res$md5_new == res$md5, "identical", "DIFFERENT"))
res$level <- ifelse(grepl("\\.(csv|tex|md|json|txt)$", res$path), "numeric/text", "binary")
# binary files that differ: same environment = failure; different environment = compare content
if (!ENV_SAME) for (i in which(res$status == "DIFFERENT" & res$level == "binary" & grepl("\\.parquet$", res$path))) {
  ref_f <- if (startsWith(res$path[i], "OUT/")) file.path("output/v2026a", substring(res$path[i], 5)) else
    file.path(PIPE_DERIVED_REF, substring(res$path[i], 9))
  if (!file.exists(ref_f)) next
  same <- isTRUE(all.equal(as.data.frame(arrow::read_parquet(ref_f)), as.data.frame(arrow::read_parquet(res$local[i])),
                           tolerance = 1e-10, check.attributes = FALSE))
  res$status[i] <- if (same) "content-identical" else "DIFFERENT"
}
if (public) res$status[res$status == "MISSING" & grepl(NOT_PUBLIC, res$path)] <- "not_public"
new_out <- list.files(out, recursive = TRUE)
new_out <- new_out[!grepl("(^|/)logs/|(^|/)env/|\\.tmp$", new_out)]
extra <- setdiff(paste0("OUT/", new_out), res$path)
new_der <- list.files(der); extra <- c(extra, setdiff(paste0("DERIVED/", new_der), res$path))

say("derived + outputs: %d in manifest | identical %d | different %d | missing %d | not produced in public mode %d | not in manifest %d",
    nrow(res), sum(res$status == "identical"), sum(res$status == "DIFFERENT"), sum(res$status == "MISSING"),
    sum(res$status == "not_public"), length(extra))
for (p in res$path[!res$status %in% c("identical", "not_public")]) say("  %-9s %s", res$status[res$path == p], p)
if (public) say("  not produced in public mode: %d files (recheck, ideology, ind2, a13, argentina, numbers, compare, oa quote classes, June comparison)", sum(res$status == "not_public"))
for (p in extra) say("  EXTRA     %s", p)
for (lv in c("numeric/text", "binary")) {
  r <- res[res$level == lv & res$status != "not_public", ]
  say("%-12s %d files | identical %d | content-identical %d | different %d | missing %d", lv, nrow(r),
      sum(r$status == "identical"), sum(r$status == "content-identical"), sum(r$status == "DIFFERENT"), sum(r$status == "MISSING"))
}
ok_numeric <- all(res$status[res$level == "numeric/text"] %in% c("identical", if (public) "not_public"))
ok_binary  <- all(res$status[res$level == "binary"] %in% c("identical", if (!ENV_SAME) "content-identical", if (public) "not_public"))
ok <- ok_numeric && ok_binary && !length(extra)
say("NUMERIC/TEXT level: %s; BINARY level: %s", if (ok_numeric) "identical" else "NUMBERS DIFFER",
    if (ok_binary) (if (ENV_SAME) "byte-identical (same environment)" else "content-identical (environment differs)") else "DIFFERENT")
say("RESULT: %s", if (ok && ENV_SAME && all(res$status %in% c("identical", "not_public"))) "BYTE-IDENTICAL" else
  if (ok) "NUMERICALLY IDENTICAL (binary files compared by content; environment differs)" else "DIFFERENCES FOUND")

dir.create("output/v2026a/logs", showWarnings = FALSE, recursive = TRUE)
writeLines(report, file.path("output/v2026a/logs", sprintf("71_verify%s_%s.txt", if (public) "_public" else "",
                                                          format(Sys.time(), "%Y%m%d_%H%M%S"))))
if (!keep) unlink(scratch, recursive = TRUE)
if (!ok) quit(status = 1)
