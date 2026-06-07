# Generate spec_curve.pdf for the paper appendix from specs_grid.parquet
# Plus exact per-fork numbers for Table C.3.
suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(ggplot2); library(scales)
})

s <- read_parquet("website/data/specs_grid.parquet")
out_dir <- "paper/R0/figures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# === Per-fork breakdown table (Table C.3 input) ===
quantile_tbl <- function(df, fork_col) {
  df |>
    group_by(.fork = !!sym(fork_col)) |>
    summarise(
      n            = n(),
      median       = round(median(coef, na.rm = TRUE), 3),
      p5           = round(quantile(coef, 0.05, na.rm = TRUE), 3),
      p95          = round(quantile(coef, 0.95, na.rm = TRUE), 3),
      pct_sig_neg  = round(100 * mean(p < 0.05 & coef < 0, na.rm = TRUE), 1),
      pct_pos      = round(100 * mean(coef > 0, na.rm = TRUE), 1),
      .groups = "drop") |>
    mutate(fork_col = fork_col, .before = 1)
}
forks <- c("model_set","outcome","within_agg","between_agg","sample_key","fe_key","se_key")
per_fork <- bind_rows(lapply(forks, function(f) quantile_tbl(s, f)))
write_csv(per_fork, "paper/R0/figures/spec_curve_perfork.csv")
cat("Exact per-fork numbers (paste-ready for Table C.3):\n\n")
print(per_fork, n = 50)

# === Specification curve plot ===
sorted <- s |> arrange(coef) |> mutate(rank = row_number())
n_sig  <- sum(sorted$p < 0.05 & sorted$coef < 0)
n_flip <- sum(sorted$coef > 0)

p <- ggplot(sorted, aes(x = rank, y = coef)) +
  geom_ribbon(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
              fill = "steelblue", alpha = 0.18) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_line(colour = "steelblue", linewidth = 0.55) +
  geom_point(data = sorted |> filter(coef > 0),
             aes(x = rank, y = coef), colour = "firebrick", size = 0.6) +
  scale_y_continuous(name = "Populism coefficient",
                     limits = c(-1.5, 0.4),
                     breaks = seq(-1.5, 0.4, 0.25)) +
  scale_x_continuous(name = "Specification (sorted by coefficient)",
                     labels = comma) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave("paper/R0/figures/spec_curve.pdf",
       plot = p, width = 7.5, height = 4.2, units = "in", device = cairo_pdf)
cat("\nWrote paper/R0/figures/spec_curve.pdf  (",
    n_sig, " of ", nrow(sorted), " specs sig negative; ",
    n_flip, " sign-flipped)\n", sep = "")

# Also tiny histogram for reference
p2 <- ggplot(sorted, aes(x = coef)) +
  geom_histogram(binwidth = 0.025, fill = "steelblue", alpha = 0.7, colour = "white") +
  geom_vline(xintercept = median(sorted$coef), colour = "darkblue", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(name = "Populism coefficient", limits = c(-1.5, 0.5)) +
  scale_y_continuous(name = "Number of specifications", labels = comma) +
  theme_minimal(base_size = 11)
ggsave("paper/R0/figures/spec_curve_hist.pdf",
       plot = p2, width = 7.5, height = 3.5, units = "in", device = cairo_pdf)
cat("Wrote paper/R0/figures/spec_curve_hist.pdf\n")
