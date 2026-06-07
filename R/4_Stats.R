# ============================================================
# 4_Stats.R — Statistical Analysis
#
# Pipeline step 3: Aggregation, descriptive statistics,
# panel regressions, country scatterplots, marginal effects.
#
# Inputs:
#   - data/CMP/full_data.parquet       (manifesto metadata)
#   - data/CMP/20251111-poplib_results.RData (GPT-scored results)
#
# Outputs:
#   - output/*.pdf                     (all figures)
#   - LaTeX tables printed to console  (copy into paper)
# ============================================================

# ---- Setup ----

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(fixest)
  library(modelsummary)
  library(marginaleffects)
  library(xtable)
  library(ggrepel)
})
source("R/1_Functions.R")

# ---- Paths & Data Ingestion ----

base_dir <- "data/CMP"
ensure_dir(base_dir)
ensure_dir("output")

dat_raw <- safe_read_table(file.path(base_dir, "full_data.parquet"))

# Validate required columns
req_cols <- c("doc_id", "party", "partyname", "countryname",
              "date_num", "date", "election_id", "text", "token_count")
miss <- setdiff(req_cols, names(dat_raw))
if (length(miss)) stop("full_data missing columns: ", paste(miss, collapse = ", "))

dat_raw$date <- as.Date(dat_raw$date)
dat <- dat_raw |> filter(!is.na(manifesto_id), !is.na(text))

# ---- Table 1: Elections Per Country ----

stat_1 <- dat |>
  group_by(countryname, date) |>
  summarise(n_parties_el = n(), .groups = "drop") |>
  group_by(countryname) |>
  summarise(
    n_elections = n(),
    first_el    = as.character(min(date)),
    last_el     = as.character(max(date)),
    min_p       = min(n_parties_el),
    max_p       = max(n_parties_el),
    mean_p      = mean(n_parties_el),
    median_p    = median(n_parties_el),
    .groups     = "drop"
  ) |>
  left_join(
    dat |> distinct(countryname, party) |> count(countryname, name = "n_parties"),
    by = "countryname"
  ) |>
  relocate(n_parties, .after = n_elections)

stat_1_xtab <- xtable(
  stat_1,
  caption = "Descriptive Statistics of Elections per Country",
  label   = "tab:stat1",
  align   = c("l", "l", "r", "r", "r", "r", "r", "r", "r", "r")
)
print(stat_1_xtab, type = "latex", include.rownames = FALSE,
      booktabs = TRUE, tabular.environment = "longtable")

# ---- Data Summary ----

n_countries <- n_distinct(dat$countryname)
n_elections <- dat |> distinct(countryname, date) |> nrow()
n_parties   <- dat |> distinct(countryname, party) |> nrow()
nn          <- nrow(dat)
n_years     <- n_distinct(year(dat$date))
min_year    <- year(min(dat$date, na.rm = TRUE))
max_year    <- year(max(dat$date, na.rm = TRUE))

message(
  "Data Summary:\n",
  "  Countries:    ", n_countries, "\n",
  "  Elections:    ", n_elections, "\n",
  "  Parties:      ", n_parties, "\n",
  "  Observations: ", nn, "\n",
  "  Years:        ", n_years, " (", min_year, " to ", max_year, ")"
)

# ---- Load GPT-Scored Results & Aggregate ----

load(file.path(base_dir, "20251111-poplib_results.RData"))

raw_out <- results |>
  map("raw") |>
  bind_rows() |>
  group_by(doc_id_root) |>
  summarise(
    across(
      c(populism_overall, pop_anti_elitism, pop_people_centrism, pop_manichean),
      ~ weighted.mean(.x, populism_confidence, na.rm = TRUE)
    ),
    across(
      c(liberalism_overall, lib_political, lib_social, lib_economic, lib_financial_market),
      ~ weighted.mean(.x, liberalism_confidence, na.rm = TRUE)
    ),
    across(
      c(pop_ideology_overall, pop_ideology_left, pop_ideology_right, pop_ideology_centrist),
      ~ weighted.mean(.x, pop_ideology_confidence, na.rm = TRUE)
    ),
    across(contains("confidence"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  # Rescale ideology to [-1, +1]: (right - left) / (right + left + centrist)
  mutate(
    pop_ideology_overall2 = (pop_ideology_right - pop_ideology_left) /
      (pop_ideology_right + pop_ideology_left + pop_ideology_centrist)
  ) |>
  left_join(
    dat |> select(doc_id, date, countryname, party, partyname),
    by = c("doc_id_root" = "doc_id")
  ) |>
  select(
    doc_id = doc_id_root, date, countryname, partyname,
    populism_overall, liberalism_overall, pop_ideology_overall,
    everything()
  ) |>
  arrange(countryname, date)

# ---- Prepare Country-Level Plot Data (2010+) ----

# Spanish "We can" / Podemos party name harmonization
spain_parties_regex <- "We can|Basque Nat|Socialist|People's Party|Voice|Catal|Citi"

raw_out_more <- raw_out |>
  left_join(
    dat_raw |> select(countryname, date, partyname, partyabbrev) |> distinct(),
    by = join_by(countryname, date, partyname)
  ) |>
  mutate(
    partyname   = if_else(countryname == "Spain" & str_detect(partyname, regex("we can", ignore_case = TRUE)),
                          "We can", partyname),
    partyabbrev = if_else(countryname == "Spain" & str_detect(partyname, fixed("We can")),
                          "We can", partyabbrev)
  ) |>
  group_by(countryname, partyname, partyabbrev, date) |>
  mutate(across(
    c(populism_overall, liberalism_overall, pop_ideology_overall2, lib_economic),
    ~ mean(.x, na.rm = TRUE)
  )) |>
  ungroup() |>
  filter(date >= "2010-01-01") |>
  mutate(date = factor(date)) |>
  rename(
    "Election Date"       = date,
    "Populist Ideology"   = pop_ideology_overall2,
    "Populism Overall"    = populism_overall,
    "Liberalism Overall"  = liberalism_overall,
    "Economic Liberalism" = lib_economic,
    "Party Name"          = partyname
  )

# ---- Descriptive Statistics Table ----

# Variables for summary and correlation tables
desc_vars <- c(
  "Populism (overall)"              = "populism_overall",
  "Liberalism (overall)"            = "liberalism_overall",
  "Liberalism (political)"          = "lib_political",
  "Liberalism (social)"             = "lib_social",
  "Liberalism (economic)"           = "lib_economic",
  "Liberalism (financial markets)"  = "lib_financial_market",
  "Populist ideology (rescaled)"    = "pop_ideology_overall2"
)

summary_stats <- raw_out |>
  select(all_of(setNames(desc_vars, names(desc_vars)))) |>
  summarise(across(everything(), list(
    mean   = ~ mean(.x, na.rm = TRUE),
    sd     = ~ sd(.x, na.rm = TRUE),
    min    = ~ min(.x, na.rm = TRUE),
    q25    = ~ quantile(.x, 0.25, na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    q75    = ~ quantile(.x, 0.75, na.rm = TRUE),
    max    = ~ max(.x, na.rm = TRUE)
  ))) |>
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "_",
               values_to = "value") |>
  pivot_wider(names_from = stat, values_from = value)

print(
  xtable(summary_stats,
         caption = "Summary Statistics of Populism, Liberalism, and Populist Ideology Scores",
         label   = "tab:sumstats",
         align   = c("l", "l", "r", "r", "r", "r", "r", "r", "r")),
  type = "latex", include.rownames = FALSE, booktabs = TRUE
)

# ---- Correlation Matrix (Lower Triangle) ----

cor_mat <- raw_out |>
  select(all_of(setNames(desc_vars, names(desc_vars)))) |>
  cor(use = "pairwise.complete.obs")
cor_mat[upper.tri(cor_mat, diag = TRUE)] <- NA

print(
  xtable(as.data.frame(cor_mat),
         caption = "Correlation Matrix of Populism, Liberalism, and Populist Ideology Scores",
         label   = "tab:cormatrix",
         align   = c("l", rep("r", ncol(cor_mat)))),
  type = "latex", include.rownames = TRUE, booktabs = TRUE
)

# ---- Time Trend Plots ----

year_agg <- raw_out |>
  mutate(year = year(date)) |>
  group_by(year) |>
  summarise(across(c(populism_overall, liberalism_overall, pop_ideology_overall2),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

year_agg_ideo <- raw_out |>
  mutate(year = year(date),
         Ideology = if_else(pop_ideology_overall2 <= 0, "Left", "Right")) |>
  group_by(year, Ideology) |>
  summarise(across(c(populism_overall, liberalism_overall, pop_ideology_overall2),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

# Helper: time trend plot
plot_trend <- function(data, yvar, title, ylab, color_var = NULL) {
  p <- ggplot(data |> filter(year >= 1960), aes(x = year, y = .data[[yvar]]))
  if (!is.null(color_var)) {
    p <- p +
      aes(color = .data[[color_var]]) +
      scale_color_manual(name = "Ideology", values = c("Left" = "blue", "Right" = "red"))
  }
  p + geom_point() + geom_smooth(method = "lm", alpha = 0.4) +
    theme_minimal() + labs(title = title, x = "Year", y = ylab)
}

# Populism over time
ggsave(plot_trend(year_agg, "populism_overall",
                  "Average Populism Score in CMP Manifestos Over Time",
                  "Populism Overall Score"),
       filename = "output/Populism_Over_Time.pdf", width = 10, height = 6)

ggsave(plot_trend(year_agg_ideo |> filter(!is.na(Ideology)),
                  "populism_overall",
                  "Average Populism Score in CMP Manifestos Over Time, split according to Ideology",
                  "Populism Overall Score", color_var = "Ideology"),
       filename = "output/Populism_Over_Time_by_Ideology.pdf", width = 10, height = 6)

# Liberalism over time
ggsave(plot_trend(year_agg, "liberalism_overall",
                  "Average Liberalism Score in CMP Manifestos Over Time",
                  "Liberalism Overall Score"),
       filename = "output/Liberalism_Over_Time.pdf", width = 10, height = 6)

ggsave(plot_trend(year_agg_ideo |> filter(!is.na(Ideology)),
                  "liberalism_overall",
                  "Average Liberalism Score in CMP Manifestos Over Time, split according to Ideology",
                  "Liberalism Overall Score", color_var = "Ideology"),
       filename = "output/Liberalism_Over_Time_by_Ideology.pdf", width = 10, height = 6)

# ---- Country Scatterplots ----

# Axis limits per plot variable: rescaled ideology in [-1,+1], scores in [0,10].
axis_limits <- function(var) if (var == "Populist Ideology") c(-1, 1) else c(0, 10)

# Generic scatterplot: x vs y, colored by party (or ideology), labeled with abbreviations
plot_country_scatter <- function(data, country, xvar, yvar, xlab, ylab, title,
                                 color_var = "Party Name",
                                 spain_filter = TRUE) {
  df <- data |> filter(countryname == country)
  # For Spain, subset to major parties
  if (country == "Spain" && spain_filter) {
    df <- df |> filter(grepl(spain_parties_regex, `Party Name`))
  }
  ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]],
                 color = .data[[color_var]], size = `Election Date`)) +
    geom_point() +
    geom_label_repel(aes(label = partyabbrev), nudge_y = 0.3, size = 3,
                     show.legend = FALSE) +
    theme_minimal() +
    coord_cartesian(xlim = axis_limits(xvar), ylim = axis_limits(yvar)) +
    labs(title = title, x = xlab, y = ylab, color = color_var)
}

# Define all plot specifications as a tibble for map-based generation.
# Note: "Populist Ideology" is the rescaled measure in [-1, +1] (see pop_ideology_overall2);
# the 0-10 sub-score `pop_ideology_overall` is NOT used in these plots.
plot_specs <- tribble(
  ~xvar,              ~yvar,                ~xlab,                                                              ~ylab,                        ~title_template,                                          ~suffix,
  "Populist Ideology", "Populism Overall",    "Populist Ideology (-1 = Left, 0 = Centrist/mixed, +1 = Right)",   "Populism Overall Score",     "Populism vs. Populist Ideology in CMP Manifestos",       "Populism_vs_Populist_Ideology",
  "Populist Ideology", "Liberalism Overall",  "Populist Ideology (-1 = Left, 0 = Centrist/mixed, +1 = Right)",   "Liberalism Overall Score",   "Liberalism vs. Populist Ideology in CMP Manifestos",     "Liberalism_vs_Populist_Ideology",
  "Populist Ideology", "Economic Liberalism", "Populist Ideology (-1 = Left, 0 = Centrist/mixed, +1 = Right)",   "Economic Liberalism Score",  "Economic Liberalism vs. Populist Ideology in CMP Manifestos", "Economic_Liberalism_vs_Populist_Ideology",
  "Populism Overall",  "Liberalism Overall",  "Populism Overall Score",                                          "Liberalism Overall Score",   "Liberalism vs. Populism in CMP Manifestos",              "Liberalism_vs_Populism",
  "Populism Overall",  "Economic Liberalism", "Populism Overall Score",                                          "Economic Liberalism Score",  "Economic Liberalism vs. Populism in CMP Manifestos",     "Economic_Liberalism_vs_Populism"
)

countries <- c("Germany", "Austria", "Spain", "Poland")

# Generate all country scatterplots (color by Party Name)
walk(countries, function(ctry) {
  pwalk(plot_specs, function(xvar, yvar, xlab, ylab, title_template, suffix) {
    p <- plot_country_scatter(raw_out_more, ctry, xvar, yvar, xlab, ylab,
                              title_template, color_var = "Party Name")
    ggsave(p, filename = sprintf("output/%s_%s.pdf", ctry, suffix),
           width = 15, height = 8)
  })
})

# v2 variants: color by continuous Populist Ideology (only for Lib vs Pop, EconLib vs Pop)
v2_specs <- plot_specs |> filter(xvar == "Populism Overall")

walk(countries, function(ctry) {
  pwalk(v2_specs, function(xvar, yvar, xlab, ylab, title_template, suffix) {
    p <- plot_country_scatter(raw_out_more, ctry, xvar, yvar, xlab, ylab,
                              title_template, color_var = "Populist Ideology")
    ggsave(p, filename = sprintf("output/%s_%s_v2.pdf", ctry, suffix),
           width = 15, height = 8)
  })
})

# ---- Panel Regressions ----

# Prepare regression data: add year column once
reg_data <- raw_out |> mutate(year = year(date))

# Dependent variables for the full specification tables (one table per DV)
lib_dvs <- c(
  "liberalism_overall"  = "Liberalism",
  "lib_political"       = "Liberalism (Political)",
  "lib_social"          = "Liberalism (Social)",
  "lib_economic"        = "Liberalism (Economic)",
  "lib_financial_market" = "Liberalism (Financial Market)"
)

# Coefficient renaming for all regression tables
coef_labels <- c(
  "populism_overall"                        = "Populism Overall",
  "pop_ideology_overall2"                   = "Populist Ideology",
  "populism_overall:pop_ideology_overall2"  = "Populism x Ideology"
)

# Common modelsummary options
ms_opts <- list(
  statistic = c("[t]" = "statistic"),
  gof_omit  = "AIC|BIC|Log.Lik.|F|Std.Errors|Within|Pseudo|Adj.R2",
  stars     = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
  coef_rename = coef_labels
)

# Run 6-model specification table for each DV
walk(names(lib_dvs), function(dv) {
  fml <- function(rhs, fe) as.formula(paste(dv, "~", rhs, "|", fe))

  models <- list(
    "Country + Year FE"              = feols(fml("populism_overall", "countryname + year"), data = reg_data),
    "Country + Year FE (Ideology)"   = feols(fml("pop_ideology_overall2", "countryname + year"), data = reg_data),
    "Country FE"                     = feols(fml("populism_overall + pop_ideology_overall2", "countryname"), data = reg_data),
    "Country + Year FE"              = feols(fml("populism_overall + pop_ideology_overall2", "countryname + year"), data = reg_data),
    "Country + Year FE (Interaction)" = feols(fml("populism_overall * pop_ideology_overall2", "countryname + year"), data = reg_data),
    "Country + Year + Party FE"      = feols(fml("populism_overall + pop_ideology_overall2", "countryname + year + partyname"), data = reg_data)
  )

  cat("\n\n% ---- Regression Table:", lib_dvs[[dv]], "----\n")
  ms_call <- c(
    list(models  = models,
         output  = "latex",
         title   = paste("Regression Results:", lib_dvs[[dv]],
                         "Regressed on Populism and Populist Ideology"),
         label   = paste0("tab:reg_", dv)),
    ms_opts
  )
  print(do.call(modelsummary, ms_call))
})

# ---- Cross-DV Comparison Table (Country + Year FE) ----

models_cross <- map(names(lib_dvs), function(dv) {
  feols(as.formula(paste(dv, "~ populism_overall + pop_ideology_overall2 | countryname + year")),
        data = reg_data)
}) |> set_names(lib_dvs)

cat("\n\n% ---- Cross-DV Comparison Table ----\n")
ms_cross <- c(
  list(models  = models_cross,
       output  = "latex",
       title   = "Regression Results: Different Liberalism Measures Regressed on Populism and Populist Ideology",
       label   = "tab:regression_results_lib_measures"),
  ms_opts
)
print(do.call(modelsummary, ms_cross))

# ---- Marginal Effects Plots ----

# Reference lines for marginal effect plots (average marginal effect from
# the non-interacted model, used as visual benchmark)
me_ref_lines <- c(
  liberalism_overall   = -0.182,
  lib_political        = -0.113,
  lib_social           = -0.155,
  lib_economic         = -0.219,
  lib_financial_market = -0.250
)

# y-axis lower bounds for each DV
me_ylim <- c(
  liberalism_overall   = -0.30,
  lib_political        = -0.30,
  lib_social           = -0.30,
  lib_economic         = -0.35,
  lib_financial_market = -0.50
)

# Clean labels for plot titles
me_labels <- c(
  liberalism_overall   = "Liberalism",
  lib_political        = "Political Liberalism",
  lib_social           = "Social Liberalism",
  lib_economic         = "Economic Liberalism",
  lib_financial_market = "Financial Market Liberalism"
)

walk(names(lib_dvs), function(dv) {
  fml <- as.formula(paste(dv, "~ populism_overall * pop_ideology_overall2 | countryname + year"))
  mod <- feols(fml, data = reg_data)

  p <- plot_slopes(mod,
                   variables = "populism_overall",
                   condition = "pop_ideology_overall2",
                   vcov = ~partyname) +
    labs(
      title = paste("Marginal Effect of Populism on", me_labels[[dv]], "by Populist Ideology"),
      x     = "Populist Ideology (0=Left, 5=Centrist/mixed, 10=Right)",
      y     = paste("Marginal Effect of Populism on", me_labels[[dv]])
    ) +
    theme_minimal() +
    ylim(me_ylim[[dv]], 0) +
    geom_hline(yintercept = 0, lty = 2, col = "gray") +
    geom_hline(yintercept = me_ref_lines[[dv]], lty = 2, col = "red")

  fname <- paste0("output/Marginal_Effect_Populism_on_",
                  gsub(" ", "_", me_labels[[dv]]),
                  "_by_Ideology.pdf")
  ggsave(p, filename = fname, width = 8, height = 5)
})

message("Done. All tables and figures exported to output/.")
