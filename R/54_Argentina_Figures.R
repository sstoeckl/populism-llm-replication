# ============================================================
# 54_Argentina_Figures.R  (PLLM-0185, 2026-10-28)
#
# Figures for the Milei question: La Libertad Avanza (2021 CF, 2023 PASO/general ON, 2025 CF/BA)
# against the baseline averages of all, right-wing and right-wing populist parties in the world,
# Latin America and Argentina (R/53 csv), on the populism-liberalism plane and on the
# predicted-liberalism surface of the baseline regressions. Style as the combined v2026a paper
# figures of R/41 section 13 (theme_minimal 11pt, Left #2b5fb3 / Right #c0392b, legend at the
# bottom, no titles; what the figure shows goes into the caption). Reads only R/53 outputs and the
# baseline; writes PIPE_OUT/argentina/figures/*.pdf (deterministic, save_pdf).
#
#   Milei_vs_Averages.pdf   seven panels (populism, ideology, five liberalism scores): baseline means
#                           ± 1 SD for world / Latin America / Argentina x all / right-wing /
#                           right-wing populist; LLA platforms as red diamonds, the other Argentine
#                           2021-2025 platforms as blue dots
#   Milei_Scatter.pdf       populism vs economic and overall liberalism: baseline cloud with OLS
#                           line, Argentina 1989-2019 in black, LLA red, other 2021-2025 platforms blue
#   Milei_Predicted.pdf     scored vs predicted liberalism (model (5) of Tables 4-8: populism x
#                           ideology, country and year FE), five panels, 45-degree line
#   Milei_Trajectory.pdf    LLA 2021 -> 2025: populism, ideology and the five liberalism scores
#
# PLLM-0252 (2026-11-11): Milei_vs_Averages_US, Milei_Scatter_US, Milei_Trajectory_US (+ _note.tex): the same
# figures with the Republican Party platforms 2016/2020/2024 (baseline, R/53 csv/53_us_republican.csv) as a
# second reference in teal; the original figures are unchanged.
# PLLM-0256 (2026-11-11): next to each pdf a <name>_note.tex with the Title Case caption (comment) and
# a self-contained note to paste below the figure in the paper; "v2026a" dropped from the captions
# inside the figures (PLLM-0240).
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(ggrepel); library(scales)
})
source("R/0_Config.R")
OUT <- file.path(PIPE_OUT, "argentina"); CSV <- file.path(OUT, "csv"); FIG <- file.path(OUT, "figures")
dir.create(FIG, recursive = TRUE, showWarnings = FALSE)

scores <- read_csv(file.path(CSV, "53_argentina_scores.csv"), show_col_types = FALSE) |>
  mutate(date = as.Date(date), IDEO = pop_ideology_direction,
         label = paste0(format(date, "%Y"), if_else(district == "ON", "", paste0(" ", district)), if_else(stage == "PASO", " PASO", "")),
         label_full = paste(abbrev, label),
         group = if_else(milei == 1, "La Libertad Avanza", "Other Argentine platforms 2021-2025"))
gm   <- read_csv(file.path(CSV, "53_group_means.csv"), show_col_types = FALSE)
pred <- read_csv(file.path(CSV, "53_predicted_liberalism.csv"), show_col_types = FALSE)
bl   <- read_parquet(file.path(PIPE_DERIVED, "manifesto_scores_v2026a.parquet")) |> mutate(IDEO = pop_ideology_direction)

OUTC <- c(populism_overall = "Populism (overall)", IDEO = "Ideology", lib_political = "Political liberalism",
          lib_social = "Social liberalism", lib_economic = "Economic liberalism",
          lib_financial_market = "Financial-market liberalism", liberalism_overall = "Liberalism (overall)")
SUBG <- c(all = "All parties", right = "Right-wing", right_populist = "Right-wing populist")
REG  <- c("World", "Latin America", "Argentina")
th13 <- theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), legend.position = "bottom",
                                              legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
LLA_COL <- "#c0392b"; OTH_COL <- "#2b5fb3"
COL_SET <- c("La Libertad Avanza" = LLA_COL, "Other Argentine platforms 2021-2025" = OTH_COL,
             "Argentina 1989-2019 (baseline)" = "black")
COL_SUB <- c("All parties" = "grey30", "Right-wing" = "#e67e22", "Right-wing populist" = "#8e44ad")

# ---- 1. Milei vs averages ------------------------------------------------------------------
g1 <- gm |> filter(subgroup %in% names(SUBG), outcome %in% names(OUTC), !is.na(mean)) |>
  mutate(region = factor(region, REG), subgroup = factor(SUBG[subgroup], SUBG), outcome = factor(OUTC[outcome], OUTC))
pts <- scores |> select(doc_id, label, group, all_of(names(OUTC))) |>
  pivot_longer(all_of(names(OUTC)), names_to = "outcome", values_to = "value") |>
  filter(!is.na(value)) |> mutate(outcome = factor(OUTC[outcome], OUTC), x = factor("Argentina", REG))
p1 <- ggplot(g1, aes(x = region, y = mean, colour = subgroup)) +
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(width = 0.6), size = 0.3, linewidth = 0.45) +
  geom_point(data = pts |> filter(group != "La Libertad Avanza"), aes(x = x, y = value, shape = group), inherit.aes = FALSE,
             colour = OTH_COL, alpha = 0.55, size = 1.2, position = position_nudge(x = 0.45)) +
  geom_point(data = pts |> filter(group == "La Libertad Avanza"), aes(x = x, y = value, shape = group), inherit.aes = FALSE,
             colour = LLA_COL, size = 2.6, position = position_nudge(x = 0.45)) +
  geom_text_repel(data = pts |> filter(group == "La Libertad Avanza"), aes(x = x, y = value, label = label), inherit.aes = FALSE,
                  colour = LLA_COL, size = 2.2, nudge_x = 0.62, direction = "y", hjust = 0, seed = 1,
                  segment.size = 0.2, min.segment.length = 0, max.time = 5, max.iter = 1e5) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 4) +
  scale_colour_manual(values = COL_SUB, name = "Baseline mean ± 1 SD:") +
  scale_shape_manual(values = c("La Libertad Avanza" = 18, "Other Argentine platforms 2021-2025" = 16), name = "Out of sample:") +
  scale_x_discrete(expand = expansion(add = c(0.6, 1.9)), labels = c(World = "World", `Latin America` = "Latin\nAmerica", Argentina = "Argentina")) +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2, override.aes = list(colour = c(LLA_COL, OTH_COL), size = c(2.6, 1.6)))) +
  labs(x = NULL, y = "Score (0-10); ideology -1 to +1",
       caption = paste0("Baseline: manifesto-level means with ± 1 SD; right-wing = ideology > 0; right-wing populist = ideology > 0 and populism above the baseline median.\n",
                        "Out of sample: platforms filed with the Cámara Nacional Electoral, scored with the identical pipeline (CF = Capital Federal, BA = Buenos Aires province).")) +
  th13 + theme(plot.caption = element_text(hjust = 0, size = 7))
save_pdf(p1, file.path(FIG, "Milei_vs_Averages.pdf"), 8, 6.2)

# ---- 2. Scatter: populism vs liberalism ----------------------------------------------------
sc_long <- function(dv) {
  b <- bl |> transmute(x = populism_overall, y = .data[[dv]],
                       set = if_else(countryname == "Argentina", "Argentina 1989-2019 (baseline)", "baseline"),
                       label = if_else(countryname == "Argentina", paste(partyabbrev, format(date, "%Y")), NA_character_))
  a <- scores |> transmute(x = populism_overall, y = .data[[dv]], set = group, label = if_else(group == "La Libertad Avanza", label, label_full))
  bind_rows(b, a) |> filter(!is.na(x), !is.na(y)) |> mutate(dv = OUTC[[dv]])
}
sc <- bind_rows(sc_long("lib_economic"), sc_long("liberalism_overall")) |> mutate(dv = factor(dv, OUTC))
p2 <- ggplot() +
  geom_point(data = sc |> filter(set == "baseline"), aes(x, y), colour = "grey72", size = 0.6, alpha = 0.5) +
  geom_smooth(data = sc |> filter(set == "baseline"), aes(x, y), method = "lm", formula = y ~ x, colour = "grey30", linewidth = 0.6, se = FALSE) +
  geom_point(data = sc |> filter(set == "Argentina 1989-2019 (baseline)"), aes(x, y, colour = set, shape = set), size = 1.5) +
  geom_point(data = sc |> filter(set == "Other Argentine platforms 2021-2025"), aes(x, y, colour = set, shape = set), size = 1.7) +
  geom_point(data = sc |> filter(set == "La Libertad Avanza"), aes(x, y, colour = set, shape = set), size = 3.2) +
  geom_text_repel(data = sc |> filter(set == "La Libertad Avanza"), aes(x, y, label = label), colour = LLA_COL,
                  size = 2.4, seed = 1, segment.size = 0.2, max.overlaps = 20, max.time = 5, max.iter = 1e5) +
  facet_wrap(~ dv) +
  scale_colour_manual(values = COL_SET, name = NULL, breaks = names(COL_SET)) +
  scale_shape_manual(values = c("La Libertad Avanza" = 18, "Other Argentine platforms 2021-2025" = 16, "Argentina 1989-2019 (baseline)" = 17),
                     name = NULL, breaks = names(COL_SET)) +
  labs(x = "Populism (overall)", y = "Liberalism",
       caption = "Grey: baseline manifestos with the OLS line; La Libertad Avanza and the other Argentine 2021-2025 platforms are scored out of sample.") +
  th13 + theme(plot.caption = element_text(hjust = 0, size = 7))
save_pdf(p2, file.path(FIG, "Milei_Scatter.pdf"), 8, 4.3)

# ---- 3. Scored vs predicted liberalism -------------------------------------------------------
DV13 <- c(liberalism_overall = "Overall", lib_political = "Political", lib_social = "Social",
          lib_economic = "Economic", lib_financial_market = "Financial markets")
pr <- pred |> mutate(outcome = factor(DV13[outcome], DV13),
                     group = if_else(milei == 1, "La Libertad Avanza", "Other Argentine platforms 2021-2025"),
                     label = paste0(format(as.Date(date), "%Y"), if_else(district == "ON", "", paste0(" ", district))))
lim <- c(0, 10)
p3 <- ggplot(pr, aes(pred_interaction, actual, colour = group, shape = group)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = 2) +
  geom_point(data = pr |> filter(group != "La Libertad Avanza"), size = 1.6, alpha = 0.8) +
  geom_point(data = pr |> filter(group == "La Libertad Avanza"), size = 3) +
  geom_text_repel(data = pr |> filter(group == "La Libertad Avanza"), aes(label = label), size = 2.2, seed = 1,
                  segment.size = 0.2, show.legend = FALSE, max.time = 5, max.iter = 1e5) +
  facet_wrap(~ outcome, nrow = 1) + coord_equal(xlim = lim, ylim = lim) + scale_x_continuous(breaks = c(0, 5, 10)) + scale_y_continuous(breaks = c(0, 5, 10)) +
  scale_colour_manual(values = COL_SET[1:2], name = NULL) + scale_shape_manual(values = c(18, 16), name = NULL) +
  labs(x = "Liberalism predicted from populism and ideology (model (5) of Tables 4-8, country and year FE)", y = "Liberalism, scored",
       caption = "Points above the dashed line are more liberal than the baseline model predicts for a party with the same populism and ideology in Argentina in that year.") +
  th13 + theme(plot.caption = element_text(hjust = 0, size = 7))
save_pdf(p3, file.path(FIG, "Milei_Predicted.pdf"), 8, 2.9)

# ---- 4. LLA trajectory -----------------------------------------------------------------------
tr <- scores |> filter(milei == 1) |> select(label, date, district, all_of(names(OUTC))) |>
  pivot_longer(all_of(names(OUTC)), names_to = "outcome", values_to = "value") |>
  mutate(outcome = factor(OUTC[outcome], OUTC), label = factor(label, unique(label[order(date, district)])))
# reference lines: baseline means of all parties in the world, Latin America and Argentina, and of right-wing populists worldwide
REFL <- c("World, all parties" = "grey55", "Latin America, all parties" = "grey35", "Argentina, all parties" = "black",
          "World, right-wing populist" = "#8e44ad")
refs <- gm |> filter(outcome %in% names(OUTC), !is.na(mean),
                     (subgroup == "all" & region %in% REG) | (subgroup == "right_populist" & region == "World")) |>
  mutate(ref = if_else(subgroup == "all", paste0(region, ", all parties"), "World, right-wing populist"),
         ref = factor(ref, names(REFL)), outcome = factor(OUTC[outcome], OUTC)) |> select(outcome, ref, mean)
p4 <- ggplot(tr, aes(x = label, y = value, group = outcome)) +
  geom_hline(data = refs, aes(yintercept = mean, colour = ref, linetype = ref), linewidth = 0.45) +
  geom_line(colour = LLA_COL, alpha = 0.5) + geom_point(colour = LLA_COL, size = 2, shape = 18) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 4) +
  scale_colour_manual(values = REFL, name = "Baseline mean:") +
  scale_linetype_manual(values = c("dashed", "dotted", "dotdash", "solid"), name = "Baseline mean:") +
  labs(x = NULL, y = "Score (0-10); ideology -1 to +1",
       caption = "Red: La Libertad Avanza platforms filed with the Cámara Nacional Electoral, 2021 Capital Federal (CF), 2023 national (PASO and general election), 2025 Buenos Aires province (BA) and CF.\nLines: baseline means; right-wing populist = ideology > 0 and populism above the baseline median.") +
  th13 + theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.caption = element_text(hjust = 0, size = 7))
save_pdf(p4, file.path(FIG, "Milei_Trajectory.pdf"), 8, 4.6)
# ---- 4b. Variants with the US Republican platforms 2016/2020/2024 (PLLM-0252) ------------------
# Same figures with the Republican Party platforms (CMP 61620; baseline manifestos, so in sample) as
# a second reference: teal squares. Written as *_US.pdf next to the originals, which stay unchanged.
US_COL <- "#138d75"; US_SET <- "US Republicans 2016-2024 (baseline)"
usr <- read_csv(file.path(CSV, "53_us_republican.csv"), show_col_types = FALSE) |>
  mutate(IDEO = ideology, label = paste0("GOP ", year), label_tr = paste0("GOP ", year, if_else(year == 2020, "*", "")))
# (1) vs averages: a fourth column "US Republicans"
REG_US <- c(REG, "gap", "US Republicans")     # empty spacer column keeps the LLA labels clear of the US points
pts_us <- usr |> select(label, all_of(names(OUTC))) |> pivot_longer(all_of(names(OUTC)), names_to = "outcome", values_to = "value") |>
  filter(!is.na(value)) |> mutate(outcome = factor(OUTC[outcome], OUTC), x = factor("US Republicans", REG_US), group = US_SET)
p1u <- ggplot(g1 |> mutate(region = factor(as.character(region), REG_US)), aes(x = region, y = mean, colour = subgroup)) +
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(width = 0.6), size = 0.3, linewidth = 0.45) +
  geom_point(data = pts |> filter(group != "La Libertad Avanza") |> mutate(x = factor("Argentina", REG_US)), aes(x = x, y = value, shape = group), inherit.aes = FALSE,
             colour = OTH_COL, alpha = 0.55, size = 1.2, position = position_nudge(x = 0.45)) +
  geom_point(data = pts |> filter(group == "La Libertad Avanza") |> mutate(x = factor("Argentina", REG_US)), aes(x = x, y = value, shape = group), inherit.aes = FALSE,
             colour = LLA_COL, size = 2.6, position = position_nudge(x = 0.45)) +
  geom_text_repel(data = pts |> filter(group == "La Libertad Avanza") |> mutate(x = factor("Argentina", REG_US)), aes(x = x, y = value, label = label), inherit.aes = FALSE,
                  colour = LLA_COL, size = 2.0, nudge_x = 0.62, direction = "y", hjust = 0, seed = 1,
                  segment.size = 0.2, min.segment.length = 0, max.time = 5, max.iter = 1e5) +
  geom_point(data = pts_us, aes(x = x, y = value, shape = group), inherit.aes = FALSE, colour = US_COL, size = 2.2) +
  geom_text_repel(data = pts_us, aes(x = x, y = value, label = label), inherit.aes = FALSE, colour = US_COL, size = 2.0,
                  nudge_x = 0.35, direction = "y", hjust = 0, seed = 1, segment.size = 0.2, min.segment.length = 0, max.time = 5, max.iter = 1e5) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 4) +
  scale_colour_manual(values = COL_SUB, name = "Baseline mean ± 1 SD:") +
  scale_shape_manual(values = setNames(c(18, 16, 15), c("La Libertad Avanza", "Other Argentine platforms 2021-2025", US_SET)), name = NULL) +
  scale_x_discrete(limits = REG_US, expand = expansion(add = c(0.6, 1.1)),
                   labels = c(World = "World", `Latin America` = "Latin\nAmerica", Argentina = "Argentina", gap = "", `US Republicans` = "US\nRepubl.")) +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2, override.aes = list(colour = c(LLA_COL, OTH_COL, US_COL), size = c(2.6, 1.6, 2.2)))) +
  labs(x = NULL, y = "Score (0-10); ideology -1 to +1",
       caption = paste0("Baseline: manifesto-level means with ± 1 SD; right-wing = ideology > 0; right-wing populist = ideology > 0 and populism above the baseline median.\n",
                        "Out of sample: platforms filed with the Cámara Nacional Electoral, scored with the identical pipeline (CF = Capital Federal, BA = Buenos Aires province).\n",
                        "Teal: Republican Party platforms 2016, 2020 (the 2016 platform re-adopted) and 2024 in the baseline.")) +
  th13 + theme(plot.caption = element_text(hjust = 0, size = 7), axis.text.x = element_text(size = 6.5))
save_pdf(p1u, file.path(FIG, "Milei_vs_Averages_US.pdf"), 9, 6.6)
# (2) scatter
COL_SET_US <- c(COL_SET, setNames(US_COL, US_SET))
sc_us <- bind_rows(map(c("lib_economic", "liberalism_overall"), function(dv)
  usr |> transmute(x = populism_overall, y = .data[[dv]], set = US_SET, label, dv = OUTC[[dv]]))) |> mutate(dv = factor(dv, OUTC))
p2u <- suppressMessages(p2 + geom_point(data = sc_us, aes(x, y, colour = set, shape = set), size = 2.4) +
  geom_text_repel(data = sc_us, aes(x, y, label = label), colour = US_COL, size = 2.3, seed = 1, segment.size = 0.2,
                  max.overlaps = 20, max.time = 5, max.iter = 1e5) +
  scale_colour_manual(values = COL_SET_US, name = NULL, breaks = names(COL_SET_US)) +
  scale_shape_manual(values = setNames(c(18, 16, 17, 15), names(COL_SET_US)), name = NULL, breaks = names(COL_SET_US)) +
  labs(caption = "Grey: baseline manifestos with the OLS line; La Libertad Avanza and the other Argentine 2021-2025 platforms are scored out of sample.\nTeal: Republican Party platforms 2016, 2020 (the 2016 platform re-adopted) and 2024 (baseline manifestos)."))
save_pdf(p2u, file.path(FIG, "Milei_Scatter_US.pdf"), 8, 4.6)
# (3) two trajectories on one election axis ordered by date
tr_us <- bind_rows(
  scores |> filter(milei == 1) |> transmute(label = paste("LLA", label), date, district, party = "La Libertad Avanza", across(all_of(names(OUTC)))),
  usr |> transmute(label = label_tr, date = as.Date(date), district = "US", party = "US Republicans", across(all_of(names(OUTC))))) |>
  pivot_longer(all_of(names(OUTC)), names_to = "outcome", values_to = "value") |>
  mutate(outcome = factor(OUTC[outcome], OUTC), label = factor(label, unique(label[order(date, district)])))
TRC <- c("La Libertad Avanza" = LLA_COL, "US Republicans" = US_COL)
p4u <- ggplot(tr_us, aes(x = label, y = value)) +
  geom_hline(data = refs, aes(yintercept = mean, linetype = ref), colour = "grey45", linewidth = 0.4) +
  geom_line(aes(group = interaction(outcome, party), colour = party), alpha = 0.5) +
  geom_point(aes(colour = party, shape = party), size = 2) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 4) +
  scale_colour_manual(values = TRC, name = NULL) + scale_shape_manual(values = c("La Libertad Avanza" = 18, "US Republicans" = 15), name = NULL) +
  scale_linetype_manual(values = c("dashed", "dotted", "dotdash", "solid"), name = "Baseline mean:") +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 1), linetype = guide_legend(order = 2)) +
  labs(x = NULL, y = "Score (0-10); ideology -1 to +1",
       caption = "Red: La Libertad Avanza platforms filed with the Cámara Nacional Electoral (out of sample).\nTeal: Republican Party platforms 2016, 2020 (*: the 2016 platform re-adopted) and 2024 (baseline).\nGrey lines: baseline means (world, Latin America, Argentina: all parties; world: right-wing populist = ideology > 0 and populism above the baseline median).") +
  th13 + theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 7), plot.caption = element_text(hjust = 0, size = 7))
save_pdf(p4u, file.path(FIG, "Milei_Trajectory_US.pdf"), 8.6, 5.2)
# ---- 5. Figure notes (PLLM-0256) --------------------------------------------------------------
write_fig_note <- function(name, caption, note) {
  write_atomic(c(sprintf("%% note for %s.pdf (generated by R/54_Argentina_Figures.R, PLLM-0256)", name),
                 sprintf("%% caption (Title Case): \\caption{%s}", caption),
                 sprintf("\\par\\smallskip\\begin{minipage}{\\linewidth}\\scriptsize\\textit{Notes:} %s\\end{minipage}", note)),
               file.path(FIG, paste0(name, "_note.tex")), writer = function(x, p) writeLines(x, p, useBytes = TRUE))
}
n_lla <- sum(scores$milei == 1); n_oth <- sum(scores$milei != 1)
pop_med <- median(bl$populism_overall, na.rm = TRUE)
cne <- "filed with the C\\'amara Nacional Electoral and scored out of sample with the identical pipeline and aggregation rule as the baseline (CF = Capital Federal, BA = Buenos Aires province; 2023 = national platform, filed for the primaries (PASO) and the general election)"
rp_def <- sprintf("right-wing = ideology $> 0$; right-wing populist = ideology $> 0$ and populism above the baseline median (%s)", formatC(pop_med, format = "f", digits = 2))
ar_bl <- bl |> filter(countryname == "Argentina", !is.na(populism_overall))
n_pr <- n_distinct(pr$doc_id)
write_fig_note("Milei_Predicted", "Liberalism of \\emph{La Libertad Avanza}, Scored and Predicted",
  sprintf("Horizontal axis: liberalism predicted by column (5) of Tables 4--8 (populism, ideology and their interaction; country and year fixed effects; estimated on the baseline manifestos), that is the liberalism of an Argentine party with the same populism and ideology scores in the same election year. Vertical axis: the scored liberalism (0--10). Points above the dashed 45-degree line are more liberal than predicted. Red diamonds: the %d platforms of La Libertad Avanza 2021--2025; blue dots: the %d other Argentine platforms 2021--2025 with populism and ideology scores; all %s.",
          sum(pr$milei[!duplicated(pr$doc_id)] == 1), sum(pr$milei[!duplicated(pr$doc_id)] != 1), cne))
write_fig_note("Milei_vs_Averages", "\\emph{La Libertad Avanza} against the Baseline Averages",
  sprintf("Panels: populism, ideology ($-$1 to +1) and the five liberalism scores (0--10). Bars: mean $\\pm$ 1 standard deviation over the baseline manifestos of all parties, right-wing and right-wing populist parties in the world, Latin America and Argentina; %s; no Argentine baseline manifesto has an ideology score above zero. Red diamonds: the %d platforms of La Libertad Avanza 2021--2025; blue dots: the %d other Argentine platforms 2021--2025; all %s.",
          rp_def, n_lla, n_oth, cne))
write_fig_note("Milei_Scatter", "The Milei Question on the Populism--Liberalism Plane",
  sprintf("Overall populism (horizontal) against economic liberalism (left) and overall liberalism (right), 0--10. Grey: the baseline manifestos with the OLS line; black triangles: the %d Argentine baseline manifestos (%d--%d); red diamonds: the %d platforms of La Libertad Avanza 2021--2025; blue dots: the %d other Argentine platforms 2021--2025, %s.",
          nrow(ar_bl), min(ar_bl$year), max(ar_bl$year), n_lla, n_oth, cne))
write_fig_note("Milei_Trajectory", "\\emph{La Libertad Avanza} 2021--2025 by Election",
  sprintf("Scores of the %d platforms of La Libertad Avanza by election, %s. Panels: populism, ideology ($-$1 to +1) and the five liberalism scores (0--10). Horizontal lines: baseline means of all parties in the world, Latin America and Argentina, and of right-wing populist parties in the world (%s).",
          n_lla, cne, rp_def))
# notes for the _US variants (PLLM-0252)
us_txt <- sprintf("Teal squares: the platforms of the Republican Party (Manifesto Project code 61620) of %s, baseline manifestos; the 2020 convention re-adopted the 2016 platform.",
                  paste(usr$year, collapse = ", "))
write_fig_note("Milei_vs_Averages_US", "\\emph{La Libertad Avanza} and the US Republicans against the Baseline Averages",
  sprintf("Panels: populism, ideology ($-$1 to +1) and the five liberalism scores (0--10). Bars: mean $\\pm$ 1 standard deviation over the baseline manifestos of all parties, right-wing and right-wing populist parties in the world, Latin America and Argentina; %s; no Argentine baseline manifesto has an ideology score above zero. Red diamonds: the %d platforms of La Libertad Avanza 2021--2025; blue dots: the %d other Argentine platforms 2021--2025; all %s. %s",
          rp_def, n_lla, n_oth, cne, us_txt))
write_fig_note("Milei_Scatter_US", "The Milei Question on the Populism--Liberalism Plane, with the US Republicans",
  sprintf("Overall populism (horizontal) against economic liberalism (left) and overall liberalism (right), 0--10. Grey: the baseline manifestos with the OLS line; black triangles: the %d Argentine baseline manifestos (%d--%d); red diamonds: the %d platforms of La Libertad Avanza 2021--2025; blue dots: the %d other Argentine platforms 2021--2025, %s. %s",
          nrow(ar_bl), min(ar_bl$year), max(ar_bl$year), n_lla, n_oth, cne, us_txt))
write_fig_note("Milei_Trajectory_US", "\\emph{La Libertad Avanza} 2021--2025 and the US Republicans 2016--2024 by Election",
  sprintf("Scores of the %d platforms of La Libertad Avanza (red diamonds), %s, and of the Republican Party platforms (teal squares; baseline manifestos; 2020 re-adopted the 2016 platform), ordered by election date. Panels: populism, ideology ($-$1 to +1) and the five liberalism scores (0--10). Horizontal lines: baseline means of all parties in the world, Latin America and Argentina, and of right-wing populist parties in the world (%s).",
          n_lla, cne, rp_def))
message("R/54 done -> ", FIG)
