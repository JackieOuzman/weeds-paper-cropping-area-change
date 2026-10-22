# ==============================================================
# MS Figure 6 — Revenue loss vs expenditure share
# Revenue loss and weed control expenditure as shares of the total cost
# of weeds in six national assessments, with the inflation-adjusted
# total (2019–20 $) shown above each bar.
# Source: timeseries_approach.xlsx, tabs "Table for MS GDP" and "Plots for MS GDP"
# Output: vector PDF + 600 dpi PNG for the manuscript
# ==============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ---- 0. Settings ----------------------------------------------

fig_dir   <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/Figure_for_ Journal_Paper/"   # same folder as the other figure scripts
file_path <- "W:/Economic impact of weeds round 2/model/4.compare older model studies/timeseries_approach.xlsx"

# Shared journal figure theme (same as the Figure 7, 8 and 9 scripts)
theme_ms <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "Arial") +
    theme(
      legend.position    = "top",
      legend.title       = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = "black"),
      plot.margin        = margin(5, 8, 5, 5)
    )
}

# ---- 1. Load and prep data ------------------------------------

raw  <- read_excel(file_path, sheet = "Table for MS GDP",  col_names = FALSE)
raw2 <- read_excel(file_path, sheet = "Plots for MS GDP",  col_names = FALSE)

studies <- as.character(raw[3, 4:10])
years   <- as.character(raw[4, 4:10])

# % split rows: 19 = Revenue losses, 20 = Expenditure
shares <- data.frame(
  study          = studies,
  year           = years,
  `Revenue loss` = as.numeric(raw[19, 4:10]),
  Expenditure    = as.numeric(raw[20, 4:10]),
  check.names = FALSE
) %>%
  filter(study != "McLeod  2018")

study_levels <- shares$study     # chronological order from the sheet

shares_long <- shares %>%
  pivot_longer(cols = c("Revenue loss", "Expenditure"),
               names_to = "component", values_to = "pct") %>%
  mutate(
    component = factor(component, levels = c("Expenditure", "Revenue loss")),   # Expenditure stacks on top
    study     = factor(study, levels = study_levels)
  )

print(shares_long)

# Adjusted $ rows (2019–20 dollars): 21 = Revenue losses, 22 = Expenditure
rev_adj <- as.numeric(raw2[21, 4:10])
exp_adj <- as.numeric(raw2[22, 4:10])

totals_label <- data.frame(study = studies, total = rev_adj + exp_adj) %>%
  filter(study != "McLeod  2018") %>%
  mutate(
    study = factor(study, levels = study_levels),
    label = scales::label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1)(total)
  )

print(totals_label)

labels_study_yr <- c(
  "Combellack\n1981–82",
  "Jones\n1998–99",
  "Sinden\n2001–02",
  "Llewellyn\n2011–13",
  "Hafi\n2020–21",
  "Ouzman\n2019–21"
)

# ---- 2. Plot ---------------------------------------------------

p_fig6 <- ggplot(shares_long, aes(x = study, y = pct, fill = component)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    colour = "white", size = 3.2, fontface = "bold"
  ) +
  geom_text(
    data = totals_label,
    aes(x = study, y = 1.05, label = label),
    inherit.aes = FALSE, size = 3.2, colour = "grey30"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_discrete(labels = labels_study_yr) +
  scale_fill_manual(values = c("Revenue loss" = "#00A9CE", "Expenditure" = "#003A5D")) +
  labs(x = NULL, y = "Share of total cost of weeds", fill = NULL) +   # no title: it goes in the caption
  theme_ms(base_size = 10)

p_fig6

# ---- 3. Save ---------------------------------------------------

# Journal version: vector PDF with embedded fonts
ggsave(file.path(fig_dir, "Fig6_revenue_loss_vs_expenditure_share.pdf"),
       plot = p_fig6, width = 140, height = 90, units = "mm", device = cairo_pdf)

# High-resolution PNG for the Word draft
ggsave(file.path(fig_dir, "Fig6_revenue_loss_vs_expenditure_share.png"),
       plot = p_fig6, width = 140, height = 90, units = "mm", dpi = 600, bg = "white")

