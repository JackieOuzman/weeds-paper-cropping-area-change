# ==============================================================
# AWC 2026 Talk — Slide 5b: Cost of weeds over time (% split)
# 
# Purpose: Rebuild the "Cost of agricultural weeds in Australia"
# chart to match the MS's percentage-split framing (revenue loss
# vs expenditure as % of total cost) rather than $ totals.
#
# Source data: timeseries_approach.xlsx, tab "Table for MS GDP"
# (W:\Economic impact of weeds round 2\model\4.compare older 
# model studies\timeseries_approach.xlsx)
#
# Output: stacked % bar chart, 6 studies (Combellack 1987 -> 
# Ouzman 2025), matching MS Figure [X] panel A
# ==============================================================

library(readxl)

file_path <- "W:/Economic impact of weeds round 2/model/4.compare older model studies/timeseries_approach.xlsx"

raw <- read_excel(file_path, sheet = "Table for MS GDP", col_names = FALSE)

# quick look to confirm we're reading the right rows/cols before
# we do anything else with it
print(raw, n = 20)
# Studies and years are in row 3/4, columns 4:10
studies <- as.character(raw[3, 4:10])
years   <- as.character(raw[4, 4:10])

# % split rows: 19 = Revenue losses, 20 = Expenditure
rev_pct <- as.numeric(raw[19, 4:10])
exp_pct <- as.numeric(raw[20, 4:10])

plot_data <- data.frame(
  study = studies,
  year  = years,
  revenue_loss_pct = rev_pct,
  expenditure_pct  = exp_pct
)

print(plot_data)


library(dplyr)
library(tidyr)

plot_data_long <- plot_data %>%
  filter(study != "McLeod  2018") %>%
  pivot_longer(
    cols = c(revenue_loss_pct, expenditure_pct),
    names_to = "component",
    values_to = "pct"
  ) %>%
  mutate(
    component = recode(component,
                       revenue_loss_pct = "Revenue loss",
                       expenditure_pct  = "Expenditure"
    ),
    # keep studies in chronological order on the x-axis rather than
    # alphabetical, and shorten labels for a cleaner axis
    study = factor(study, levels = plot_data$study[plot_data$study != "McLeod  2018"])
  )

print(plot_data_long)
library(ggplot2)

p <- ggplot(plot_data_long, aes(x = study, y = pct, fill = component)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Revenue loss" = "#00A9CE", "Expenditure" = "#003A5D")) +
  labs(
    x = NULL,
    y = "Share of total cost of weeds",
    fill = NULL,
    title = "Revenue loss vs expenditure share, 1987–2025"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0)
  )

p <- p + scale_x_discrete(labels = c(
  "Combellack\n1987",
  "Jones\n2005",
  "Sinden\n2004",
  "Llewellyn\n2016",
  "Hafi\n2023",
  "Ouzman\n2025"
))
p
library(stringr)
p + scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(axis.text.x = element_text(size = 11))

p 


ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/revenue_loss_vs_expenditure_1987-2025.png",
  plot = p,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)


raw2 <- read_excel(file_path, sheet = "Plots for MS GDP", col_names = FALSE)

# adjusted $ rows: 21 = Revenue losses, 22 = Expenditure
rev_adj <- as.numeric(raw2[21, 4:10])
exp_adj <- as.numeric(raw2[22, 4:10])

plot_data_adj <- data.frame(
  study = studies,
  year  = years,
  revenue_loss_adj = rev_adj,
  expenditure_adj  = exp_adj
) %>%
  filter(study != "McLeod  2018")

print(plot_data_adj)


plot_data_adj_long <- plot_data_adj %>%
  mutate(total = revenue_loss_adj + expenditure_adj) %>%
  pivot_longer(
    cols = c(revenue_loss_adj, expenditure_adj),
    names_to = "component",
    values_to = "value_adj"
  ) %>%
  mutate(
    component = recode(component,
                       revenue_loss_adj = "Revenue loss",
                       expenditure_adj  = "Expenditure"
    ),
    pct = value_adj / total,
    study = factor(study, levels = plot_data_adj$study)
  )

print(plot_data_adj_long)


p2 <- ggplot(plot_data_adj_long, aes(x = study, y = value_adj, fill = component)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = "B")) +
  scale_x_discrete(labels = c(
    "Combellack\n1987",
    "Jones\n2005",
    "Sinden\n(2004)",
    "Llewellyn\n(2016)",
    "Hafi\n(2023)",
    "Ouzman\n2025"
  )) +
  scale_fill_manual(values = c("Revenue loss" = "#00A9CE", "Expenditure" = "#003A5D")) +
  labs(
    x = NULL,
    y = "Total cost of weeds (2021 $, billions)",
    fill = NULL,
    title = "Cost of weeds: total spend and revenue loss share, 1987–2025"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

p2

ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/total_cost_adjusted_1987-2025.png",
  plot = p2,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)


totals_label <- plot_data_adj_long %>%
  distinct(study, total) %>%
  mutate(label = scales::label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1)(total))

p <- p +
  geom_text(
    data = totals_label,
    aes(x = study, y = 1.05, label = label),
    inherit.aes = FALSE,
    size = 3.8,
    color = "grey30"
  ) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.08)))

p <- p +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0.1))
  )

p
ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/revenue_loss_vs_expenditure_1987-2025.png",
  plot = p,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)


p <- p + scale_x_discrete(labels = c(
  "Combellack\n1981–82",
  "Jones\n1998–99",
  "Sinden\n2001–02",
  "Llewellyn\n2011–13",
  "Hafi\n2020–21",
  "Ouzman\n2019–21"
))

p
library(stringr)
p + scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(axis.text.x = element_text(size = 11))

p

ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/revenue_loss_vs_expenditure_1987-2025.png",
  plot = p,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

