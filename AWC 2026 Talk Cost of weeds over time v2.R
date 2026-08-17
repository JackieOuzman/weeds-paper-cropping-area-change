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
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

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




plot_data_long_1 <- plot_data %>%
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

print(plot_data_long_1)

###############################################################################
### add rev_adj and exp_adj data 
###############################################################################
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


plot_data_adj_long_2 <- plot_data_adj %>%
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
    pct_adj = value_adj / total,
    study = factor(study, levels = plot_data_adj$study)
  )

print(plot_data_long_1)
print(plot_data_adj_long_2)

checkplot_data_adj_long_1_and_2 <- left_join(plot_data_long_1, plot_data_adj_long_2)
checkplot_data_adj_long_1_and_2
################################################################################
### Plots 
################################################################################

labels_study_yr <- c(
  "Combellack\n1987",
  "Jones\n2005",
  "Sinden\n2004",
  "Llewellyn\n2016",
  "Hafi\n2023",
  "Ouzman\n2025"
)

p1 <- ggplot(checkplot_data_adj_long_1_and_2, aes(x = study, y = pct, fill = component)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 6,
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
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0)
  )+
  scale_x_discrete(labels = labels_study_yr)



p1 


ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/revenue_loss_vs_expenditure_1987-2025_percentage.png",
  plot = p1,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

#################################################################################
### add in the total cost of weeds adjusted value


totals_label <- plot_data_adj_long_2 %>%
  distinct(study, total) %>%
  mutate(label = scales::label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1)(total))

totals_label


p2 <- ggplot(checkplot_data_adj_long_1_and_2, aes(x = study, y = pct, fill = component)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 6,
    fontface = "bold"
  ) +
  geom_text(
    data = totals_label,
    aes(x = study, y = 1.05, label = label),
    inherit.aes = FALSE,
    size = 6,
    color = "grey30"
  ) +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0.1))
  )+
  scale_fill_manual(values = c("Revenue loss" = "#00A9CE", "Expenditure" = "#003A5D")) +
  labs(
    x = NULL,
    y = "Share of total cost of weeds",
    fill = NULL,
    title = "Revenue loss vs expenditure share, 1987–2025",
    caption = "Value above each bar total is the inflation-adjusted costs"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0),
    plot.caption = element_text(size = 12) 
  )+
  scale_x_discrete(labels = labels_study_yr)


p2





ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/revenue_loss_vs_expenditure_1987-2025_plus_adjust_fig.png",
  plot = p2,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)






################################################################################

p2 <- ggplot(plot_data_adj_long, aes(x = study, y = value_adj, fill = component)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5,
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
  theme_minimal(base_size = 16) +
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




