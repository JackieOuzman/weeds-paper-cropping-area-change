# ==============================================================
# MS Figure — Panel B: Residual weeds vs contamination
# (adjusted revenue loss, 2019–20 $), 4 studies with full breakdown
# ==============================================================


library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

file_path <- "W:/Economic impact of weeds round 2/model/4.compare older model studies/timeseries_approach.xlsx"

# Study/year headers + unadjusted revenue-loss subcategories
raw_rev <- read_excel(
  file_path,
  sheet = "Table for MS GDP",
  range = "B12:J17",
  col_names = FALSE
)
studies <- as.character(raw_rev[1, 3:9])   # row 12
years   <- as.character(raw_rev[2, 3:9])   # row 13

revenue_losses_unadj <- as.numeric(raw_rev[3, 3:9])  # row 14
residual_weeds       <- as.numeric(raw_rev[4, 3:9])  # row 15
contamination        <- as.numeric(raw_rev[6, 3:9])  # row 17


plot_data_B <- data.frame(
  study                 = studies,
  year                  = years,
  revenue_losses_unadj  = revenue_losses_unadj,
  residual_weeds        = residual_weeds,
  contamination         = contamination
)

print(plot_data_B)

plot_data_B <- plot_data_B %>%
  mutate(
    residual_weeds_pct = residual_weeds / revenue_losses_unadj,
    contamination_pct  = contamination / revenue_losses_unadj
  )

print(plot_data_B)


plot_data_B_long <- plot_data_B %>%
  filter(!is.na(contamination_pct)) %>%          # keep only studies with both values
  select(study, year, residual_weeds_pct, contamination_pct) %>%
  pivot_longer(
    cols = c(residual_weeds_pct, contamination_pct),
    names_to = "component",
    values_to = "pct"
  ) %>%
  mutate(
    component = recode(component,
                       residual_weeds_pct = "Residual weeds",
                       contamination_pct  = "Contamination"
    ),
    study = factor(study, levels = unique(study))
  )

print(plot_data_B_long)

p_B <- ggplot(plot_data_B_long, aes(x = study, y = pct, fill = component)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Residual weeds" = "#003A5D", "Contamination" = "#8DC63F")) +
  labs(
    x = NULL,
    y = "Share of unadjusted revenue loss",
    fill = NULL,
    title = "Residual weeds vs contamination share of revenue loss"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

p_B


raw_ratio <- read_excel(
  file_path,
  sheet = "Table for MS GDP",
  range = "B44:J44",
  col_names = FALSE
)

ratio_used <- as.numeric(raw_ratio[1, 3:9])

plot_data_B <- plot_data_B %>%
  mutate(
    ratio_used         = ratio_used,
    residual_weeds_adj = residual_weeds * ratio_used,
    contamination_adj  = contamination * ratio_used
  )

print(plot_data_B)

plot_data_B_long_adj <- plot_data_B %>%
  filter(!is.na(contamination_adj)) %>%
  select(study, year, residual_weeds_adj, contamination_adj) %>%
  pivot_longer(
    cols = c(residual_weeds_adj, contamination_adj),
    names_to = "component",
    values_to = "value_adj"
  ) %>%
  mutate(
    component = recode(component,
                       residual_weeds_adj = "Residual weeds",
                       contamination_adj  = "Contamination"
    ),
    study = factor(study, levels = unique(study))
  )

print(plot_data_B_long_adj)

p_B_adj <- ggplot(plot_data_B_long_adj, aes(x = study, y = value_adj, fill = component)) +
  geom_col(width = 0.65) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
  scale_x_discrete(labels = c(
    "Combellack\n1981–82",
    "Jones\n1998–99",
    "Llewellyn\n2011–13",
    "Ouzman\n2019–21"
  )) +
  scale_fill_manual(values = c("Residual weeds" = "#003A5D", "Contamination" = "#8DC63F")) +
  labs(
    x = NULL,
    y = "Adjusted revenue loss\n(2019–20 $)",
    fill = NULL,
    title = "Residual weeds vs contamination"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

p_B_adj

ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/residual_weeds_vs_contamination_adjusted.png",
  plot = p_B_adj,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
