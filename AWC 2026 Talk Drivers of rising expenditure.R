

# ==============================================================
# AWC 2026 Talk — Slide 8: Drivers of rising expenditure (1)
# "Patterns in Cultivation and Herbicide Use"
#
# Purpose: Show the shift in expenditure composition from 
# cultivation to herbicides over time, supporting the point that 
# growers substituted spray for the plough as no-till systems 
# took hold (MS paragraph 88).
#
# Source data: timeseries_approach.xlsx, tab "Table for MS GDP"
# (W:\Economic impact of weeds round 2\model\4.compare older 
# model studies\timeseries_approach.xlsx)
#
# Note: only 4 of the 6 studies (Combellack, Jones, Llewellyn, 
# Ouzman) report a cultivation/herbicide expenditure breakdown — 
# Sinden and Hafi are excluded from this comparison.
#
# Output: stacked/grouped bar chart, cultivation vs herbicide 
# share of expenditure, 4 studies over time
# ==============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

file_path <- "W:/Economic impact of weeds round 2/model/4.compare older model studies/timeseries_approach.xlsx"
raw <- read_excel(file_path, sheet = "Table for MS GDP", col_names = FALSE)

# Studies and years are in row 3/4, columns 4:10
studies <- as.character(raw[3, 4:10])
years   <- as.character(raw[4, 4:10])

# Expenditure total (row 12), Cultivation (row 13), Herbicides (row 14),
# Application (row 15) in "Table for MS GDP"
exp_total_row <- as.numeric(raw[12, 4:10])
cult_row      <- as.numeric(raw[13, 4:10])
herb_row      <- as.numeric(raw[14, 4:10])
app_row       <- as.numeric(raw[15, 4:10])

drivers_data <- data.frame(
  study = studies,
  year  = years,
  total_expenditure = exp_total_row,
  cultivation = cult_row,
  # fold Application into Herbicides, matching how the MS quotes 
  # these figures (e.g. Combellack $137M herbicides + $34M 
  # application = $171M, matching MS paragraph 88)
  herbicides  = herb_row + ifelse(is.na(app_row), 0, app_row)
) %>%
  filter(!is.na(cultivation) & !is.na(herb_row)) %>%  # keeps only the 4 studies with this breakdown
  mutate(
    cultivation_pct = cultivation / total_expenditure,
    herbicides_pct  = herbicides / total_expenditure
  )

print(drivers_data)

drivers_long2 <- drivers_data %>%
  mutate(remaining_pct = 1 - cultivation_pct - herbicides_pct) %>%
  select(study, year, cultivation_pct, herbicides_pct, remaining_pct) %>%
  pivot_longer(
    cols = c(cultivation_pct, herbicides_pct, remaining_pct),
    names_to = "component",
    values_to = "pct"
  ) %>%
  mutate(
    component = recode(component,
                       cultivation_pct = "Cultivation",
                       herbicides_pct  = "Herbicides",
                       remaining_pct   = "Remaining"
    ),
    component = factor(component, levels = c("Herbicides", "Cultivation", "Remaining")),
    study = factor(study, levels = drivers_data$study)
  )

drivers_long2 <- drivers_long2 %>%
  mutate(component = factor(component, levels = c("Cultivation", "Herbicides", "Remaining")))

# compute cumulative stacking positions manually so we can place 
# small-segment labels just above their segment rather than centered
drivers_long2 <- drivers_long2 %>%
  group_by(study) %>%
  arrange(component, .by_group = TRUE) %>%
  mutate(
    ymax = cumsum(pct),
    ymin = ymax - pct,
    label_y = ifelse(pct < 0.05, ymax + 0.03, (ymin + ymax) / 2),
    label_color = ifelse(pct < 0.05, "dark", "white")
  ) %>%
  ungroup()

p5 <- ggplot(drivers_long2, aes(x = study, y = pct, fill = component, color = component)) +
  geom_col(width = 0.6, linewidth = 0.6, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = filter(drivers_long2, component != "Remaining"),
    aes(y = label_y, label = scales::percent(pct, accuracy = 1),
        color = NULL),
    color = ifelse(filter(drivers_long2, component != "Remaining")$label_color == "white", "white", "grey20"),
    size = 4,
    fontface = "bold"
  ) +
  scale_x_discrete(labels = c(
    "Combellack\n1987", "Jones\n2005", "Llewellyn\n2016", "Ouzman\n2025"
  )) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Cultivation" = "#8DC63F", "Herbicides" = "#003A5D", "Remaining" = "grey92")) +
  scale_color_manual(values = c("Cultivation" = "#8DC63F", "Herbicides" = "#003A5D", "Remaining" = "grey70")) +
  guides(color = "none") +
  labs(
    x = NULL, y = "Share of total expenditure", fill = NULL,
    title = "Cultivation vs herbicide expenditure"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.major.x = element_blank())

p5


ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/cultivation_vs_herbicide_expenditure.png",
  plot = p5,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)



################################################################################
drivers_long2 <- drivers_data %>%
  mutate(remaining_pct = 1 - cultivation_pct - herbicides_pct) %>%
  select(study, year, cultivation_pct, herbicides_pct, remaining_pct) %>%
  pivot_longer(
    cols = c(cultivation_pct, herbicides_pct, remaining_pct),
    names_to = "component",
    values_to = "pct"
  ) %>%
  mutate(
    component = recode(component,
                       cultivation_pct = "Cultivation",
                       herbicides_pct  = "Herbicides",
                       remaining_pct   = "IWM"
    ),
    component = factor(component, levels = c("Cultivation", "Herbicides", "IWM")),
    study = factor(study, levels = drivers_data$study)
  ) %>%
  group_by(study) %>%
  arrange(component, .by_group = TRUE) %>%
  mutate(
    ymax = cumsum(pct),
    ymin = ymax - pct,
    label_y = ifelse(pct < 0.05, ymax + 0.03, (ymin + ymax) / 2),
    label_color = ifelse(pct < 0.05, "dark", "white")
  ) %>%
  ungroup()

print(drivers_long2)


p5 <- ggplot(drivers_long2, aes(x = study, y = pct, fill = component, color = component)) +
  geom_col(width = 0.6, linewidth = 0.6, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = filter(drivers_long2, component != "IWM"),
    aes(y = label_y, label = scales::percent(pct, accuracy = 1),
        color = NULL),
    color = ifelse(filter(drivers_long2, component != "IWM")$label_color == "white", "white", "grey20"),
    size = 4,
    fontface = "bold"
  ) +
  scale_x_discrete(labels = c(
    "Combellack\n1981–82", "Jones\n1998–99", "Llewellyn\n2011–13", "Ouzman\n2019–21"
  )) + 
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Cultivation" = "#8DC63F", "Herbicides" = "#003A5D", "IWM" = "grey92")) +
  scale_color_manual(values = c("Cultivation" = "#8DC63F", "Herbicides" = "#003A5D", "IWM" = "grey70")) +
  guides(color = "none") +
  labs(
    x = NULL, y = "Share of total expenditure", fill = NULL,
    title = "Cultivation vs herbicide expenditure"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.major.x = element_blank())

p5


ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/cultivation_vs_herbicide_expenditure.png",
  plot = p5,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
