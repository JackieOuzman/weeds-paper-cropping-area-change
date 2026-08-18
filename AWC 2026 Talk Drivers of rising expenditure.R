

# ==============================================================
# AWC 2026 Talk — Drivers of rising expenditure
# "Cultivation vs herbicide expenditure"
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
# Produces two outputs from one dataset:
#   - manuscript version (smaller font)  -> Draft Journal Paper/
#   - talk version (larger font)         -> AWC_2026/
# ==============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ---- 1. Load and prep data ----------------------------------

file_path <- "W:/Economic impact of weeds round 2/model/4.compare older model studies/timeseries_approach.xlsx"
raw <- read_excel(file_path, sheet = "Table for MS GDP", col_names = FALSE)

studies <- as.character(raw[3, 4:10])
years   <- as.character(raw[4, 4:10])

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

# Correct study-period labels (not publication years)
study_year_labels <- c(
  "Combellack\n1981–82", "Jones\n1998–99", "Llewellyn\n2011–13", "Ouzman\n2019–21"
)

drivers_long <- drivers_data %>%
  mutate(iwm_pct = 1 - cultivation_pct - herbicides_pct) %>%
  select(study, year, cultivation_pct, herbicides_pct, iwm_pct) %>%
  pivot_longer(
    cols = c(cultivation_pct, herbicides_pct, iwm_pct),
    names_to = "component",
    values_to = "pct"
  ) %>%
  mutate(
    component = recode(component,
                       cultivation_pct = "Cultivation",
                       herbicides_pct  = "Herbicides",
                       iwm_pct         = "IWM"),
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

# ---- 2. Reusable plot builder --------------------------------

build_expenditure_plot <- function(data, base_size, label_size) {
  ggplot(data, aes(x = study, y = pct, fill = component, color = component)) +
    geom_col(width = 0.6, linewidth = 0.6, position = position_stack(reverse = TRUE)) +
    geom_text(
      data = filter(data, component != "IWM"),
      aes(y = label_y, label = scales::percent(pct, accuracy = 1), color = NULL),
      color = ifelse(filter(data, component != "IWM")$label_color == "white", "white", "grey20"),
      size = label_size,
      fontface = "bold"
    ) +
    scale_x_discrete(labels = study_year_labels) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = c("Cultivation" = "#8DC63F", "Herbicides" = "#003A5D", "IWM" = "grey92")) +
    scale_color_manual(values = c("Cultivation" = "#8DC63F", "Herbicides" = "#003A5D", "IWM" = "grey70")) +
    guides(color = "none") +
    labs(x = NULL, y = "Share of total expenditure", fill = NULL,
         title = "Cultivation vs herbicide expenditure") +
    theme_minimal(base_size = base_size) +
    theme(legend.position = "top", panel.grid.major.x = element_blank())
}

# ---- 3. Manuscript version (smaller font) --------------------

p_ms <- build_expenditure_plot(drivers_long, base_size = 13, label_size = 4)
p_ms

ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/cultivation_vs_herbicide_expenditure.png",
  plot = p_ms, width = 8, height = 5.5, dpi = 300, bg = "white"
)

# ---- 4. Talk version (larger font) ----------------------------

p_talk <- build_expenditure_plot(drivers_long, base_size = 16, label_size = 6)
p_talk

ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/cultivation_vs_herbicide_expenditure.png",
  plot = p_talk, width = 8, height = 5.5, dpi = 300, bg = "white"
)

