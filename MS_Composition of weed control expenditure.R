# ==============================================================
# MS Figure 8 — Composition of weed control expenditure
# Share of total weed control expenditure on cultivation, herbicides
# (including application) and IWM, in the four studies that report
# this breakdown (Combellack, Jones, Llewellyn, Ouzman).
# Source: timeseries_approach.xlsx, tab "Table for MS GDP"
# Output: vector PDF + 600 dpi PNG for the manuscript
# ==============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ---- 0. Settings ----------------------------------------------

fig_dir   <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/Figure_for_ Journal_Paper/"    # same folder as the Figure 7 script
file_path <- "W:/Economic impact of weeds round 2/model/4.compare older model studies/timeseries_approach.xlsx"

# Shared journal figure theme (same as the Figure 7 script)
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
  # herbicide application costs are folded into herbicides, as quoted in the text
  herbicides  = herb_row + ifelse(is.na(app_row), 0, app_row)
) %>%
  filter(!is.na(cultivation) & !is.na(herb_row)) %>%   # keeps the 4 studies with this breakdown
  mutate(
    cultivation_pct = cultivation / total_expenditure,
    herbicides_pct  = herbicides / total_expenditure
  )

print(drivers_data)

study_year_labels <- c(
  "Combellack\n1981–82", "Jones\n1998–99", "Llewellyn\n2011–13", "Ouzman\n2019–21"
)

drivers_long <- drivers_data %>%
  mutate(iwm_pct = 1 - cultivation_pct - herbicides_pct) %>%   # IWM = what is left (check against Table 4)
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
    ymax    = cumsum(pct),
    ymin    = ymax - pct,
    label_y = (ymin + ymax) / 2,
    lab     = ifelse(pct < 0.05, "", scales::percent(pct, accuracy = 1))   # segments under 5% are not labelled
  ) %>%
  ungroup()

print(drivers_long)

# ---- 2. Plot ---------------------------------------------------

p_ms <- ggplot(drivers_long, aes(x = study, y = pct, fill = component, color = component)) +
  geom_col(data = filter(drivers_long, pct >= 0.005),   # CHANGED: skip zero/near-zero segments (stops stray grey lines)
           width = 0.6, linewidth = 0.6, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = filter(drivers_long, component != "IWM"),
    aes(x = study, y = label_y, label = lab),
    colour = "white", size = 3.2, fontface = "bold", inherit.aes = FALSE
  ) +
  geom_text(
    data = filter(drivers_long, component == "IWM"),
    aes(x = study, y = label_y, label = lab),
    colour = "grey20", size = 3.2, fontface = "bold", inherit.aes = FALSE
  ) +
  scale_x_discrete(labels = study_year_labels) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values  = c("Cultivation" = "#8DC63F", "Herbicides" = "#003A5D", "IWM" = "grey92")) +
  scale_color_manual(values = c("Cultivation" = "#8DC63F", "Herbicides" = "#003A5D", "IWM" = "grey70")) +
  guides(color = "none") +
  labs(x = NULL, y = "Share of total expenditure", fill = NULL) +    # no title: it goes in the caption
  theme_ms(base_size = 10)

p_ms

# ---- 3. Save ---------------------------------------------------

# Journal version: vector PDF with embedded fonts
ggsave(file.path(fig_dir, "Fig8_composition_of_weed_control_expenditure.pdf"),
       plot = p_ms, width = 140, height = 90, units = "mm", device = cairo_pdf)

# High-resolution PNG for the Word draft
ggsave(file.path(fig_dir, "Fig8_composition_of_weed_control_expenditure.png"),
       plot = p_ms, width = 140, height = 90, units = "mm", dpi = 600, bg = "white")

