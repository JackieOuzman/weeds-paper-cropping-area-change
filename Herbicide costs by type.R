# ============================================================
# Figure: Herbicide costs by type, 2016 vs 2025
# ============================================================
#
# Purpose:
#   Compare national herbicide expenditure between two time
#   points (2016 and 2025) across four herbicide use categories:
#   Fallow, Knockdown, Pre-emergent, and Post-emergent.
#   Costs are split into chemical and application components.
#
# Data:
#   Source: Ouzman et al. (2025) GRDC technical report
#   File:   herb costs 2016 and 2025.xlsx, sheet "just data"
#   Note:   2016 values are inflation-adjusted to 2025 dollars
#           using the GDP implicit price deflator (deflator = 1.1)
#
# Output:
#   Two figure variants saved as PNG (600 dpi) and PDF:
#   (1) Total costs (million AUD) - stacked by chemical vs application
#   (2) Cost per hectare (AUD/ha) - paired bars by year
#
# Author:  Jackie Ouzman
# Date:    June 2026
# ============================================================

library(tidyverse)
library(readxl)
library(scales)

# ============================================================
## Section 2 — Load data
# ============================================================
data_path <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/copy of model/herb costs 2016 and 2025.xlsx"

raw <- read_excel(data_path, sheet = "just data")

# ============================================================
#Section 3 — Wrangle
# ============================================================

# Rename the row-label column
names(raw)[1] <- "row_type"

# Keep only Chemical $ and Application $ rows
costs <- raw %>%
  filter(row_type %in% c("Chemical $", "Application $")) %>%
  select(row_type,
         `2016_Fallow`, `2016_Knockdown`, `2016_Pre-emergent`, `2016_Post-emergent`,
         `2025_Fallow`, `2025_Knockdown`, `2025_Pre-emergent`, `2025_Post-emergent`)

# Pivot to long format
costs_long <- costs %>%
  pivot_longer(
    cols = -row_type,
    names_to = "key",
    values_to = "dollars"
  ) %>%
  mutate(
    year      = str_extract(key, "^[0-9]{4}"),
    herb_type = str_remove(key, "^[0-9]{4}_") %>% str_trim(),
    cost_type = case_when(
      row_type == "Chemical $"    ~ "Herbicide",
      row_type == "Application $" ~ "Application"
    ),
    year      = factor(year, levels = c("2016", "2025")),
    herb_type = factor(herb_type,
                       levels = c("Fallow", "Knockdown", "Pre-emergent", "Post-emergent")),
    cost_type = factor(cost_type, levels = c("Herbicide", "Application")),
    dollars_M = dollars / 1e6
  )


# ============================================================
#Section 4 — Theme and colours
# ============================================================
cols_cost <- c("Herbicide"   = "#4E79A7",
               "Application" = "#A0CBE8")

theme_journal <- theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 11),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 10),
    panel.spacing    = unit(0.8, "lines")
  )


theme_journal_option2 <- theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 11),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 10),
    panel.spacing    = unit(1.5, "lines")  # increased from 0.8
  )





# ============================================================
#Section 5 — Plot (total costs, stacked bars)
# ============================================================
p <- ggplot(costs_long,
            aes(x = year, y = dollars_M, fill = cost_type)) +
  geom_col(position = "stack", width = 0.6, colour = "white", linewidth = 0.3) +
  facet_wrap(~ herb_type, nrow = 1) +
  scale_fill_manual(values = cols_cost) +
  scale_x_discrete(expand = expansion(add = .7)) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "M", accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = "Cost (million AUD)") +
  theme_journal_option2

p





# ============================================================
#Wrangle % data
# ============================================================

costs_pct <- raw %>%
  filter(row_type %in% c("Chemical %", "Application %")) %>%
  select(row_type,
         `2016_Fallow`, `2016_Knockdown`, `2016_Pre-emergent`, `2016_Post-emergent`,
         `2025_Fallow`, `2025_Knockdown`, `2025_Pre-emergent`, `2025_Post-emergent`) %>%
  pivot_longer(
    cols = -row_type,
    names_to = "key",
    values_to = "pct"
  ) %>%
  mutate(
    year      = str_extract(key, "^[0-9]{4}"),
    herb_type = str_remove(key, "^[0-9]{4}_") %>% str_trim(),
    cost_type = case_when(
      row_type == "Chemical %"     ~ "Herbicide",
      row_type == "Application %"  ~ "Application"
    ),
    year      = factor(year, levels = c("2016", "2025")),
    herb_type = factor(herb_type,
                       levels = c("Fallow", "Knockdown", "Pre-emergent", "Post-emergent")),
    cost_type = factor(cost_type, levels = c("Herbicide", "Application")),
    pct       = pct * 100   # convert to percentage
  )
glimpse(costs_pct) 


# ============================================================
#Plot % version
# ============================================================

p_pct <- ggplot(costs_pct,
                aes(x = year, y = pct, fill = cost_type)) +
  geom_col(position = "stack", width = 0.6, colour = "white", linewidth = 0.3) +
  facet_wrap(~ herb_type, nrow = 1) +
  scale_fill_manual(values = cols_cost) +
  scale_x_discrete(expand = expansion(add = 0.6)) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = "Share of total herbicide cost (%)") +
  theme_journal_option2

p_pct



ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/copy of model/herbicide_costs_pct_2016_2025_600dpi.png",
  plot     = p_pct,
  width    = 180,
  height   = 100,
  units    = "mm",
  dpi      = 600
)

ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/copy of model/herbicide_costs_pct_2016_2025.pdf",
  plot     = p_pct,
  width    = 180,
  height   = 100,
  units    = "mm"
)
