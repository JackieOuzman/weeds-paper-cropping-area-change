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


library(tidyverse)
library(readxl)
library(scales)

data_path <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/copy of model/herb costs 2016 and 2025.xlsx"

raw <- read_excel(data_path, sheet = "just data")
names(raw)[1] <- "row_type"

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
      row_type == "Chemical %"    ~ "Herbicide",
      row_type == "Application %" ~ "Application"
    ),
    # combine Fallow + Knockdown into "Pre-seeding"
    herb_group = case_when(
      herb_type %in% c("Fallow", "Knockdown") ~ "Pre-seeding",
      TRUE ~ herb_type
    ),
    year      = factor(year, levels = c("2016", "2025")),
    herb_group = factor(herb_group,
                        levels = c("Pre-seeding", "Pre-emergent", "Post-emergent")),
    cost_type = factor(cost_type, levels = c("Herbicide", "Application")),
    pct       = pct * 100
  )

# sum percentages within the new grouped category
costs_pct_grouped <- costs_pct %>%
  group_by(herb_group, year, cost_type) %>%
  summarise(pct = sum(pct), .groups = "drop")

print(costs_pct_grouped)


p_pct_grouped <- ggplot(costs_pct_grouped,
                        aes(x = year, y = pct, fill = cost_type)) +
  geom_col(position = "stack", width = 0.6, colour = "white", linewidth = 0.3) +
  facet_wrap(~ herb_group, nrow = 1) +
  scale_fill_manual(values = c("Herbicide" = "#4E79A7", "Application" = "#A0CBE8")) +
  scale_x_discrete(expand = expansion(add = 0.6)) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = "Share of total herbicide cost (%)", fill = NULL,
       title = "Herbicide use has shifted toward pre-emergent control") +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    panel.spacing = unit(1.2, "lines")
  )

p_pct_grouped


ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/herbicide_shift_pre_seeding_pre_post.png",
  plot = p_pct_grouped,
  width = 8.5,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/herbicide_shift_pre_seeding_pre_post.png",
  plot = p_pct_grouped,
  width = 8.5,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
