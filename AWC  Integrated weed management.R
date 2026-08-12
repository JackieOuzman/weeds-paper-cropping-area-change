# ==============================================================
# AWC 2026 Talk — Slide (new, after 8): Integrated weed management
#
# Purpose: Show the breakdown of IWM expenditure by practice, to 
# pay off the "grey gap" flagged on the cultivation vs herbicide 
# expenditure slide. IWM is the second-largest expenditure 
# category nationally ($1,072M, 29% of total weed control spend).
#
# Source data: yield loss and expenditure Kynetec Herb dataV2 
# Cotton Mods.xlsx, sheet "For Journal paper"
# (W:\Economic impact of weeds round 2\Reports and papers\
# Draft Journal Paper\copy of model\yield loss and expenditure 
# Kynetec Herb dataV2 Cotton Mods.xlsx)
# Row 120 = headers, Row 124 = National totals ($M), 
# Row 131 = National totals ($/ha)
#
# Output: horizontal bar chart, IWM expenditure by practice ($M)
# ==============================================================

library(dplyr)
library(ggplot2)
library(scales)
library(readxl)

file_path <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/copy of model/yield loss and expenditure Kynetec Herb dataV2 Cotton Mods.xlsx"

journal_tab <- read_excel(file_path, sheet = "For Journal paper", col_names = FALSE)

# Row 120 = headers, Row 124 = National ($M), Row 131 = Total ($/ha)
# Columns B:O (2:15) cover Total IWM through Burn stubble
headers <- as.character(journal_tab[120, 2:15])
national_M <- as.numeric(journal_tab[124, 2:15])
national_per_ha <- as.numeric(journal_tab[131, 2:15])

iwm_full <- data.frame(
  practice = headers,
  total_M = national_M,
  per_ha = national_per_ha
)

print(iwm_full)

headers <- as.character(journal_tab[120, 2:16])
national_M <- as.numeric(journal_tab[124, 2:16])
national_per_ha <- as.numeric(journal_tab[131, 2:16])

iwm_full <- data.frame(
  practice = headers,
  total_M = national_M,
  per_ha = national_per_ha
) %>%
  filter(!is.na(practice))  # drops the blank leading column

print(iwm_full)

iwm_plot_data <- iwm_full %>%
  filter(practice != "Total IWM") %>%
  mutate(
    total_M_clean = total_M / 1e6,
    pct = total_M / sum(total_M),
    practice = factor(practice, levels = practice[order(total_M)])
  )

p9 <- ggplot(iwm_plot_data, aes(x = practice, y = total_M_clean)) +
  geom_col(fill = "#003A5D", width = 0.6) +
  geom_text(aes(label = paste0("$", round(total_M_clean), "M (", scales::percent(pct, accuracy = 1), ")")),
            hjust = -0.05, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 500), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "IWM expenditure ($M)",
       title = "Integrated weed management: $1,072M, 29% of total expenditure") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 10))

p9
