library(dplyr)
library(ggplot2)
library(scales)
library(readxl)

file_path <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/copy of model/yield loss and expenditure Kynetec Herb dataV2 Cotton Mods.xlsx"

journal_tab <- read_excel(file_path, sheet = "For Journal paper", col_names = FALSE)

# Row 120 = headers, Row 124 = National ($M), Row 131 = Total ($/ha)
# Columns B:P (2:16) cover Total IWM through Burn stubble
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

# Group the five HWSC component practices into one category,
# matching the MS's own "HWSC (all practices)" grouping
iwm_grouped <- iwm_full %>%
  filter(practice != "Total IWM") %>%
  mutate(
    group = case_when(
      practice %in% c("Seed milling", "Bale direct", "Chaff, lining and Chaff tramlining",
                      "Chaff cart", "Narrow windrow burning") ~ "HWSC (all practices)",
      TRUE ~ practice
    )
  ) %>%
  group_by(group) %>%
  summarise(total_M = sum(total_M), .groups = "drop") %>%
  mutate(
    total_M_clean = total_M / 1e6,
    pct = total_M / sum(total_M),
    group = factor(group, levels = group[order(total_M)])
  )

print(iwm_grouped)

p9 <- ggplot(iwm_grouped, aes(x = group, y = total_M_clean)) +
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


iwm_grouped_ha <- iwm_full %>%
  filter(practice != "Total IWM") %>%
  mutate(
    group = case_when(
      practice %in% c("Seed milling", "Bale direct", "Chaff, lining and Chaff tramlining",
                      "Chaff cart", "Narrow windrow burning") ~ "HWSC (all practices)",
      TRUE ~ practice
    )
  ) %>%
  group_by(group) %>%
  summarise(per_ha = sum(per_ha), .groups = "drop") %>%
  arrange(desc(per_ha))

print(iwm_grouped_ha)
iwm_top6 <- iwm_grouped_ha %>%
  filter(group %in% c("Break crops", "Double knock", "Crop topping",
                      "Competitive crop seeding", "Tillage prior to sowing",
                      "HWSC (all practices)")) %>%
  mutate(group = factor(group, levels = group[order(per_ha)]))

p9 <- ggplot(iwm_top6, aes(x = group, y = per_ha)) +
  geom_col(fill = "#003A5D", width = 0.6) +
  geom_text(aes(label = paste0("$", round(per_ha, 2), "/ha")),
            hjust = -0.05, size = 4, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 25), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "IWM expenditure ($/ha)",
       title = "Integrated weed management: top practices by cost per hectare") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 11))

p9

ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/iwm_top_practices_per_ha.png",
  plot = p9,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
