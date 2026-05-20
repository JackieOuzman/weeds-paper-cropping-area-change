

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(patchwork)


# Load all four datasets
wheat_long <- readRDS("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/wheat_long.rds")
cg_long    <- readRDS("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/cg_long.rds")
oil_long   <- readRDS("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/oil_long.rds")
pulse_long <- readRDS("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/pulse_long.rds")

# Combine
all_crops <- bind_rows(wheat_long, cg_long, oil_long, pulse_long)

# Check
cat("Crop groups:", unique(all_crops$crop_group), "\n")
cat("Crops:", unique(all_crops$crop), "\n")
cat("States:", unique(all_crops$state), "\n")
cat("Year range:", min(all_crops$year), "to", max(all_crops$year), "\n")
cat("Rows:", nrow(all_crops), "\n")


all_crops %>%
  group_by(crop_group, crop) %>%
  summarise(
    first_year = min(year[!is.na(area_000ha)]),
    last_year  = max(year[!is.na(area_000ha)]),
    .groups = "drop"
  ) %>%
  print(n = Inf)

crops_keep <- c("Wheat", "Barley", "Oats", "Triticale", "Canola", 
                "Lupins", "Field peas", "Chickpeas", "Lentils", 
                "Faba beans", "Sorghum")

all_crops %>%
  filter(state == "Australia", crop %in% crops_keep, year >= 1980) %>%
  group_by(year, crop_group) %>%
  summarise(area_000ha = sum(area_000ha, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = area_000ha, fill = crop_group)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = c(
    "Wheat"         = "#1a6e8a",
    "Coarse grains" = "#2d9e6b",
    "Oilseeds"      = "#74c2a8",
    "Pulses"        = "#5b9fc9"
  )) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 5)) +
  labs(
    title    = "Australian cropping area by crop group",
    subtitle = "Source: ABARES Agricultural Commodity Statistics 2024-25",
    x        = "Year",
    y        = "Area planted ('000 ha)",
    fill     = NULL,
    caption  = "Coarse grains: Barley, Oats, Triticale, Sorghum | Oilseeds: Canola | Pulses: Lupins, Field peas, Chickpeas, Lentils, Faba beans"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, hjust = 0, colour = "grey40")
  )




all_crops %>%
  filter(state != "Australia", crop %in% crops_keep, year >= 1980) %>%
  group_by(year, state, crop_group) %>%
  summarise(area_000ha = sum(area_000ha, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = area_000ha, fill = crop_group)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = c(
    "Wheat"         = "#1a6e8a",
    "Coarse grains" = "#2d9e6b",
    "Oilseeds"      = "#74c2a8",
    "Pulses"        = "#5b9fc9"
  )) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 10)) +
  facet_wrap(~state, scales = "free_y", ncol = 2) +
  labs(
    title    = "Australian cropping area by crop group and state",
    subtitle = "Source: ABARES Agricultural Commodity Statistics 2024-25",
    x        = "Year",
    y        = "Area planted ('000 ha)",
    fill     = NULL,
    caption  = "Coarse grains: Barley, Oats, Triticale, Sorghum | Oilseeds: Canola | Pulses: Lupins, Field peas, Chickpeas, Lentils, Faba beans"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    plot.caption     = element_text(size = 8, hjust = 0, colour = "grey40")
  )



all_crops %>%
  filter(state != "Australia", crop %in% crops_keep, year >= 1980) %>%
  group_by(crop_group, state) %>%
  summarise(
    n_years    = n_distinct(year),
    n_missing  = sum(is.na(area_000ha)),
    first_data = min(year[!is.na(area_000ha)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(crop_group, state) %>%
  print(n = Inf)


### cleaner version by state


state_order <- c("WA", "NSW", "SA", "VIC", "QLD")

all_crops %>%
  filter(state %in% state_order, crop %in% crops_keep, year >= 1980) %>%
  mutate(state = factor(state, levels = state_order)) %>%
  group_by(year, state, crop_group) %>%
  summarise(area_000ha = sum(area_000ha, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = area_000ha, fill = crop_group)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = c(
    "Wheat"         = "#1a6e8a",
    "Coarse grains" = "#2d9e6b",
    "Oilseeds"      = "#74c2a8",
    "Pulses"        = "#5b9fc9"
  )) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 10)) +
  #facet_wrap(~state, scales = "free_y", ncol = 2) +
  facet_wrap(~state,  ncol = 2) +
  labs(
    title    = "Australian cropping area by crop group and state",
    subtitle = "Source: ABARES Agricultural Commodity Statistics 2024-25",
    x        = "Year",
    y        = "Area planted ('000 ha)",
    fill     = NULL,
    caption  = "Coarse grains: Barley, Oats, Triticale, Sorghum | Oilseeds: Canola | Pulses: Lupins, Field peas, Chickpeas, Lentils, Faba beans"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    plot.caption     = element_text(size = 8, hjust = 0, colour = "grey40")
  )


