library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(patchwork)
library(ggplot2)

dir_out <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/"

# ── Load all four datasets ─────────────────────────────────────────────────────
wheat_long <- readRDS(paste0(dir_out, "wheat_long.rds"))
cg_long    <- readRDS(paste0(dir_out, "cg_long.rds"))
oil_long   <- readRDS(paste0(dir_out, "oil_long.rds"))
pulse_long <- readRDS(paste0(dir_out, "pulse_long.rds"))

# ── Combine ────────────────────────────────────────────────────────────────────
all_crops <- bind_rows(wheat_long, cg_long, oil_long, pulse_long)

# ── Rebuild cg_split WITHOUT state filter so all states are included ───────────
cg_split <- all_crops %>%
  filter(crop_group == "Coarse grains",
         crop %in% c("Barley", "Oats", "Triticale", "Sorghum")) %>%
  mutate(crop_group = crop)

all_crops_expanded <- bind_rows(all_crops, cg_split)

# ── Plotting helpers ───────────────────────────────────────────────────────────
crop_order_state    <- c("Triticale", "Pulses", "Oilseeds", "Sorghum", "Oats", "Barley", "Wheat")
crops_keep_expanded <- c("Wheat", "Barley", "Oats", "Sorghum", "Oilseeds", "Pulses", "Triticale")
region_order        <- c("Western", "Southern", "Northern")

# ── Build region summary ───────────────────────────────────────────────────────
region_summary <- all_crops_expanded %>%
  filter(state != "Australia",
         state != "TAS",
         crop_group %in% crops_keep_expanded,
         year >= 1980) %>%
  mutate(region = case_when(
    state %in% c("SA", "VIC")  ~ "Southern",
    state %in% c("NSW", "QLD") ~ "Northern",
    state == "WA"               ~ "Western",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(region)) %>%
  group_by(year, region, crop_group) %>%
  summarise(area_000ha = sum(area_000ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    crop_group = factor(crop_group, levels = crop_order_state),
    region     = factor(region, levels = region_order)
  )

# ── Plot ───────────────────────────────────────────────────────────────────────
ggplot(region_summary, aes(x = year, y = area_000ha, fill = crop_group)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Wheat"     = "#1a6e8a",
      "Barley"    = "#2d9e6b",
      "Oats"      = "#74c2a8",
      "Sorghum"   = "#1a3a5c",
      "Oilseeds"  = "#5b9fc9",
      "Pulses"    = "#a8c8e8",
      "Triticale" = "#8fbc8f"
    ),
    guide = guide_legend(reverse = TRUE)
  ) +
  #scale_x_continuous(breaks = c(seq(1980, 2020, by = 10), 2024)) +
  scale_x_continuous(breaks = c(seq(1980, 2020, by = 10))) +
  scale_y_continuous(breaks = function(x) pretty(x, n = 3))+
  #facet_wrap(~ region, scales = "free_y", ncol = 1) +
  facet_wrap(~ region, ncol = 2) +
  labs(
    x    = "",
    y    = "Area planted ('000 ha)",
    fill = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = c(0.58, 0.25),
    legend.justification = c(0, 0.5),
    panel.grid       = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    strip.text       = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(reverse = TRUE, ncol = 2))

# ── Save ───────────────────────────────────────────────────────────────────────



ggsave(
  filename = paste0(dir_out, "cropping_area_by_region.png"),
  plot     = last_plot(),
  width    = 250,
  height   = 160,
  units    = "mm",
  dpi      = 300
)

ggsave(
  filename = paste0(dir_out, "cropping_area_by_region_600dpi.png"),
  plot     = last_plot(),
  width    = 250,
  height   = 160,
  units    = "mm",
  dpi      = 600
)
