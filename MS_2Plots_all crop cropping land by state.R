# ==============================================================
# MS Figure 2 — Australian cropping area by crop group and region, 1980–2024
# Western region: Western Australia; Southern region: South Australia and
# Victoria combined; Northern region: New South Wales and Queensland combined.
# Data source: ABARES Agricultural Commodity Statistics 2024–25
# Inputs: wheat_long.rds, cg_long.rds, oil_long.rds, pulse_long.rds
# Output: vector PDF + 600 dpi PNG for the manuscript
# ==============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 0. Settings ----------------------------------------------

dir_out <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/"
fig_dir <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/Figure_for_ Journal_Paper/"

# Shared journal figure theme (same as the other figure scripts)
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

# ---- 1. Load and combine the four crop datasets ---------------

wheat_long <- readRDS(paste0(dir_out, "wheat_long.rds"))
cg_long    <- readRDS(paste0(dir_out, "cg_long.rds"))
oil_long   <- readRDS(paste0(dir_out, "oil_long.rds"))
pulse_long <- readRDS(paste0(dir_out, "pulse_long.rds"))

all_crops <- bind_rows(wheat_long, cg_long, oil_long, pulse_long)

# Split coarse grains into individual crops (all states, not just Australia)
cg_split <- all_crops %>%
  filter(crop_group == "Coarse grains",
         crop %in% c("Barley", "Oats", "Triticale", "Sorghum")) %>%
  mutate(crop_group = crop)

all_crops_expanded <- bind_rows(all_crops, cg_split)

# ---- 2. Prep: states to regions --------------------------------

crop_order          <- c("Triticale", "Pulses", "Oilseeds", "Sorghum", "Oats", "Barley", "Wheat")
crops_keep_expanded <- c("Wheat", "Barley", "Oats", "Sorghum", "Oilseeds", "Pulses", "Triticale")

region_lookup <- c(WA = "Western", SA = "Southern", VIC = "Southern",
                   NSW = "Northern", QLD = "Northern")

region_data <- all_crops_expanded %>%
  filter(state %in% names(region_lookup),               # drops Australia and TAS
         crop_group %in% crops_keep_expanded,
         year >= 1980) %>%
  mutate(region = region_lookup[as.character(state)]) %>%
  group_by(year, crop_group, region) %>%
  summarise(area_000ha = sum(area_000ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    crop_group = factor(crop_group, levels = crop_order),
    region     = factor(region, levels = c("Western", "Southern", "Northern"))
  )

print(region_data %>% group_by(region) %>% summarise(max_area = max(area_000ha)))

# ---- 3. Plot ---------------------------------------------------

p_fig2 <- ggplot(region_data, aes(x = year, y = area_000ha, fill = crop_group)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  facet_wrap(~ region, ncol = 2, axes = "all") +   # CHANGED: axis lines and ticks on every panel
  scale_fill_manual(
    values = c(
      "Wheat"     = "#1a6e8a",
      "Barley"    = "#2d9e6b",
      "Oats"      = "#74c2a8",
      "Sorghum"   = "#1a3a5c",
      "Oilseeds"  = "#5b9fc9",
      "Pulses"    = "#a8c8e8",
      "Triticale" = "#8fbc8f"
    )
  ) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = "Area planted ('000 ha)", fill = NULL) +    # no title: it goes in the caption
  theme_ms(base_size = 10) +
  theme(
    axis.text.x           = element_text(angle = 45, hjust = 1),
    axis.line             = element_line(colour = "black", linewidth = 0.3),
    axis.ticks            = element_line(colour = "black", linewidth = 0.3),
    strip.text            = element_text(face = "bold", size = 10),
    panel.grid            = element_blank(),
    panel.spacing         = unit(1, "lines"),
    legend.position       = "inside",
    legend.position.inside = c(0.75, 0.22)                      # sits in the empty fourth panel slot
  ) +
  guides(fill = guide_legend(ncol = 2, reverse = TRUE))

p_fig2

# ---- 4. Save ---------------------------------------------------

# Journal version: vector PDF with embedded fonts
ggsave(file.path(fig_dir, "Fig2_cropping_area_by_region.pdf"),
       plot = p_fig2, width = 160, height = 120, units = "mm", device = cairo_pdf)

# High-resolution PNG for the Word draft
ggsave(file.path(fig_dir, "Fig2_cropping_area_by_region.png"),
       plot = p_fig2, width = 160, height = 120, units = "mm", dpi = 600, bg = "white")
