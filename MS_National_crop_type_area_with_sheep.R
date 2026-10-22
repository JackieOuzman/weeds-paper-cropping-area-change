# ==============================================================
# MS Figure 1 — Australian cropping area by crop, 1990–2024, and
# sheep stocking density on cropping farms, 1990–2021
# Crop area data: ABARES Statistics 2024–25
# Sheep data:     ABARES Farm Data Portal, Historical National Estimates (2025)
# Inputs: all_crops_expanded_for_plot.csv and sheep_crop_wide_for_plot.csv
#         (the sheep file is written by ABARES_import_sheep.R)
# Output: vector PDF + 600 dpi PNG for the manuscript
# ==============================================================

library(ggplot2)
library(dplyr)
library(tidyr)

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

# ---- 1. Load data ---------------------------------------------

all_crops_expanded <- read.csv(paste0(dir_out, "all_crops_expanded_for_plot.csv"))
sheep              <- read.csv(paste0(dir_out, "sheep_crop_wide_for_plot.csv"))

# ---- 2. Prep crop area data -----------------------------------

crops_keep_expanded <- c("Wheat", "Barley", "Oats", "Triticale", "Sorghum", "Oilseeds", "Pulses")
crop_order          <- c("Triticale", "Pulses", "Oilseeds", "Sorghum", "Oats", "Barley", "Wheat")

crop_summary <- all_crops_expanded %>%
  filter(state == "Australia",
         crop_group %in% crops_keep_expanded,
         year >= 1990) %>%
  group_by(year, crop_group) %>%
  summarise(area_000ha = sum(area_000ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(crop_group = factor(crop_group, levels = crop_order))

# Total cropped area per year (for scaling the sheep axis)
total_area <- crop_summary %>%
  group_by(year) %>%
  summarise(total_area = sum(area_000ha), .groups = "drop")

# ---- 3. Prep sheep data ---------------------------------------

sheep_filt <- sheep %>%
  filter(Year >= 1990 & Year <= 2021) %>%
  rename(year = Year)

# Scaling factor for the secondary axis
# (maps the sheep per cropped hectare range onto the crop area range)
area_max     <- max(total_area$total_area, na.rm = TRUE)
sheep_max    <- max(sheep_filt$sheep_per_crop_ha, na.rm = TRUE)
scale_factor <- area_max / sheep_max

# ---- 4. Plot --------------------------------------------------

plot1 <- ggplot() +
  geom_area(
    data = crop_summary,
    aes(x = year, y = area_000ha, fill = crop_group),
    alpha = 0.85, colour = "white", linewidth = 0.2
  ) +
  geom_line(
    data = sheep_filt,
    aes(x = year, y = sheep_per_crop_ha * scale_factor),
    colour = "grey55", linewidth = 1, linetype = "solid"
  ) +
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
    guide = guide_legend(reverse = FALSE)
  ) +
  scale_x_continuous(breaks = c(seq(1990, 2020, by = 5), 2024)) +
  scale_y_continuous(
    name = "Area planted ('000 ha)",
    labels = scales::comma,   # CHANGED
    sec.axis = sec_axis(
      transform = ~ . / scale_factor,
      name      = "Sheep per cropped hectare"
    )
  ) +
  labs(x = NULL, fill = NULL) +                       # no title: it goes in the caption
  theme_ms(base_size = 10) +
  theme(
    axis.text.x          = element_text(angle = 45, hjust = 1),
    axis.line            = element_line(colour = "black", linewidth = 0.3),
    axis.ticks           = element_line(colour = "black", linewidth = 0.3),
    legend.position      = "bottom",
    legend.justification = "left",
    panel.grid           = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE))

plot1

# ---- 5. Save --------------------------------------------------

# Journal version: vector PDF with embedded fonts
ggsave(file.path(fig_dir, "Fig1_cropping_area_sheep.pdf"),
       plot = plot1, width = 160, height = 100, units = "mm", device = cairo_pdf)

# High-resolution PNG for the Word draft
ggsave(file.path(fig_dir, "Fig1_cropping_area_sheep.png"),
       plot = plot1, width = 160, height = 100, units = "mm", dpi = 600, bg = "white")

