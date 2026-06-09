library(ggplot2)
library(dplyr)
library(tidyr)

# ── Load data ──────────────────────────────────────────────────────────────────
dir_out <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/"

all_crops_expanded <- read.csv(paste0(dir_out, "all_crops_expanded_for_plot.csv"))
sheep              <- read.csv(paste0(dir_out, "sheep_crop_wide_for_plot.csv"))

# ── Prep crop area data ────────────────────────────────────────────────────────
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

# ── Prep sheep data ────────────────────────────────────────────────────────────
sheep_filt <- sheep %>%
  filter(Year >= 1990 & Year <= 2021) %>%
  rename(year = Year)

# ── Scaling factor for secondary axis ─────────────────────────────────────────
# Maps sheep_per_crop_ha range onto the crop area range
area_max  <- max(total_area$total_area, na.rm = TRUE)
sheep_max <- max(sheep_filt$sheep_per_crop_ha, na.rm = TRUE)
scale_factor <- area_max / sheep_max

# ── Plot ───────────────────────────────────────────────────────────────────────
plot1 <- ggplot() +
  geom_area(
    data = crop_summary,
    aes(x = year, y = area_000ha, fill = crop_group),
    alpha = 0.85, colour = "white", linewidth = 0.2
  ) +
  geom_line(
    data = sheep_filt,
    aes(x = year, y = sheep_per_crop_ha * scale_factor),
    #colour = "grey40", linewidth = 1.1, linetype = "dashed"
    colour = "grey", linewidth = 1.8, linetype = "solid"
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
    sec.axis = sec_axis(
      transform = ~ . / scale_factor,
      name      = "Sheep per cropped hectare"
    )
  ) +
  labs(
    x    = "",
    fill = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(colour = "black"),
    legend.position    = "bottom",
    panel.grid         = element_blank(),
    legend.justification = "left"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE))
plot1



# ── Save ───────────────────────────────────────────────────────────────────────
ggsave(
  filename    = paste0(dir_out, "cropping_area_sheep_dual_axis.png"),
  plot        = last_plot(),
  width       = 220,
  height      = 120,
  units       = "mm",
  dpi         = 300
)


ggsave(
  filename    = paste0(dir_out, "cropping_area_sheep_dual_axis_600dpi.png"),
  plot        = last_plot(),
  width       = 220,
  height      = 120,
  units       = "mm",
  dpi         = 600
)
