# ws_biotypes_for_MS.R
# Manuscript version of ws_biotypes.R - Figure 1 only (Australia; world as the reference point).
# Built at final journal size for Crop & Pasture Science (full page width 16.5 cm).
#   Figure 1a : Plot_fig1_left_au_world   (Australia vs World, cumulative resistant biotypes)
#   Figure 1b : Plot_au_ALS_glycines      (Australia, ALS inhibitors vs glycines/glyphosate)
# Output    : figures/Figure1.eps (submission), figures/Figure1.png (300 dpi, for the Word draft)
# Needs     : tidyverse, patchwork (install.packages("patchwork") if missing)
# The data source is given in the Word figure caption, so no source caption is drawn on the plots.

library(tidyverse)
library(patchwork)

# ------------------------------------------------------------------
# Settings for final size (change here only)
# ------------------------------------------------------------------
fig_width_cm  <- 16.5   # CPS full page width
fig_height_cm <- 7
base_pt       <- 9      # axis text ~ 9 pt after no reduction (journal wants ~7-8 pt min)
label_mm      <- 3.2    # direct labels on the lines (ggplot text size is in mm; 3.2 mm ~ 9 pt)
line_mm       <- 1.1    # line width (mm); > 1 pt
point_mm      <- 1.6

# ------------------------------------------------------------------
# Data
# ------------------------------------------------------------------
# Totals: Australia / NZ / World biotypes over time (wide format; NZ columns are ignored)
hr_totals <- read_csv(
  "C:/Users/ouz001/Downloads/HR_biotypes_AU_NZ_World.csv",
  comment = "#"
)

# By herbicide class over time (country, herbicide_class, year, cumulative_cases)
hr_by_class <- read_csv(
  "C:/Users/ouz001/Downloads/HR_by_class_AU_NZ.csv",
  comment = "#"
)

# Common theme for both panels
theme_ms <- theme_minimal(base_size = base_pt) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.tag = element_text(face = "bold"))

# ------------------------------------------------------------------
# Figure 1a: Australia vs World
# ------------------------------------------------------------------
hr_au_world <- hr_totals %>%
  select(Year, Australia_resistant_biotypes, World_resistant_biotypes) %>%
  pivot_longer(
    cols = -Year,
    names_to = "region",
    values_to = "cumulative_cases"
  ) %>%
  mutate(region = recode(region,
                         "Australia_resistant_biotypes" = "Australia",
                         "World_resistant_biotypes" = "World"
  )) %>%
  filter(!is.na(cumulative_cases))

y_top <- max(hr_au_world$cumulative_cases, na.rm = TRUE)

Plot_fig1_left_au_world <-
  ggplot(hr_au_world, aes(x = Year, y = cumulative_cases, colour = region)) +
  geom_line(linewidth = line_mm) +
  geom_point(size = point_mm) +
  annotate("text", x = 1981, y = y_top * 0.95, label = "World",
           colour = "grey45", fontface = "bold", size = label_mm, hjust = 0) +
  annotate("text", x = 1981, y = y_top * 0.86, label = "Australia",
           colour = "#003f5c", fontface = "bold", size = label_mm, hjust = 0) +
  scale_colour_manual(values = c("Australia" = "#003f5c", "World" = "grey60")) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10)) +
  labs(x = NULL, y = "Cumulative resistant biotypes") +
  theme_ms
Plot_fig1_left_au_world

# ------------------------------------------------------------------
# Figure 1b: Australia only, ALS inhibitors + Glycines (glyphosate)
# ------------------------------------------------------------------
class_colours <- c(
  "ACCase inhibitors" = "#003f5c",
  "ALS inhibitors" = "#2f8fbf",
  "Glycines (glyphosate)" = "#7fcdff",
  "PSII (incl. Triazines)" = "#a9a9a9"
)

hr_au_2class <- hr_by_class %>%
  filter(country == "Australia",
         herbicide_class %in% c("ALS inhibitors", "Glycines (glyphosate)"))

Plot_au_ALS_glycines <-
  ggplot(hr_au_2class, aes(x = year, y = cumulative_cases, colour = herbicide_class)) +
  geom_line(linewidth = line_mm) +
  geom_point(size = point_mm) +
  annotate("text", x = 1981, y = 27, label = "ALS inhibitors",
           colour = class_colours["ALS inhibitors"], fontface = "bold", size = label_mm, hjust = 0) +
  annotate("text", x = 1981, y = 24, label = "Glycines (glyphosate)",
           colour = class_colours["Glycines (glyphosate)"], fontface = "bold", size = label_mm, hjust = 0) +
  scale_colour_manual(values = class_colours) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10)) +
  labs(x = NULL, y = "Cumulative resistant cases") +
  theme_ms
Plot_au_ALS_glycines

# ------------------------------------------------------------------
# Combine (a) and (b) and save at final size
# ------------------------------------------------------------------
Fig1 <- Plot_fig1_left_au_world + Plot_au_ALS_glycines +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
Fig1

fig_dir <- "W:/Economic impact of weeds round 2/Reports and papers/HR_MS/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# EPS for submission (journal-preferred vector format)
ggsave(file.path(fig_dir, "Figure1.eps"), Fig1, device = cairo_ps,
       width = fig_width_cm, height = fig_height_cm, units = "cm")

# 300 dpi PNG for the Word draft
ggsave(file.path(fig_dir, "Figure1.png"), Fig1,
       width = fig_width_cm, height = fig_height_cm, units = "cm",
       dpi = 300, bg = "white")

