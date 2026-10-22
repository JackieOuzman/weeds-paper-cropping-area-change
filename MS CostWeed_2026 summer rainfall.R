# ==============================================================
# MS Figure 3 — Share of annual rainfall falling outside the growing season
# (April–October), pre-1995 (1959–1994) vs post-1995 (1995–2025), for the
# Northern, Southern and Western grain-growing regions.
# Source data: pre-computed regional rasters prop_summer_rain_<year>.tif
# (SILO gridded climate data); this script only aggregates and plots them.
# Output: vector PDF + 600 dpi PNG for the manuscript
# ==============================================================

library(raster)
library(dplyr)
library(ggplot2)
library(scales)

# ---- 0. Settings ----------------------------------------------

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

# ---- 1. Aggregate the regional rasters ------------------------
# (Same as your original script. This is the slow part: it reads about 1,000 tif files.)

# 15 grouped AEZ regions used in the existing analysis
# (excludes Qld_Atherton, Qld_Burdekin, WA_Ord)
region_groups <- data.frame(
  region = c(
    "Qld_Central", "NSW_NE_Qld_SE", "NSW_NW_Qld_SW", "NSW_Vic_Slopes", "NSW_Central",
    "SA_Midnorth_Lower_Yorke_Eyre", "SA_Vic_Mallee", "SA_Vic_Bordertown_Wimmera", "Tas_Grain", "Vic_High_Rainfall",
    "WA_Central", "WA_Eastern", "WA_Northern", "WA_Sandplain", "WA_Mallee"
  ),
  zone = c(rep("Northern", 5), rep("Southern", 5), rep("Western", 5))
)

years_all <- 1959:2025

df_all <- do.call(rbind, lapply(region_groups$region, function(region_name) {
  region_means <- sapply(years_all, function(yr) {
    tif_path <- paste0("D:/work/Climate_analysis_nc_file_jackie/", region_name, "/prop_summer_rain_", yr, ".tif")
    if (file.exists(tif_path)) {
      r <- raster::raster(tif_path)
      raster::cellStats(r, mean, na.rm = TRUE)
    } else {
      NA
    }
  })
  data.frame(
    year = years_all,
    mean_prop = region_means,
    region = region_name,
    period = ifelse(years_all < 1995, "Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")
  )
}))

print(head(df_all))
print(sum(is.na(df_all$mean_prop)))   # sanity check for missing tif files

df_national <- df_all %>%
  left_join(region_groups, by = "region") %>%
  dplyr::group_by(year, period, zone) %>%
  dplyr::summarise(mean_prop = mean(mean_prop, na.rm = TRUE), .groups = "drop")

df_national_summary <- df_national %>%
  dplyr::group_by(period, zone) %>%
  dplyr::summarise(
    mean = mean(mean_prop, na.rm = TRUE),
    se   = sd(mean_prop, na.rm = TRUE) / sqrt(sum(!is.na(mean_prop))),
    .groups = "drop"
  ) %>%
  dplyr::mutate(period = factor(period, levels = c("Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")))

print(df_national_summary)

# Optional: save the summary so you can re-plot without re-reading the rasters
# write.csv(df_national_summary, file.path(fig_dir, "Fig3_rainfall_summary.csv"), row.names = FALSE)

# ---- 2. Plot ---------------------------------------------------

p_fig3 <- ggplot(df_national_summary, aes(x = zone, y = mean, fill = period)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.6), width = 0.15, linewidth = 0.4) +
  scale_fill_manual(values = c("Pre-1995 (1959-1994)" = "#8DC9E8",
                               "Post-1995 (1995-2025)" = "#003A5D")) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Share of annual rainfall\nfalling outside growing season", fill = NULL) +
  theme_ms(base_size = 10)

p_fig3

# ---- 3. Save ---------------------------------------------------

# Journal version: vector PDF with embedded fonts
ggsave(file.path(fig_dir, "Fig3_rainfall_outside_growing_season.pdf"),
       plot = p_fig3, width = 140, height = 90, units = "mm", device = cairo_pdf)

# High-resolution PNG for the Word draft
ggsave(file.path(fig_dir, "Fig3_rainfall_outside_growing_season.png"),
       plot = p_fig3, width = 140, height = 90, units = "mm", dpi = 600, bg = "white")
