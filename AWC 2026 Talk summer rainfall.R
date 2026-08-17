# ==============================================================
# AWC 2026 Talk — Slide 10: Fallow weed management
# National summer rainfall proportion (pre vs post 1995)
#
# Purpose: Aggregate the already-computed regional summer 
# rainfall proportion rasters (prop_summer_rain_<year>.tif) up 
# to a single national mean, to support the point that rainfall 
# is increasingly falling outside the growing season, without 
# requiring the audience to know Australian grain region names.
#
# Source data: pre-computed per-region, per-year rasters at
# N:/work/Climate_analysis_nc_file_jackie/<region_name>/
# prop_summer_rain_<year>.tif
# (raw SILO extraction already done — this script only 
# aggregates existing output, does not reprocess .nc files)
#
# Output: national mean summer rainfall proportion, 
# pre-1995 (1959-1994) vs post-1995 (1995-2025)
# ==============================================================

library(raster)
library(dplyr)
library(ggplot2)
library(scales)

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

years_all <- 1959:2018

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
print(sum(is.na(df_all$mean_prop)))  # sanity check for missing tif files

df_national <- df_all %>%
  dplyr::group_by(year, period) %>%
  dplyr::summarise(mean_prop = mean(mean_prop, na.rm = TRUE), .groups = "drop")

df_national_summary <- df_national %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(
    mean = mean(mean_prop, na.rm = TRUE),
    se   = sd(mean_prop, na.rm = TRUE) / sqrt(sum(!is.na(mean_prop))),
    .groups = "drop"
  ) %>%
  dplyr::mutate(period = factor(period, levels = c("Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")))

print(df_national_summary)
df_national_summary <- df_national_summary %>%
  mutate(period = recode(as.character(period),
                         "Post-1995 (1995-2025)" = "Post-1995 (1995-2018)",
                         "Pre-1995 (1959-1994)" = "Pre-1995 (1959-1994)"
  ))

df_national_summary <- df_national_summary %>%
  mutate(period = factor(period, levels = c("Pre-1995 (1959-1994)", "Post-1995 (1995-2018)")))

p8 <- ggplot(df_national_summary, aes(x = period, y = mean, fill = period)) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.15) +
  geom_text(aes(label = scales::percent(mean, accuracy = 1)), vjust = -1.8, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Pre-1995 (1959-1994)" = "#8DC9E8", "Post-1995 (1995-2018)" = "#003A5D")) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = NULL, y = "Share of annual rainfall\nfalling outside growing season",
    title = "Rainfall is shifting outside the growing season",
    caption = "Error bars represent ±1 SE. Data source: SILO gridded climate dataset."
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 15),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.caption = element_text(size = 10, color = "grey40", hjust = 0)
  )

p8
ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/national_rainfall_shift.png",
  plot = p8,
  width = 8,
  height = 6.5,
  dpi = 300,
  bg = "white"
)
