# ==============================================================
# Cost of weeds MS 2026  — Fallow weed management
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
print(sum(is.na(df_all$mean_prop)))  # sanity check for missing tif files

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
df_national_summary <- df_national_summary %>%
  mutate(period = recode(as.character(period),
                         "Post-1995 (1995-2025)" = "Post-1995 (1995-2025)",
                         "Pre-1995 (1959-1994)" = "Pre-1995 (1959-1994)"
  ))

df_national_summary <- df_national_summary %>%
  mutate(period = factor(period, levels = c("Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")))

p8 <- ggplot(df_national_summary, aes(x = zone, y = mean, fill = period)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.6), width = 0.15) +
  scale_fill_manual(values = c("Pre-1995 (1959-1994)" = "#8DC9E8", "Post-1995 (1995-2025)" = "#003A5D")) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Share of annual rainfall\nfalling outside growing season",
       fill = "Period") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text = element_text(colour = "black")
        )
p8
ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/regional_rainfall_shift.png",
  plot = p8,
  width = 6,
  height = 3.5,
  dpi = 300,
  bg = "white"
)
