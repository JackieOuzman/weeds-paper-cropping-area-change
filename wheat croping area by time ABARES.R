# ============================================================
# 01_wheat_area.R
# Reads ABARES wheat tab, extracts area ('000 ha) columns only
# Builds a clean header from the metadata rows
# ============================================================
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

wheat_file <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/21_ACS2024_25_WheatTables_v1.0.0 (1).xlsx"

raw_wheat <- read_excel(wheat_file, sheet = "Wheat", col_names = FALSE)

cat("Rows:", nrow(raw_wheat), "| Cols:", ncol(raw_wheat), "\n\n")

# Extract metadata rows
reporter_w  <- as.character(raw_wheat[5, ])
commodity_w <- as.character(raw_wheat[4, ])
measure_w   <- as.character(raw_wheat[7, ])
unit_w      <- as.character(raw_wheat[8, ])
frequency_w <- as.character(raw_wheat[9, ])

# Find area columns with frequency filter
area_cols_w <- which(
  str_trim(measure_w)   == "Area" &
    str_trim(unit_w)      == "'000 ha" &
    str_trim(frequency_w) == "Fiscal Year"
)

cat("Columns found:", length(area_cols_w), "\n")
cat(paste(commodity_w[area_cols_w], reporter_w[area_cols_w], sep = " - ", collapse = "\n"), "\n")

# Extract data
wheat_wide <- raw_wheat[12:nrow(raw_wheat), c(1, area_cols_w)]

col_names_w <- paste(commodity_w[area_cols_w], reporter_w[area_cols_w], sep = "||")
col_names_w <- str_replace_all(col_names_w, "New South Wales",   "NSW")
col_names_w <- str_replace_all(col_names_w, "Western Australia", "WA")
col_names_w <- str_replace_all(col_names_w, "South Australia",   "SA")
col_names_w <- str_replace_all(col_names_w, "Victoria",          "VIC")
col_names_w <- str_replace_all(col_names_w, "Queensland",        "QLD")
col_names_w <- str_replace_all(col_names_w, "Tasmania",          "TAS")

names(wheat_wide) <- c("year", col_names_w)

wheat_clean <- wheat_wide %>%
  mutate(
    year = as.integer(str_extract(year, "\\d{4}")),
    across(-year, ~ suppressWarnings(as.numeric(.)))
  ) %>%
  filter(!is.na(year))

print(head(wheat_clean, 5))
cat("\nYear range:", min(wheat_clean$year), "to", max(wheat_clean$year), "\n")

wheat_long <- wheat_clean %>%
  pivot_longer(
    cols      = -year,
    names_to  = "crop_state",
    values_to = "area_000ha"
  ) %>%
  separate(crop_state, into = c("crop", "state"), sep = "\\|\\|") %>%
  mutate(crop_group = "Wheat")

cat("Crops:", unique(wheat_long$crop), "\n")
cat("States:", unique(wheat_long$state), "\n")

saveRDS(wheat_long, "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/wheat_long.rds")
cat("Saved successfully\n")






#Plot#


wheat_long %>%
  filter(state == "Australia",
         year >= 1980) %>%
  ggplot(aes(x = year, y = area_000ha)) +
  geom_area(fill = "#e6a817", alpha = 0.4) +
  geom_line(colour = "#b07d0a", linewidth = 1) +
  scale_x_continuous(breaks = seq(1974, 2024, by = 5)) +
  labs(
    title = "Australian wheat area planted",
    subtitle = "Source: ABARES Agricultural Commodity Statistics 2024-25",
    x = "Year",
    y = "Area planted ('000 ha)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )


wheat_long %>%
  filter(state != "Australia", state != "TAS",
         year >= 1980) %>%
  ggplot(aes(x = year, y = area_000ha, colour = state)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2, alpha = 0.6) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(1980, 2024, by = 5)) +
  labs(
    title = "Wheat area planted by state",
    subtitle = "Source: ABARES Agricultural Commodity Statistics 2024-25",
    x = "Year",
    y = "Area planted ('000 ha)",
    colour = "State"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


wheat_long %>%
  filter(state != "Australia", state != "TAS",
         year >= 1980) %>%
  ggplot(aes(x = year, y = area_000ha, fill = state)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(1980, 2024, by = 5)) +
  labs(
    title = "Wheat area planted by state (stacked)",
    subtitle = "Source: ABARES Agricultural Commodity Statistics 2024-25",
    x = "Year",
    y = "Area planted ('000 ha)",
    fill = "State"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
