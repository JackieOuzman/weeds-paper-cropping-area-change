library(readxl)
library(dplyr)
library(tidyr)
library(stringr)


oil_file <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/16_ACS2024_25_OilseedsTables_v1.0.0.xlsx"

raw_oil <- read_excel(oil_file, sheet = "Oilseeds1", col_names = FALSE)

cat("Rows:", nrow(raw_oil), "| Cols:", ncol(raw_oil), "\n\n")


reporter_oil  <- as.character(raw_oil[5, ])
commodity_oil <- as.character(raw_oil[4, ])
measure_oil   <- as.character(raw_oil[7, ])
unit_oil      <- as.character(raw_oil[8, ])
frequency_oil <- as.character(raw_oil[9, ])

area_cols_oil <- which(
  str_trim(measure_oil)   == "Area" &
    str_trim(unit_oil)      == "'000 ha" &
    str_trim(frequency_oil) == "Fiscal Year"
)


oil_wide <- raw_oil[12:nrow(raw_oil), c(1, area_cols_oil)]

col_names_oil <- paste(commodity_oil[area_cols_oil], reporter_oil[area_cols_oil], sep = "||")
col_names_oil <- str_replace_all(col_names_oil, "New South Wales", "NSW")
col_names_oil <- str_replace_all(col_names_oil, "Western Australia", "WA")
col_names_oil <- str_replace_all(col_names_oil, "South Australia", "SA")
col_names_oil <- str_replace_all(col_names_oil, "Victoria", "VIC")
col_names_oil <- str_replace_all(col_names_oil, "Queensland", "QLD")
col_names_oil <- str_replace_all(col_names_oil, "Tasmania", "TAS")

names(oil_wide) <- c("year", col_names_oil)

oil_clean <- oil_wide %>%
  mutate(
    year = as.integer(str_extract(year, "\\d{4}")),
    across(-year, ~ suppressWarnings(as.numeric(.)))
  ) %>%
  filter(!is.na(year)) %>%
  pivot_longer(
    cols      = -year,
    names_to  = "crop_state",
    values_to = "area_000ha"
  ) %>%
  separate(crop_state, into = c("crop", "state"), sep = "\\|\\|") %>%
  mutate(crop_group = "Oilseeds")


saveRDS(oil_clean, "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/oil_long.rds")

cat("Saved successfully\n")
