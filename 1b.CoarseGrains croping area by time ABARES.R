
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

cg_file <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/04_ACS2024_25_CoarseGrainsTables_v1.0.0.xlsx"

raw_cg <- read_excel(cg_file, sheet = "CoarseGrains1", col_names = FALSE)

# Extract metadata rows
reporter_cg  <- as.character(raw_cg[5, ])
commodity_cg <- as.character(raw_cg[4, ])
measure_cg   <- as.character(raw_cg[7, ])
unit_cg      <- as.character(raw_cg[8, ])
frequency_cg <- as.character(raw_cg[9, ])

# Find columns matching all criteria
area_cols_cg <- which(
  str_trim(measure_cg)   == "Area" &
    str_trim(unit_cg)      == "'000 ha" &
    str_trim(reporter_cg)  == "Australia" &
    str_trim(frequency_cg) == "Fiscal Year" &
    str_trim(commodity_cg) != "Coarse grains total"
)


cg_wide <- raw_cg[12:nrow(raw_cg), c(1, area_cols_cg)]
names(cg_wide) <- c("year", commodity_cg[area_cols_cg])

cg_clean <- cg_wide %>%
  mutate(
    year = as.integer(str_extract(year, "\\d{4}")),
    across(-year, ~ suppressWarnings(as.numeric(.)))
  ) %>%
  filter(!is.na(year))

cg_long <- cg_clean %>%
  pivot_longer(
    cols      = -year,
    names_to  = "crop",
    values_to = "area_000ha"
  ) %>%
  mutate(crop_group = "Coarse grains")

print(head(cg_long, 10))
cat("\nCrops:", unique(cg_long$crop), "\n")
cat("Year range:", min(cg_long$year), "to", max(cg_long$year), "\n")


area_cols_cg <- which(
  str_trim(measure_cg)   == "Area" &
    str_trim(unit_cg)      == "'000 ha" &
    str_trim(frequency_cg) == "Fiscal Year" &
    str_trim(commodity_cg) != "Coarse grains total"
)

cg_wide <- raw_cg[12:nrow(raw_cg), c(1, area_cols_cg)]

col_names <- paste(commodity_cg[area_cols_cg], reporter_cg[area_cols_cg], sep = "||") 
col_names <- str_replace_all(col_names, "New South Wales", "NSW")
col_names <- str_replace_all(col_names, "Western Australia", "WA")
col_names <- str_replace_all(col_names, "South Australia", "SA")
col_names <- str_replace_all(col_names, "Victoria", "VIC")
col_names <- str_replace_all(col_names, "Queensland", "QLD")
col_names <- str_replace_all(col_names, "Tasmania", "TAS")

names(cg_wide) <- c("year", col_names)

# Check column names look right
cat("Column names:\n")
print(names(cg_wide))


cg_clean <- cg_wide %>%
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
  mutate(crop_group = "Coarse grains")


saveRDS(cg_clean, "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/cg_long.rds")

cat("Saved successfully\n")
