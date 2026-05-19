
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

pulse_file <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/17_ACS2024_25_PulsesTables_v1.0.0.xlsx"


raw_pulse <- read_excel(pulse_file, sheet = "Pulses", col_names = FALSE)

cat("Rows:", nrow(raw_pulse), "| Cols:", ncol(raw_pulse), "\n\n")


reporter_pulse  <- as.character(raw_pulse[5, ])
commodity_pulse <- as.character(raw_pulse[4, ])
measure_pulse   <- as.character(raw_pulse[7, ])
unit_pulse      <- as.character(raw_pulse[8, ])
frequency_pulse <- as.character(raw_pulse[9, ])

area_cols_pulse <- which(
  str_trim(measure_pulse)   == "Area" &
    str_trim(unit_pulse)      == "'000 ha" &
    str_trim(frequency_pulse) == "Fiscal Year"
)

area_cols_pulse <- area_cols_pulse[commodity_pulse[area_cols_pulse] != "Pulses"]

pulse_wide <- raw_pulse[12:nrow(raw_pulse), c(1, area_cols_pulse)]

col_names_pulse <- paste(commodity_pulse[area_cols_pulse], reporter_pulse[area_cols_pulse], sep = "||")
col_names_pulse <- str_replace_all(col_names_pulse, "New South Wales", "NSW")
col_names_pulse <- str_replace_all(col_names_pulse, "Western Australia", "WA")
col_names_pulse <- str_replace_all(col_names_pulse, "South Australia", "SA")
col_names_pulse <- str_replace_all(col_names_pulse, "Victoria", "VIC")
col_names_pulse <- str_replace_all(col_names_pulse, "Queensland", "QLD")

names(pulse_wide) <- c("year", col_names_pulse)

pulse_clean <- pulse_wide %>%
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
  mutate(crop_group = "Pulses")

cat("Crops:", unique(pulse_clean$crop), "\n")
cat("States:", unique(pulse_clean$state), "\n")
cat("Year range:", min(pulse_clean$year), "to", max(pulse_clean$year), "\n")


saveRDS(pulse_clean, "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/pulse_long.rds")


