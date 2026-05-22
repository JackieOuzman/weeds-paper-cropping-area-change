#install.packages("read.abares")
library(read.abares)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

#################################################################################
## National prop_area_cropped 
#################################################################################

# Pull full national historical estimates dataset
nat_est <- read_historical_national_estimates()

# Inspect what's in there first - check unique industries and variables
unique(nat_est$Industry)
unique(nat_est$Variable)  # or Variable - check snake_case naming


cropping_area <- nat_est  %>% 
  filter(Industry == "Cropping")


cropping_area_subset <- cropping_area  %>% 
  filter(Variable %in% c("Total area cropped (ha)", "Area operated at 30 June (ha)")
  )


cropping_wide <- cropping_area_subset %>%
  select(-RSE) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(prop_area_cropped = `Total area cropped (ha)` / `Area operated at 30 June (ha)`)


#################################################################################
## States prop_area_cropped 
#################################################################################


state_est <- read_historical_state_estimates()

state_cropping_wide <- state_est %>%
  filter(
    Industry == "Cropping",
    Variable %in% c("Total area cropped (ha)", "Area operated at 30 June (ha)")
  ) %>%
  select(-RSE) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(prop_area_cropped = `Total area cropped (ha)` / `Area operated at 30 June (ha)`)


### keep 2 time points 1990 and 2021 and 
unique(state_cropping_wide$Year)

state_cropping_wide_90_21 <- state_cropping_wide %>%
  filter(Year %in% c(1990, 2021))


state_cropping_pct_change <- state_cropping_wide_90_21 %>%
  group_by(State) %>%
  arrange(Year) %>%
  summarise(pct_change_prop_area_cropped = round((last(prop_area_cropped) - first(prop_area_cropped)) / first(prop_area_cropped) * 100, 0))


read.abares_options()


#################################################################################
## Try again with different data set and filtering by crops States prop_area_cropped 
#################################################################################
## nope this wont do the job.
broadacre <- read_abs_broadacre_data()
glimpse(broadacre)
distinct(broadacre,commodity)

#################################################################################
## Try again with different data set crop receipt vs total receipt
#################################################################################

#grep("receipt|cash", unique(nat_est$Variable), value = TRUE, ignore.case = TRUE)

####  
receipts_subset <- nat_est %>%
  filter(
    Industry == "Cropping",
    Variable %in% c("Total cash receipts ($)", "Total crop gross receipts ($)")
  )

receipts_wide <- receipts_subset %>%
  select(-RSE) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(prop_crop_receipts = `Total crop gross receipts ($)` / `Total cash receipts ($)`)

glimpse(receipts_wide)

receipts_wide %>%
  summarise(
    mean_prop = round(mean(prop_crop_receipts, na.rm = TRUE), 2),
    min_prop = round(min(prop_crop_receipts, na.rm = TRUE), 2),
    max_prop = round(max(prop_crop_receipts, na.rm = TRUE), 2)
  )

ggplot(receipts_wide, aes(x = Year, y = prop_crop_receipts)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Proportion of cash receipts from crops - Cropping industry",
    x = "Year",
    y = "Proportion"
  ) +
  theme_bw()

#smooth version 

ggplot(receipts_wide, aes(x = Year, y = prop_crop_receipts)) +
  #geom_point() +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(
    title = "Proportion of cash receipts from crops - Cropping industry",
    x = "Year",
    y = "Proportion"
  ) +
  theme_bw()


#################################################################################
## Try again with different data set crop receipt vs total receipt per state
#################################################################################

receipts_state <- state_est %>%
  filter(
    Industry == "Cropping",
    Variable %in% c("Total cash receipts ($)", "Total crop gross receipts ($)"),
    Year %in% c(1990, 2021)
  ) %>%
  select(-RSE) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(prop_crop_receipts = `Total crop gross receipts ($)` / `Total cash receipts ($)`)

glimpse(receipts_state)

receipts_state_pct_change <- receipts_state %>%
  group_by(State) %>%
  arrange(Year) %>%
  summarise(pct_change_prop_crop_receipts = round((last(prop_crop_receipts) - first(prop_crop_receipts)) / first(prop_crop_receipts) * 100, 0))

print(receipts_state_pct_change)
