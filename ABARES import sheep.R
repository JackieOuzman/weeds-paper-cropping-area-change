#install.packages("read.abares")
library(read.abares)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

#################################################################################
## National sheep data 
#################################################################################

# Pull full national historical estimates dataset
nat_est <- read_historical_national_estimates()

# Inspect what's in there first - check unique industries and variables
unique(nat_est$Industry)
unique(nat_est$Variable)  # or Variable - check snake_case naming

grep("sheep", unique(nat_est$Variable), value = TRUE, ignore.case = TRUE)
grep("livestock", unique(nat_est$Variable), value = TRUE, ignore.case = TRUE)



sheep_crop <- nat_est %>%
  filter(
    Industry == "Cropping",
    Variable %in% c("Sheep flock at 30 June (no.)", "Total area cropped (ha)")
  )

sheep_crop_wide <- sheep_crop %>%
  select(-RSE) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(sheep_per_crop_ha = `Sheep flock at 30 June (no.)` / `Total area cropped (ha)`)

glimpse(sheep_crop_wide)

ggplot(sheep_crop_wide, aes(x = Year, y = sheep_per_crop_ha)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Sheep per cropped hectare - Cropping industry",
    x = "Year",
    y = "Sheep per cropped ha"
  ) +
  theme_bw()

### Percentage change
sheep_crop_wide_1990_2021 <- sheep_crop_wide %>%
  filter(Year %in% c(1990, 2021))

sheep_crop_wide %>%
  filter(Year %in% c(1990, 2021)) %>%
  summarise(pct_change_sheep_per_crop_ha = round((last(sheep_per_crop_ha) - first(sheep_per_crop_ha)) / first(sheep_per_crop_ha) * 100, 0))

### better quality plot and fixed time points
sheep_crop_wide %>%
  filter(Year >= 1990 & Year <= 2021) %>%
  ggplot(aes(x = Year, y = sheep_per_crop_ha)) +
  geom_line(linewidth = 0.8, colour = "black") +
  #geom_point(size = 2, colour = "black") +
  scale_x_continuous(breaks = seq(1990, 2021, by = 5)) +
  labs(
    x = "Year",
    y = "Sheep per cropped hectare",
    caption = "Source: ABARES Farm Data Portal — Historical National Estimates (2025)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, colour = "black"),
    plot.caption = element_text(size = 8, hjust = 0)
  )



write.csv(
  sheep_crop_wide,
  "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/sheep_crop_wide_for_plot.csv",
  row.names = FALSE
)

