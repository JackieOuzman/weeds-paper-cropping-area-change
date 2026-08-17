library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

adopt_file <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/adoption_curve/adoption_curves_R.xlsx"

adopt_raw <- read_excel(adopt_file, sheet = "Q14_Q15_NoTill")

print(adopt_raw, n = 10)
print(nrow(adopt_raw))
# Build cumulative national adoption curve: for each year, 
# % of all surveyed farmers who had adopted no-till by that year 
# (non-adopters remain in the denominator throughout)

year_range <- seq(min(adopt_raw$Q15_year_first_try_no_till, na.rm = TRUE),
                  max(adopt_raw$Q15_year_first_try_no_till, na.rm = TRUE))

total_farmers <- nrow(adopt_raw)

adoption_curve <- data.frame(year = year_range) %>%
  rowwise() %>%
  mutate(
    n_adopted = sum(adopt_raw$Q14_ever_used_no_till_for_cropping == 1 &
                      adopt_raw$Q15_year_first_try_no_till <= year, na.rm = TRUE),
    pct_adopted = n_adopted / total_farmers
  ) %>%
  ungroup()

print(tail(adoption_curve, 10))

adoption_curve_trimmed <- adoption_curve %>%
  filter(year >= 1980)

p6 <- ggplot(adoption_curve_trimmed, aes(x = year, y = pct_adopted)) +
  geom_line(color = "#003A5D", linewidth = 2.5) +
  #geom_point(color = "#003A5D", size = 1.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  labs(
    x = NULL, y = "Farmers who have adopted no-till",
    title = "No-till adoption, national"
  ) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank())

p6

ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/no_till_adoption_national.png",
  plot = p6,
  width = 6,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
