# ============================================================
# Adoption curves: No-till and Paid Agronomist use by region
# Regions: Northern (NSW), Southern (SA + VIC), Western (WA)
# ============================================================

library(tidyverse)
library(readxl)

# ---- Load data ----
df <- read_excel("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/adoption_curve/Ago_curve_input_data.xlsx", sheet = 1)

# Strip whitespace from column names
names(df) <- trimws(names(df))

# ---- Recode state to region ----
df <- df %>%
  mutate(
    region = case_when(
      StateQ3 == "NSW"              ~ "Northern",
      StateQ3 %in% c("SA", "VIC")  ~ "Southern",
      StateQ3 == "WA"               ~ "Western",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(region))

df$region <- factor(df$region, levels = c("Northern", "Southern", "Western"))

# ---- Helper: cumulative % adoption by year ----
calc_adoption_curve <- function(df, year_col) {
  df %>%
    select(region, year = all_of(year_col)) %>%
    filter(year > 0) %>%
    group_by(region) %>%
    mutate(n_total = n()) %>%
    group_by(region, year) %>%
    summarise(n_adopted = n(), n_total = first(n_total), .groups = "drop") %>%
    arrange(region, year) %>%
    group_by(region) %>%
    mutate(pct = cumsum(n_adopted) / n_total * 100) %>%
    ungroup()
}

# ---- Calculate curves ----
notill <- calc_adoption_curve(df, "NoTill_YrQ20")      %>% mutate(practice = "No-till")
agro   <- calc_adoption_curve(df, "Agro_Yr_StartQ34")  %>% mutate(practice = "Paid agronomist")

curves <- bind_rows(notill, agro)

# ---- Plot ----
max(curves$year)
p <- ggplot(curves, aes(x = year, y = pct, linetype = practice)) +
  geom_line(linewidth = 1.1) +                          # thicker lines
  facet_wrap(~ region, ncol = 2) +                      # 2 columns = 2 rows
  scale_linetype_manual(
    values = c("No-till" = "dotted", "Paid agronomist" = "solid"),
    name = NULL
  ) +
  scale_x_continuous(limits = c(1980, 2015), breaks = c(1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "",
    y = "Percentage of farmers",
    #caption = "Figure 4. Percentage of farmers who have used no-till (dotted line) and use a paid agronomist\n(solid line) by region. Southern region combines SA and VIC."
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background  = element_rect(fill = "white", colour = "black"),
    strip.text        = element_text(size = 10),
    panel.grid.minor  = element_blank(),
    #legend.position   = c(0.75, 0.25),                  # inside bottom-right panel (Southern)
    legend.position   = "none",                  # inside bottom-right panel (Southern)
    legend.justification = c(0, 1),                     # anchor top-left of legend box
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key        = element_blank(),
    plot.caption      = element_text(hjust = 0, size = 9)
  )

p

ggsave("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/adoption_curve/ago_adoption_curves_regional.png", plot = p,
       width = 9, height = 4, dpi = 300, bg = "white")
