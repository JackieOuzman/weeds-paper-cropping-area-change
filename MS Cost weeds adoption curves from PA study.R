# ==============================================================
# MS Figure 4 — Cumulative adoption of no-till (dotted line) and use of a
# paid agronomist (solid line) as a percentage of farmers surveyed, by region.
# Regions: Northern (NSW), Southern (SA + VIC), Western (WA)
# Source: Llewellyn and Ouzman (2019); Ago_curve_input_data.xlsx
# Output: vector PDF + 600 dpi PNG for the manuscript
# ==============================================================

library(tidyverse)
library(readxl)

# ---- 0. Settings ----------------------------------------------

fig_dir <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/Figure_for_ Journal_Paper/"


# Shared journal figure theme (same as the other figure scripts)
theme_ms <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "Arial") +
    theme(
      legend.position    = "top",
      legend.title       = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = "black"),
      plot.margin        = margin(5, 8, 5, 5)
    )
}

# ---- 1. Load data ---------------------------------------------

df <- read_excel("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/adoption_curve/Ago_curve_input_data.xlsx", sheet = 1)

# Strip whitespace from column names
names(df) <- trimws(names(df))

# Recode state to region
df <- df %>%
  mutate(
    region = case_when(
      StateQ3 == "NSW"             ~ "Northern",
      StateQ3 %in% c("SA", "VIC")  ~ "Southern",
      StateQ3 == "WA"              ~ "Western",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(region))

df$region <- factor(df$region, levels = c("Northern", "Southern", "Western"))

# ---- 2. Cumulative % adoption by year -------------------------

calc_adoption_curve <- function(df, year_col) {
  df %>%
    dplyr::select(region, year = all_of(year_col)) %>%
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

notill <- calc_adoption_curve(df, "NoTill_YrQ20")     %>% mutate(practice = "No-till")
agro   <- calc_adoption_curve(df, "Agro_Yr_StartQ34") %>% mutate(practice = "Paid agronomist")

curves <- bind_rows(notill, agro)
print(max(curves$year))

# ---- 3. Plot ---------------------------------------------------

p_fig4 <- ggplot(curves, aes(x = year, y = pct, linetype = practice)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ region, ncol = 3, axes = "all", axis.labels = "margins") +
  scale_linetype_manual(
    values = c("No-till" = "dotted", "Paid agronomist" = "solid"),
    name = NULL
  ) +
  scale_x_continuous(limits = c(1980, 2015), breaks = c(1980, 1990, 2000, 2010)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = NULL, y = "Percentage of farmers") +        # no title or caption: it goes in the Word caption
  theme_ms(base_size = 10) +
  theme(
    strip.text       = element_text(face = "bold", size = 10),
    axis.line        = element_line(colour = "black", linewidth = 0.3),
    axis.ticks       = element_line(colour = "black", linewidth = 0.3),
    panel.spacing    = unit(1, "lines"),
    legend.position  = "bottom",
    legend.key.width = unit(1.2, "cm")                 # wide enough to see dotted vs solid
  )

p_fig4

# ---- 4. Save ---------------------------------------------------

# Journal version: vector PDF with embedded fonts
ggsave(file.path(fig_dir, "Fig4_adoption_curves_by_region.pdf"),
       plot = p_fig4, width = 160, height = 70, units = "mm", device = cairo_pdf)

# High-resolution PNG for the Word draft
ggsave(file.path(fig_dir, "Fig4_adoption_curves_by_region.png"),
       plot = p_fig4, width = 160, height = 70, units = "mm", dpi = 600, bg = "white")
