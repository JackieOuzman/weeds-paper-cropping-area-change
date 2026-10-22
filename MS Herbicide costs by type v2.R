# ============================================================
# MS Figure 9 — Herbicide costs by use category
# Share of total national herbicide expenditure in Llewellyn et al.
# (2011–13) and the current study (2019–21), split into chemical
# and application components, for pre-seeding (fallow + knockdown),
# pre-emergent and post-emergent herbicides.
# Data:   herb costs 2016 and 2025.xlsx, sheet "just data"
#         (2016 = Llewellyn et al. 2011–13; 2025 = current study 2019–21)
# Output: vector PDF + 600 dpi PNG for the manuscript
# ============================================================

library(tidyverse)
library(readxl)
library(scales)

# ---- 0. Settings ----------------------------------------------

fig_dir   <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/Figure_for_ Journal_Paper/"   # same folder as the other figure scripts
data_path <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/copy of model/herb costs 2016 and 2025.xlsx"

# Shared journal figure theme (same as the Figure 7 and 8 scripts)
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

# ---- 1. Load and prep data ------------------------------------

raw <- read_excel(data_path, sheet = "just data")
names(raw)[1] <- "row_type"

costs_pct <- raw %>%
  filter(row_type %in% c("Chemical %", "Application %")) %>%
  select(row_type,
         `2016_Fallow`, `2016_Knockdown`, `2016_Pre-emergent`, `2016_Post-emergent`,
         `2025_Fallow`, `2025_Knockdown`, `2025_Pre-emergent`, `2025_Post-emergent`) %>%
  pivot_longer(
    cols = -row_type,
    names_to = "key",
    values_to = "pct"
  ) %>%
  mutate(
    year      = str_extract(key, "^[0-9]{4}"),
    herb_type = str_remove(key, "^[0-9]{4}_") %>% str_trim(),
    cost_type = case_when(
      row_type == "Chemical %"    ~ "Herbicide",
      row_type == "Application %" ~ "Application"
    ),
    # combine Fallow + Knockdown into "Pre-seeding"
    herb_group = case_when(
      herb_type %in% c("Fallow", "Knockdown") ~ "Pre-seeding",
      TRUE ~ herb_type
    ),
    year       = factor(year, levels = c("2016", "2025")),
    herb_group = factor(herb_group,
                        levels = c("Pre-seeding", "Pre-emergent", "Post-emergent")),
    cost_type  = factor(cost_type, levels = c("Herbicide", "Application")),
    pct        = pct * 100
  )

# sum percentages within the new grouped category
costs_pct_grouped <- costs_pct %>%
  group_by(herb_group, year, cost_type) %>%
  summarise(pct = sum(pct), .groups = "drop")

print(costs_pct_grouped)

# Label positions: Application is the bottom segment, Herbicide (chemical) sits on top
label_df <- costs_pct_grouped %>%
  group_by(herb_group, year) %>%
  mutate(app_pct = pct[cost_type == "Application"],
         label_y = ifelse(cost_type == "Application", pct / 2, app_pct + pct / 2)) %>%
  ungroup()

# ---- 2. Plot ---------------------------------------------------

p_pct_grouped_ms <- ggplot(costs_pct_grouped,
                           aes(x = year, y = pct, fill = cost_type)) +
  geom_col(position = "stack", width = 0.6, colour = "white", linewidth = 0.3) +
  geom_text(data = label_df,
            aes(x = year, y = label_y, label = sprintf("%.0f%%", pct), colour = cost_type),
            size = 3.2, fontface = "bold", inherit.aes = FALSE) +
  facet_wrap(~ herb_group, nrow = 1) +
  scale_fill_manual(values = c("Herbicide" = "#003A5D", "Application" = "#9CC3DD"),
                    labels = c("Herbicide" = "Chemical", "Application" = "Application")) +
  scale_colour_manual(values = c("Herbicide" = "white", "Application" = "grey15")) +
  guides(colour = "none") +
  scale_x_discrete(labels = c("2016" = "2011–13", "2025" = "2019–21"),
                   expand = expansion(add = 0.6)) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = "Share of total herbicide cost", fill = NULL) +   # no title: it goes in the caption
  theme_ms(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(1, "lines")
  )

p_pct_grouped_ms

# ---- 3. Save ---------------------------------------------------

# Journal version: vector PDF with embedded fonts
ggsave(file.path(fig_dir, "Fig9_herbicide_costs_by_use_category.pdf"),
       plot = p_pct_grouped_ms, width = 140, height = 90, units = "mm", device = cairo_pdf)

# High-resolution PNG for the Word draft
ggsave(file.path(fig_dir, "Fig9_herbicide_costs_by_use_category.png"),
       plot = p_pct_grouped_ms, width = 140, height = 90, units = "mm", dpi = 600, bg = "white")
