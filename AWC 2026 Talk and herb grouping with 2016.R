# ==============================================================
# AWC 2026 Talk — Slide 7: "Why keep spending?"
#
# Purpose: Compare national revenue loss per hectare against 
# 2025 herbicide treatment costs per hectare (by category), to 
# support the claim that revenue lost to weeds is now lower 
# than the cost of a single herbicide treatment.
#
# Source data: herb costs 2016 and 2025.xlsx, tab "just data"
# (W:\Economic impact of weeds round 2\Reports and papers\
# Draft Journal Paper\copy of model\herb costs 2016 and 2025.xlsx)
#
# Revenue loss figure ($26/ha) from MS paragraph 58 (national 
# in-crop residual weed revenue loss)
#
# Output: bar chart, revenue loss vs 4 herbicide categories
# (fallow, knockdown, pre-emergent, post-emergent), 2025 $/ha
# ==============================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

herb_file <- "W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/copy of model/herb costs 2016 and 2025.xlsx"

herb_raw <- read_excel(herb_file, sheet = "just data")

comparison_data <- data.frame(
  category = c("Revenue loss", "Fallow\nherbicide", "Knockdown\nherbicide", 
               "Pre-emergent\nherbicide", "Post-emergent\nherbicide"),
  value = c(26, 28.88, 31.79, 35.41, 29.90),
  type = c("Revenue loss", "Treatment cost", "Treatment cost", "Treatment cost", "Treatment cost")
)

p4 <- ggplot(comparison_data, aes(x = reorder(category, value), y = value, fill = type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::dollar(value, accuracy = 0.01)), vjust = -0.5, 
            size = 6, fontface = "bold", color = "#003A5D") +
  scale_fill_manual(values = c("Revenue loss" = "#00A9CE", "Treatment cost" = "#003A5D")) +
  scale_y_continuous(labels = scales::dollar, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "$/ha", fill = NULL,
       title = "Revenue lost to weeds is lower than the cost of a single herbicide treatment",
       caption = "Treatment costs include chemical + application, 2025 values") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        plot.caption = element_text(size = 12, color = "grey40", hjust = 0))

p4


ggsave(
  filename = "W:/Economic impact of weeds round 2/Reports and papers/AWC_2026/revenue_loss_vs_herbicide_treatment_cost.png",
  plot = p4,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)
