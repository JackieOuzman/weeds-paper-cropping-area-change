library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

path <- "W:/Economic impact of weeds round 2/Reports and papers/WeedSmartNZ Workshop/scenarioB summary data for NZ workshop.xlsx"

df <- read_excel(path, sheet = "iwm_stacking_2016")

plot_df <- pivot_longer(df, cols = c(resistance_pct, no_resistance_pct),
                        names_to = "category", values_to = "pct")
plot_df$category <- factor(plot_df$category,
                           levels = c("no_resistance_pct", "resistance_pct"),
                           labels = c("No resistance", "Resistance"))
plot_df$iwm_practices <- factor(plot_df$iwm_practices, levels = df$iwm_practices)

teal <- "#00A9CE"
navy <- "#12294B"

p <- ggplot(plot_df, aes(x = iwm_practices, y = pct, fill = category)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(pct, "%")),
            y = 25,
            color = "white", fontface = "bold", size = 5,
            data = . %>% filter(category == "Resistance")) +
  scale_fill_manual(values = c("No resistance" = navy, "Resistance" = teal), name = NULL) +
  scale_y_continuous(labels = percent_format(scale = 1), expand = c(0, 0)) +
  labs(x = "Number of IWM practices used on farm", y = "Percentage of growers") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#2C2C2A", size = 13),
    axis.title = element_text(color = "#2C2C2A", size = 13),
    plot.margin = margin(20, 25, 15, 15),
    plot.caption = element_text(size = 10.5, color = "#8A8A8A", hjust = 0, margin = margin(t = 10)),
    plot.background = element_rect(fill = "white", color = "#D9D9D9", linewidth = 0.6),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(caption = "White labels show the % of growers reporting herbicide resistance,\nby number of IWM practices used (2016 survey data).")

p
ggsave("W:/Economic impact of weeds round 2/Reports and papers/WeedSmartNZ Workshop/iwm_stacking_2016_chart_v2.png", p, width = 8, height = 5.7, dpi = 200, bg = "white")
