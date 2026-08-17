#Weeds workshop NZ2026

library(ggplot2)
library(readxl)

# Read directly from your source workbook
wb_path <- "W:/Economic impact of weeds round 2/Reports and papers/WeedSmartNZ Workshop/HR cost over time.xlsx"
raw <- read_excel(wb_path, sheet = "to plot")

cost_2016 <- as.numeric(raw[3, 3])   # 2016 adjusted, $61.02
cost_2025 <- as.numeric(raw[4, 3])   # 2025, $64.92 (row indices per your "to plot" sheet layout — check these against your file)

df <- data.frame(
  year = factor(c("2016", "2025"), levels = c("2016", "2025")),
  cost = c(cost_2016, cost_2025)
)

LIGHT_BLUE <- "#7EC8E3"
DARK_NAVY  <- "#0B2545"
GREY       <- "#5C6660"

p <- ggplot(df, aes(x = year, y = cost, fill = year)) +
  geom_col(width = 0.55, colour = NA, show.legend = FALSE) +
  geom_text(aes(label = paste0("$", round(cost, 0))),
            vjust = -0.6, size = 6, fontface = "bold", colour = "black") +
  scale_fill_manual(values = c(LIGHT_BLUE, DARK_NAVY)) +
  scale_y_continuous(limits = c(0, max(df$cost) * 1.2), expand = c(0, 0)) +
  labs(
    title = "Real cost of managing herbicide resistance is rising",
    y = "Cost of HR weeds ($/ha)", x = NULL,
    caption = "Inflation-adjusted to 2025 dollars."
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", colour = "black", size = 18, hjust = 0.5, margin = margin(b = 14)),
    plot.caption = element_text(colour = GREY, size = 10, hjust = 0, face = "italic", margin = margin(t = 10)),
    axis.text.x = element_text(colour = "black", size = 16, lineheight = 1.1),
    axis.text.y = element_text(colour = "black", size = 16),
    axis.title.y = element_text(colour = "black", size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    plot.background = element_rect(fill = "white", colour = "grey80", linewidth = 0.6),
    plot.margin = margin(18, 22, 14, 18)
  )
p
ggsave("W:/Economic impact of weeds round 2/Reports and papers/WeedSmartNZ Workshop/HR_cost_2016_2025.png", p, width = 7.2, height = 5.4, dpi = 200, bg = "white")
