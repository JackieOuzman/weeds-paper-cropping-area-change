##IWM No HR weed sce and IWM for ws talk

library(readxl)
library(ggplot2)
           
path <- "W:/Economic impact of weeds round 2/Reports and papers/WeedSmartNZ Workshop/scenarioB summary data for NZ workshop.xlsx"

# National "Extra IWM costs" total row is row 28 (Total) minus the NR total (row 13)
nr <- read_excel(path, sheet = "IWM plots", range = "B9:P13")

hdr <- as.character(read_excel(path, sheet = "IWM plots", range = "C17:P17", col_names = FALSE)[1, ])
base <- read_excel(path, sheet = "IWM plots", range = "B25:P28", col_names = FALSE)
colnames(base) <- c("region", hdr)

# National totals rows: nr last row ("Total"), base last row ("Total")
nr_tot <- nr[nr[[1]] == "Total", ]
base_tot <- base[base$region == "Total", ]

cols_hwsc <- c("Seed milling", "Bale direct", "Chaff, lining and Chaff tramlining",
               "Chaff cart", "Narrow windrow burning")

get_val <- function(df, colname) {
  idx <- grep(paste0("^", colname), colnames(df))
  as.numeric(df[[idx[1]]])
}

practices <- c("Break crops", "Double knock", "Competitive crop seeding", "Crop topping")

extra <- sapply(practices, function(p) {
  b <- get_val(base_tot, p)
  n <- get_val(nr_tot, p)
  b - n
})

hwsc_extra <- sum(sapply(cols_hwsc, function(p) {
  b <- get_val(base_tot, p)
  n <- get_val(nr_tot, p)
  (if (length(b) == 0) 0 else b) - (if (length(n) == 0) 0 else n)
}))

df <- data.frame(
  practice = c(names(extra), "HWSC (all practices)"),
  cost = c(as.numeric(extra), hwsc_extra)
)
df <- df[order(-df$cost), ]
df$practice <- factor(df$practice, levels = rev(df$practice))

print(df)

teal <- "#00A9CE"

p <- ggplot(df, aes(x = practice, y = cost)) +
  geom_col(fill = teal, width = 0.65) +
  geom_text(aes(label = paste0("$", sprintf("%.2f", cost))),
            hjust = -0.15, size = 5, color = "#2C2C2A") +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, max(df$cost) * 1.18), expand = c(0, 0)) +
  labs(title = "IWM: extra cost due to resistance ($/ha)", x = NULL, y = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E5E5E5"),
    axis.text.y = element_text(color = "#2C2C2A", size = 14),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 15, color = "#5F5E5A", hjust = 0),
    plot.margin = margin(10, 40, 10, 10)
  )
p
ggsave("W:/Economic impact of weeds round 2/Reports and papers/WeedSmartNZ Workshop/iwm_extra_cost_chart.png", p, width = 9, height = 4.2, dpi = 200, bg = "white")
