library(readxl)
library(ggplot2)
library(scales)

path <- "W:/Economic impact of weeds round 2/Reports and papers/WeedSmartNZ Workshop/scenarioB summary data for NZ workshop.xlsx"

raw <- read_excel(path, sheet = "Sheet1", col_names = FALSE)

hdr_row <- which(apply(raw, 1, function(r) any(grepl("^Fallow", r, ignore.case = TRUE))))[1]
hdr <- trimws(as.character(raw[hdr_row, ]))

base_row <- which(apply(raw, 1, function(r) any(grepl("^Baseline$", r, ignore.case = TRUE))))
base_row <- base_row[base_row > hdr_row][1]
nr_row   <- which(apply(raw, 1, function(r) any(grepl("^NR$", r, ignore.case = TRUE))))
nr_row   <- nr_row[nr_row > hdr_row][1]

fallow_col <- which(grepl("^Fallow", hdr, ignore.case = TRUE))
knock_col  <- which(grepl("Knockdown", hdr, ignore.case = TRUE))
pre_col    <- which(grepl("Pre-emergent", hdr, ignore.case = TRUE))
post_col   <- which(grepl("Post-emergent", hdr, ignore.case = TRUE))

cols <- c(fallow_col, knock_col, pre_col, post_col)
classes <- c("Fallow", "Knockdown", "Pre-emergent", "Post-emergent")

nr_vals   <- as.numeric(raw[nr_row, cols])
base_vals <- as.numeric(raw[base_row, cols])
extra_vals <- base_vals - nr_vals

df <- data.frame(class = classes, nr = nr_vals, extra = extra_vals, total = base_vals)
df <- df[order(df$extra), ]
df$class <- factor(df$class, levels = df$class)

print(df)

plot_df <- rbind(
  data.frame(class = df$class, segment = "baseline", value = df$nr),
  data.frame(class = df$class, segment = "extra", value = df$extra)
)
plot_df$segment <- factor(plot_df$segment, levels = c("baseline", "extra"))

navy <- "#12294B"

p <- ggplot(plot_df, aes(x = class, y = value, fill = segment)) +
  geom_col(width = 0.55, colour = navy, linewidth = 0.6) +
  geom_text(data = df, aes(x = class, y = total, label = paste0("$", sprintf("%.2f", total))),
            inherit.aes = FALSE, vjust = -0.6, size = 5.5, fontface = "bold", color = navy) +
  geom_text(data = df, aes(x = class, y = extra / 2, label = paste0("+$", sprintf("%.2f", extra))),
            inherit.aes = FALSE, vjust = 0.5, size = 5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("baseline" = "white", "extra" = navy)) +
  scale_x_discrete(labels = c("Fallow" = "Fallow",
                              "Knockdown" = "Knockdown",
                              "Pre-emergent" = "Pre-\nemergent",
                              "Post-emergent" = "Post-\nemergent")) +
  scale_y_continuous(limits = c(0, max(df$total) * 1.18), expand = c(0, 0),
                     labels = dollar_format()) +
  labs(x = NULL, y = "$/ha") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0", linewidth = 0.4),
    axis.text = element_text(color = "#2C2C2A", size = 14.5),
    axis.title.y = element_text(color = "#2C2C2A", size = 15, margin = margin(r = 8)),
    plot.margin = margin(20, 25, 15, 15),
    plot.caption = element_text(size = 12, color = "#8A8A8A", hjust = 0, margin = margin(t = 10)),
    plot.background = element_rect(fill = "white", color = "#D9D9D9", linewidth = 0.6),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(caption = "Dark blue = extra cost due to resistance, per hectare (2025 values)")
p


ggsave("W:/Economic impact of weeds round 2/Reports and papers/WeedSmartNZ Workshop/herbicide_class_chart_v5.png", p, width = 8, height = 5, dpi = 200, bg = "white")
