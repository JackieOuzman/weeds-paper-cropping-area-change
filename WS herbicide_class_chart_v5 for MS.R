# herbicide costs by type v2 for MS
# Manuscript version of WS_herbicide_class_chart_v5.R (Figure 3: herbicide expenditure per hectare by class).
# Changes vs v5: journal-size text (9 pt base), no in-plot "(2025 values)" note (caption is in the Word
# document: 2021 prices, including application costs), saves EPS + 300 dpi PNG to HR_MS/figures.
# Data: Scenario B summary workbook (Baseline and NR rows by herbicide class, $/ha incl. application cost).

library(readxl)
library(ggplot2)
library(scales)

fig_dir <- "W:/Economic impact of weeds round 2/Reports and papers/HR_MS/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

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
  geom_col(width = 0.55, colour = navy, linewidth = 0.4) +
  geom_text(data = df, aes(x = class, y = total, label = paste0("$", sprintf("%.2f", total))),
            inherit.aes = FALSE, vjust = -0.6, size = 3.2, fontface = "bold", color = navy) +
  geom_text(data = df, aes(x = class, y = extra / 2, label = paste0("+$", sprintf("%.2f", extra))),
            inherit.aes = FALSE, vjust = 0.5, size = 3, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("baseline" = "white", "extra" = navy)) +
  scale_x_discrete(labels = c("Fallow" = "Fallow",
                              "Knockdown" = "Knockdown",
                              "Pre-emergent" = "Pre-\nemergent",
                              "Post-emergent" = "Post-\nemergent")) +
  scale_y_continuous(limits = c(0, max(df$total) * 1.18), expand = c(0, 0),
                     labels = dollar_format()) +
  labs(x = NULL, y = "$/ha") +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0", linewidth = 0.3),
    axis.text = element_text(color = "#2C2C2A"),
    axis.title.y = element_text(color = "#2C2C2A", margin = margin(r = 6)),
    plot.margin = margin(8, 10, 6, 6),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
p


# EPS for submission (journal-preferred vector format) and 300 dpi PNG for the Word draft
ggsave(file.path(fig_dir, "Figure3.eps"), p, device = cairo_ps, width = 12, height = 7.5, units = "cm")
ggsave(file.path(fig_dir, "Figure3.png"), p, width = 12, height = 7.5, units = "cm", dpi = 300, bg = "white")

