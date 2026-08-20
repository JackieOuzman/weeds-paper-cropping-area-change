library(readxl)
library(ggplot2)
library(scales)

path <- "W:/Economic impact of weeds round 2/Reports and papers/WeedSmartNZ Workshop/scenarioB summary data for NZ workshop.xlsx"

raw <- read_excel(path, sheet = "Sheet1", col_names = FALSE)

hdr_row <- which(apply(raw, 1, function(r) any(grepl("^Total IWM", r, ignore.case = TRUE))))[1]
hdr <- trimws(as.character(raw[hdr_row, ]))

base_row <- which(apply(raw, 1, function(r) any(grepl("^Baseline$", r, ignore.case = TRUE))))
base_row <- base_row[base_row > hdr_row][1]
nr_row   <- which(apply(raw, 1, function(r) any(grepl("^NR$", r, ignore.case = TRUE))))
nr_row   <- nr_row[nr_row > hdr_row][1]

bc_col   <- which(grepl("Break crops", hdr, ignore.case = TRUE))
dk_col   <- which(grepl("Double knock", hdr, ignore.case = TRUE))
ccs_col  <- which(grepl("Competitive crop seeding", hdr, ignore.case = TRUE))
ct_col   <- which(grepl("Crop topping", hdr, ignore.case = TRUE))
hwsc_col <- which(grepl("HWSC", hdr, ignore.case = TRUE))

cols <- c(bc_col, dk_col, ccs_col, ct_col, hwsc_col)
practices <- c("Break crops", "Double knock", "Competitive\ncrop seeding", "Crop topping", "HWSC")

nr_vals   <- as.numeric(raw[nr_row, cols])
base_vals <- as.numeric(raw[base_row, cols])
extra_vals <- base_vals - nr_vals

df <- data.frame(practice = practices, nr = nr_vals, extra = extra_vals, total = base_vals)
df <- df[order(-df$total), ]                      # descending by total (baseline) cost
df$practice <- factor(df$practice, levels = df$practice)

plot_df <- rbind(
  data.frame(practice = df$practice, segment = "baseline", value = df$nr),
  data.frame(practice = df$practice, segment = "extra", value = df$extra)
)
plot_df$segment <- factor(plot_df$segment, levels = c("baseline", "extra"))

navy <- "#12294B"

p <- ggplot(plot_df, aes(x = practice, y = value, fill = segment)) +
  geom_col(width = 0.55, colour = navy, linewidth = 0.6) +
  geom_text(data = df, aes(x = practice, y = total, label = paste0("$", sprintf("%.2f", total))),
            inherit.aes = FALSE, vjust = -0.6, size = 5, fontface = "bold", color = navy) +
  geom_text(data = df, aes(x = practice, y = extra / 2, label = paste0("+$", sprintf("%.2f", extra))),
            inherit.aes = FALSE, vjust = 0.5, size = 4.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("baseline" = "white", "extra" = navy)) +
  scale_x_discrete(labels = c("Break crops" = "Break\ncrops",
                              "Double knock" = "Double\nknock",
                              "Crop topping" = "Crop\ntopping",
                              "Competitive crop seeding" = "Competitive\ncrop seeding",
                              "HWSC" = "HWSC")) +
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
    axis.title.y = element_text(color = "#2C2C2A", size = 14.5, margin = margin(r = 8)),
    plot.margin = margin(20, 25, 15, 15),
    plot.caption = element_text(size = 12, color = "#8A8A8A", hjust = 0, margin = margin(t = 10)),
    plot.background = element_rect(fill = "white", color = "#D9D9D9", linewidth = 0.6),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(caption = "Dark blue = extra cost due to resistance, per hectare (2025 values).\nHollow = would still be spent without resistance.")
p
ggsave("W:/Economic impact of weeds round 2/Reports and papers/WeedSmartNZ Workshop/iwm_hollow_chart_v2.png", p, width = 9.5, height = 6.2, dpi = 200, bg = "white")
