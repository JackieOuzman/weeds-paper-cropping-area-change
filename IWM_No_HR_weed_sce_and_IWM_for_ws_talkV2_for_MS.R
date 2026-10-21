# IWM_No_HR_weed_sce_and_IWM_for_ws_talkV2_for_MS.R
# Manuscript version of IWM_No_HR_weed_sce_and_IWM_for_ws_talkV2.R (Figure 4: IWM expenditure per hectare by practice).
# Changes: journal-size text (9 pt base), no in-plot note (caption is in the Word document: 2021 prices),
# prints the plotted values and warns if they differ from Table 7, saves EPS + 300 dpi PNG to HR_MS/figures.
# NOTE: the Excel source must have competitive crop seeding NR = $2.51 (was $0.25 before the model fix).

library(readxl)
library(ggplot2)
library(scales)

fig_dir <- "W:/Economic impact of weeds round 2/Reports and papers/HR_MS/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

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

print(df)

# Sanity check against Table 7 (extra IWM cost, $/ha). Warns if the Excel values are stale.
table7_extra <- c("Break crops" = 10.69, "Double knock" = 9.48,
                  "Competitive\ncrop seeding" = 2.51, "Crop topping" = 3.16, "HWSC" = 1.65)
chk <- abs(df$extra - table7_extra[as.character(df$practice)])
if (any(is.na(chk)) || any(chk > 0.02)) {
  warning("Chart values differ from Table 7 - check the Excel (competitive crop seeding NR should be $2.51, extra $2.51).")
}

plot_df <- rbind(
  data.frame(practice = df$practice, segment = "baseline", value = df$nr),
  data.frame(practice = df$practice, segment = "extra", value = df$extra)
)
plot_df$segment <- factor(plot_df$segment, levels = c("baseline", "extra"))

navy <- "#12294B"

p <- ggplot(plot_df, aes(x = practice, y = value, fill = segment)) +
  geom_col(width = 0.55, colour = navy, linewidth = 0.4) +
  geom_text(data = df, aes(x = practice, y = total, label = paste0("$", sprintf("%.2f", total))),
            inherit.aes = FALSE, vjust = -0.6, size = 3.2, fontface = "bold", color = navy) +
  geom_text(data = df, aes(x = practice, y = extra / 2, label = paste0("+$", sprintf("%.2f", extra))),
            inherit.aes = FALSE, vjust = 0.5, size = 3, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("baseline" = "white", "extra" = navy)) +
  scale_x_discrete(labels = c("Break crops" = "Break\ncrops",
                              "Double knock" = "Double\nknock",
                              "Crop topping" = "Crop\ntopping",
                              "Competitive\ncrop seeding" = "Competitive\ncrop seeding",
                              "HWSC" = "HWSC")) +
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
ggsave(file.path(fig_dir, "Figure4.eps"), p, device = cairo_ps, width = 14, height = 8, units = "cm")
ggsave(file.path(fig_dir, "Figure4.png"), p, width = 14, height = 8, units = "cm", dpi = 300, bg = "white")
