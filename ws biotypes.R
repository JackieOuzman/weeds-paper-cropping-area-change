library(tidyverse)
library(ggrepel)
# --- Plot 1 data: AU / NZ / World biotypes over time ---
hr_totals <- read_csv(
  "C:/Users/ouz001/Downloads/HR_biotypes_AU_NZ_World.csv",
  comment = "#"
)

# --- Plot 2 data: AU / NZ by herbicide class over time ---
hr_by_class <- read_csv(
  "C:/Users/ouz001/Downloads/HR_by_class_AU_NZ.csv",
  comment = "#"
)


#Plot 1: Australia vs NZ vs World — this data is wide format

hr_totals_long <- hr_totals %>%
  select(Year, Australia_resistant_biotypes, New_Zealand_resistant_biotypes, World_resistant_biotypes) %>%
  pivot_longer(
    cols = -Year,
    names_to = "region",
    values_to = "cumulative_cases"
  ) %>%
  mutate(region = recode(region,
                         "Australia_resistant_biotypes" = "Australia",
                         "New_Zealand_resistant_biotypes" = "New Zealand",
                         "World_resistant_biotypes" = "World"
  )) %>%
  filter(!is.na(cumulative_cases))

Plot_world_Aust_NZ_HR_biotypes <- ggplot(hr_totals_long, aes(x = Year, y = cumulative_cases, colour = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Herbicide-resistant biotypes over time",
    subtitle = "Australia, New Zealand, and World",
    x = "Year", y = "Cumulative resistant biotypes", colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
Plot_world_Aust_NZ_HR_biotypes



##########################################################################
hr_au_nz <- hr_totals_long %>%
  filter(region != "World")

hr_au_nz_labels <- hr_au_nz %>%
  filter(Year == max(Year))

Plot_au_nz_HR_biotype<-
  ggplot(hr_au_nz, aes(x = Year, y = cumulative_cases, colour = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  annotate("text", x = 1981, y = 95, label = "Australia",
           colour = "#003f5c", fontface = "bold", size = 4.5, hjust = 0) +
  annotate("text", x = 1981, y = 85, label = "New Zealand",
           colour = "#7fcdff", fontface = "bold", size = 4.5, hjust = 0) +
  scale_colour_manual(values = c(
    "Australia" = "#003f5c",
    "New Zealand" = "#7fcdff"
  )) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10)) +
  labs(
    title = NULL,
    x = NULL,
    y = "Cumulative\nresistant biotypes",
    caption = "Source:\nInternational Herbicide-Resistant Weed Database\n(weedscience.org, accessed Aug 2026)"
    
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, colour = "grey40")
  )
Plot_au_nz_HR_biotype
#########################################################################


# scale factor: World's max is ~500, AU/NZ's max is ~95, so roughly /5.3 to overlay sensibly
scale_factor <- max(hr_totals_long$cumulative_cases[hr_totals_long$region == "World"], na.rm = TRUE) /
  max(hr_totals_long$cumulative_cases[hr_totals_long$region != "World"], na.rm = TRUE)

ggplot() +
  geom_line(data = filter(hr_all_long, region != "World"),
            aes(x = Year, y = cumulative_cases, colour = region), linewidth = 1.2) +
  geom_line(data = filter(hr_all_long, region == "World"),
            aes(x = Year, y = cumulative_cases / scale_factor, colour = region), linewidth = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Cumulative\nresistant biotypes",
    sec.axis = sec_axis(~ . * scale_factor, name = "World biotypes")
  ) +
  scale_colour_manual(values = c("Australia" = "#003f5c", "New Zealand" = "#7fcdff", "World" = "grey60")) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")
#########################################################################

world_latest <- hr_totals_long %>%
  filter(region == "World") %>%
  filter(Year == max(Year))

source_caption <- str_wrap(
  "Source: International Herbicide-Resistant Weed Database (weedscience.org)",
  width = 45
)

definition_caption <- str_wrap(
  "A biotype is a genetically distinct population of a weed species that has evolved resistance to a particular herbicide site of action.",
  width = 45
)
ggplot(hr_au_nz, aes(x = Year, y = cumulative_cases, colour = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  annotate("text", x = 1981, y = 95, label = "Australia",
           colour = "#003f5c", fontface = "bold", size = 4.5, hjust = 0) +
  annotate("text", x = 1981, y = 85, label = "New Zealand",
           colour = "#7fcdff", fontface = "bold", size = 4.5, hjust = 0) +
  annotate("label", x = 1981, y = 68,
           label = paste0("World total (", world_latest$Year, "):\n", world_latest$cumulative_cases, " biotypes"),
           colour = "grey30", fill = "white", size = 4, hjust = 0, label.size = 0.3) +
  scale_colour_manual(values = c("Australia" = "#003f5c", "New Zealand" = "#7fcdff")) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10)) +
  labs(
    title = NULL, x = NULL,
    y = "Cumulative\nresistant biotypes",
    caption = paste0(source_caption#, 
                     #"\n\n", definition_caption
    )
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 9, colour = "grey40"))



########################################################################
### Next set of plots #######
#######################################################################
hr_by_class <- read_csv(
  "C:/Users/ouz001/Downloads/HR_by_class_AU_NZ.csv",
  comment = "#"
)

class_labels <- hr_by_class %>%
  group_by(country, herbicide_class) %>%
  filter(year == max(year)) %>%
  ungroup()

class_colours <- c(
  "ACCase inhibitors" = "#003f5c",
  "ALS inhibitors" = "#2f8fbf",
  "Glycines (glyphosate)" = "#7fcdff",
  "PSII (incl. Triazines)" = "#a9a9a9"
)

ggplot(hr_by_class, aes(x = year, y = cumulative_cases, colour = herbicide_class)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~country) +
  scale_colour_manual(values = class_colours) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 20)) +
  labs(
    title = NULL,
    x = NULL,
    y = "Cumulative\nresistant cases",
    colour = NULL,
    caption = str_wrap(
      "Source: International Herbicide-Resistant Weed Database (weedscience.org, accessed Aug 2026). 'PSII (incl. Triazines)' is a broader category than Triazines alone — see data notes.",
      width = 70
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8, colour = "grey40")
  )+
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))



