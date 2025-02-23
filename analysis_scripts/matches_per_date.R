##### Description #####
# An R script to look at matches where the halftime or fulltime score have been
# equal to the date as day-month

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(stringr)
  library(tidyverse)
  library(ggrepel)
  library(ggtext)
  library(RColorBrewer)
  library(pdftools)
  remotes::install_github("wilkelab/gridtext")
  library(gridtext)
  library(ggtext)
  library(pdftools)
  library(ggimage)
  library(paletteer)
  library(extrafont)
  # library(showtext)
}

extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

##### Analysis #####
matches_per_day <- match_data |>
  mutate(date_of_year = format(date, format = "%d/%m")) |>
  filter(year(date) >= 1998) |>
  group_by(date_of_year) |>
  summarise(n_matches = n(),
            .groups = "drop") |>
  complete(date_of_year = seq.Date(as.Date("2024-02-01"), as.Date("2024-10-31"), by = "days") |> format("%d/%m"),
           fill = list(n_matches = NA)) |>
  mutate(month = gsub("\\d{2}\\/(\\d{2})", "\\1", date_of_year) |> as.numeric(),
         month_abb = factor(month.abb[month], levels = month.abb, ordered = TRUE),
         day = gsub("(\\d{2})\\/\\d{2}", "\\1", date_of_year) |> 
           as.numeric()
        ) |>
  arrange(month, day)

##### Plot #####
fm <- 2
text_col <- "gray90"
bg_col <- "gray10"

matches_plot <- ggplot(matches_per_day) +
  geom_tile(aes(x = day, y = month_abb,
                fill = n_matches), 
            col = NA, linewidth = 0) +
  scale_fill_gradientn(name = "# Matches",
                       colours = paletteer_c("viridis::inferno", 30)[5:30],
                       guide = guide_coloursteps(
                         direction = "horizontal"
                        ),
                       n.breaks = 10,
                       na.value = "#180C3D",) +
  scale_x_continuous(position = "top",
                     expand = c(0,0),
                     sec.axis = dup_axis(name = element_blank()),
                     breaks = 1:31) +
  scale_y_discrete(position = "left", expand = c(0,0), limits = rev, drop = TRUE) +
  theme_classic() +
  theme(
    text = element_text(family = "Montserrat", colour = text_col),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_text(family = "Montserrat", face = "bold", size = 6*fm, angle = 90, margin = margin(r = 5)),
    axis.title.x = element_text(family = "Montserrat", face = "bold", size = 6*fm, margin = margin(b = 5)),
    axis.text.y = element_text(size = 5*fm, margin = margin(r = 1), colour = text_col),
    axis.text.x.top = element_text(size = 5*fm, margin = margin(b = 1), colour = text_col),
    axis.text.x.bottom = element_text(size = 5*fm, margin = margin(b = 1), colour = text_col),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = "Montserrat", size = 9*fm, face = "bold", margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10),
    legend.title = element_text(family = "Montserrat", size = 5*fm, hjust = 0.5),
    legend.title.position = "top",
    legend.text = element_text(family = "Montserrat", size = 4*fm, margin = margin(t = 2), hjust = 0.5),
    legend.position = "bottom",
    legend.margin = margin(t = -5),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(3, "lines"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = bg_col)
  ) +
  labs(
    x = "Date",
    y = "Month",
    title = "NRL era (1998-) Matches per Date"
  )
matches_plot

ggsave(filename = "plots/matches_per_date_NRL.png", plot = matches_plot, 
       width = 8, height = 7, units = "in", dpi = 1000)


pdf_convert("plots/matches_per_date_NRL.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/matches_per_date_NRL.png")
