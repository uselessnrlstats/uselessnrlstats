##### Description #####
# An R script to look at all one-point matches

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(ggrepel)
  library(ggtext)
  library(RColorBrewer)
  library(pdftools)
  remotes::install_github("wilkelab/gridtext")
  library(gridtext)
  library(pdftools)
  library(ggimage)
  library(paletteer)
}

#extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
plot_data <- player_match_data |>
  select(player_id, match_id, team) |>
  left_join(match_data |> select(match_id, competition, competition_year, date),
            by = "match_id") |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         day_of_week = lubridate::wday(date, label = TRUE),
         friday = (day_of_week == "Fri")) |>
  group_by(player_id) |>
  summarise(
    first_year = min(year),
    last_year = max(year),
    n_matches = n(),
    fri_matches = sum(friday),
    bronco = ifelse("Brisbane Broncos" %in% unique(team), "Yes", "No"),
    n_broncos = sum(team == "Brisbane Broncos"),
    .groups = "drop"
  ) |>
  mutate(other_matches = n_matches - fri_matches,
         bronco = factor(bronco, levels = c("Yes", "No"))) |>
  left_join(player_data |> select(player_id, full_name), 
            by = "player_id") |>
  arrange(first_year, last_year)

##### Plot #####
fm <- 2
max_limit <- max(plot_data$fri_matches) + 1
text_col <- "gray10"

friday_games1 <- ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = max_limit, yend = max_limit), 
               col = "coral1", linewidth = 0.8) +
  geom_point(data = plot_data, aes(x = other_matches, y = fri_matches, colour = first_year), 
             size = 2, alpha = 0.75, show.legend = TRUE) +
  scale_colour_gradientn(name = "Debut Year:",
                         colours = paletteer_c("viridis::viridis", 30)[1:27],
                         guide = guide_coloursteps(
                           direction = "horizontal"
                         ),
                         n.breaks = 10) +
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0, 350, by = 50)) +
  scale_y_continuous(expand = c(0.01,0), breaks = seq(0, 150, by = 25)) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    text = element_text(family = "Montserrat", colour = text_col),
    axis.line = element_line(colour = text_col),
    axis.ticks = element_line(colour = text_col),
    axis.text = element_text(family = "Montserrat Light", colour = text_col),
    axis.title.y = element_text(size = 5*fm),
    axis.title.x = element_text(size = 5*fm),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 4*fm, hjust = 1),
    legend.title.position = "left",
    legend.text = element_text(family = "Montserrat Light", size = 3.5*fm, margin = margin(t = 2), hjust = 0.5),
    legend.position = "bottom",
    legend.margin = margin(t = -3),
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(4, "lines"),
    legend.background = element_blank(),
    plot.title = element_text(size = 6*fm, face = "bold", margin = margin(b = 5)),
    plot.title.position = "plot",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  ) +
  labs(
    x = "# Matches not played on a Friday",
    y = "# Matches played on Friday",
    title = "Friday Night Broncos",
    subtitle = "NSWRL/NRL players by # matches played on Friday/Not on Friday"
  )
friday_games1

ggsave(filename = "plots/friday_games1.pdf", plot = friday_games1, device = cairo_pdf,
       width = 7, height = 7, units = "in")
pdf_convert("plots/friday_games1.pdf",
            format = "png", dpi = 1000,
            filenames = "plots/friday_games1.png")

##### Add ggrepel #####
friday_games2 <- friday_games1 +
  geom_text_repel(
    data = plot_data |> 
      filter(((fri_matches > other_matches) & n_matches >= 15) |
               ((fri_matches > 75) & (other_matches <= 150)) |
               ((n_matches >= 325)) |
               (fri_matches > 100) |
               (other_matches >= 300)),
    aes(x = other_matches, y = fri_matches, label = full_name),
    family = "Montserrat",
    size = 3.2,
    min.segment.length = 0,
    box.padding = 0.25,
    force = 20,
    point.padding = unit(0, "npc"),
    color = "grey30")
friday_games2

ggsave(filename = "plots/friday_games2.pdf", plot = friday_games2, device = cairo_pdf,
       width = 7, height = 7, units = "in")
pdf_convert("plots/friday_games2.pdf",
            format = "png", dpi = 1000,
            filenames = "plots/friday_games2.png")

##### Broncos vs not #####
friday_games3 <- ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = max_limit, yend = max_limit), 
               col = "#FFB90F", linewidth = 0.8) +
  geom_point(data = plot_data |> arrange(desc(bronco), n_matches), 
             aes(x = other_matches, y = fri_matches, colour = bronco, alpha = bronco), 
             size = 2) +
  scale_colour_manual(name = "Played for Broncos:",
                      values = c("#E45C8F", "gray70"),
                      guide = guide_legend(
                        direction = "horizontal",
                        override.aes = list(size = 5, shape = 15, fill = c("gray70", "#E45C8F"))
                      )) +
  scale_alpha_manual(guide = NULL,
                     values = c(0.75, 0.75)) +
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0, 350, by = 50)) +
  scale_y_continuous(expand = c(0.01,0), breaks = seq(0, 150, by = 25)) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    text = element_text(family = "Montserrat", colour = text_col),
    axis.line = element_line(colour = text_col),
    axis.ticks = element_line(colour = text_col),
    axis.text = element_text(family = "Montserrat Light", colour = text_col),
    axis.title.y = element_text(size = 5*fm),
    axis.title.x = element_text(size = 5*fm),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 4*fm, hjust = 1),
    legend.title.position = "left",
    legend.text = element_text(family = "Montserrat Light", size = 3.5*fm, margin = margin(t = 2), hjust = 0.5),
    legend.position = "bottom",
    legend.margin = margin(t = -3),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(1, "lines"),
    legend.background = element_blank(),
    plot.title = element_text(size = 6*fm, face = "bold", margin = margin(b = 5)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 4.5*fm, hjust = 0),
    plot.margin = margin(t = 5, r = 5, b = 10, l = 5)
  ) +
  labs(
    x = "# Matches not played on a Friday",
    y = "# Matches played on a Friday",
    title = "Friday Night Broncos",
    subtitle = "NSWRL/NRL players by # matches played on Friday/Not on Friday (<span style = 'color: #FFB90F;'>**gold line**</span> represents equal counts)"
  )
friday_games3

ggsave(filename = "plots/friday_games3.pdf", plot = friday_games3, device = cairo_pdf,
       width = 7, height = 7, units = "in")
pdf_convert("plots/friday_games3.pdf",
            format = "png", dpi = 1000,
            filenames = "plots/friday_games3.png")

##### Add ggrepel #####
friday_games4 <- friday_games3 +
  geom_text_repel(
    data = plot_data |> 
      filter(((fri_matches > other_matches) & n_matches >= 15) |
               ((fri_matches > 75) & (other_matches <= 150)) |
               ((n_matches >= 325)) |
               (fri_matches > 100) |
               (other_matches >= 300)),
    aes(x = other_matches, y = fri_matches, label = full_name),
    family = "Montserrat",
    size = 3.2,
    min.segment.length = 0,
    box.padding = 0.25,
    force = 20,
    point.padding = unit(0, "npc"),
    color = "grey30")
friday_games4

ggsave(filename = "plots/friday_games4.pdf", plot = friday_games4, device = cairo_pdf,
       width = 7, height = 7, units = "in")
pdf_convert("plots/friday_games4.pdf",
            format = "png", dpi = 1000,
            filenames = "plots/friday_games4.png")
