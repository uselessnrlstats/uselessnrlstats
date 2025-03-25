##### Description #####
# An R script to look at trends of names throughout the years

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
  remotes::install_github("wilkelab/gridtext")
  library(gridtext)
  library(ggtext)
  library(extrafont)
  library(pdftools)
  library(ggimage)
  library(paletteer)
}

#extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")

##### Helper Stats #####
player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year)) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

##### Analysis #####
names_to_filter <- c("Owen")

player_years <- player_match_data |>
  select(player_id, match_id, team) |>
  left_join(player_data |> select(player_id, first_name, last_name, full_name), by = "player_id") |>
  left_join(match_data |> select(match_id, competition_year, round, date), 
            by = "match_id") |>
  mutate(year = year(date)) |>
  filter(first_name == "Owen") |>
  distinct(player_id, full_name, last_name, year) |>
  arrange(year)
  
player_start_years <- player_career_years |>
  left_join(player_data |> select(player_id, first_name, last_name, full_name),
            by = "player_id") |>
  filter(first_name == "Owen") |>
  arrange(first_year) |>
  mutate(label = ifelse(
    player_id == 50791,
    paste0("<b><span style = 'color: #4EEE94;'>", full_name, "</span></b><br><span style = 'color: #CCCCCC;'>", career_years, "</span>"),
    paste0("<b>", full_name, "</b><br><span style = 'color: #CCCCCC;'>", career_years, "</span>")))

##### Plot #####
name_plot <- ggplot() +
  geom_tile(data = player_years,
            aes(x = year, y = factor(player_id, levels = player_start_years$player_id, ordered = TRUE)),
            fill = "#4EEE94", width = 1, height = 0.8) +
  geom_richtext(
    data = player_start_years,
    aes(x = ifelse(first_year <= 1930, last_year + 1, first_year - 1), 
        y = factor(player_id, levels = player_id, ordered = TRUE),
        label = label, hjust = ifelse(first_year <= 1930, 0, 1)),
    text.colour = "white", label.colour = NA, fill = NA,
    family = "Montserrat", size = 3
  ) +
  scale_x_continuous(breaks = seq(1910, 2020, 10), position = "top") +
  scale_y_discrete(limits = rev) +
  theme_classic() +
  theme(
    text = element_text(family = "Montserrat", colour = "white"),
    axis.text.x = element_text(size = 9, colour = "white"),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 10),
    axis.line.x = element_line(colour = "white"),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(colour = "white"),
    axis.ticks.y = element_blank(),
    plot.title = element_markdown(family = "Montserrat", size = 18, face = "bold"),
    plot.subtitle = element_textbox_simple(family = "Montserrat", size = 12, lineheight = 0.3, margin = margin(0,0,10,0)),
    plot.background = element_rect(fill = "grey20"),
    panel.background = element_rect(fill = "grey20")
  ) +
  labs(
    title = "OWEN TEN",
    subtitle = "Tracking the 10 NSWRL/NRL players named <span style = 'color: #4EEE94;'>OWEN</span> since 1908",
    x = "Season",
    y = NULL
  )
  
name_plot

ggsave(filename = "plots/owen_name_years.png", plot = name_plot, device = "png", type = "cairo",
       width = 7, height = 7, units = "in")

# ggsave(filename = "plots/owen_name_years.pdf", plot = name_plot, 
#        width = 6, height = 4.5, units = "in")
# pdf_convert("plots/owen_name_years.pdf", 
#             format = "png", dpi = 1000,
#             filenames = "plots/owen_name_years.png")
