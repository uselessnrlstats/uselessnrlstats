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
  library(showtext)
  library(ggrepel)
  remotes::install_github("wilkelab/gridtext")
  library(gridtext)
  library(ggtext)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")

##### Analysis #####
names_to_filter <- c("Tom", "Tommy", "Thomas")

name_trends <- player_match_data |>
  select(player_id, match_id) |>
  left_join(player_data |> select(player_id, full_name, first_name, last_name),
            by = "player_id") |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  filter(first_name %in% names_to_filter) |>
  distinct(player_id, full_name, last_name, year) |>
  group_by(year) |>
  arrange(last_name) |>
  summarise(
    n_players = length(unique(player_id)),
    players = paste(last_name, collapse = "\n"),
    .groups =
  ) |>
  complete(year = 1908:2023, fill = list(n_players = 0))

##### Plot #####
font_add_google("Montserrat")
showtext_auto(enable = TRUE)

name_plot <- ggplot() +
  geom_line(data = name_trends,
            aes(x = year, y= n_players), 
            col = "coral1") +
  # geom_point(data = name_trends |> filter(n_players > 0),
  #           aes(x = year, y= n_players), 
  #           col = "purple", size = 0.5) +
  scale_x_continuous(breaks = seq(1910, 2020, 10)) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  xlab("Season") +
  ylab("# of Players") +
  theme_classic() +
  geom_text_repel(
    data = name_trends |> filter(n_players == 15),
    aes(x = year, y = n_players, label = players),
    family = "Montserrat",
    size = 6,
    min.segment.length = 0.3,
    box.padding = 0.5,
    max.overlaps = Inf,
    arrow = arrow(length = unit(0.012, "npc"),),
    nudge_x = c(40,-20),
    nudge_y = -0.08,
    point.padding = unit(0.02, "npc"),
    color = "grey50",
    lineheight = 0.3,
    hjust = 0
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    plot.title = element_markdown(family = "Montserrat", size = 26, face = "bold"),
    plot.subtitle = element_textbox_simple(family = "Montserrat", size = 20, lineheight = 0.3, margin = margin(0,0,10,0))
  ) +
  labs(
    title = "First Name Frequency: THOMAS",
    subtitle = "The number of players with the first name **Thomas** that have played in each season of the NSWRL/NRL"
  )
name_plot

ggsave(filename = "plots/thomas_name_years.png", plot = name_plot, device = "png", type = "cairo",
       width = 10, height = 8, units = "cm")
