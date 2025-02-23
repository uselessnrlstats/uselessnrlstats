##### Description #####
# An R script to look at players whose names give the highest scrabble score

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
  library(ggtext)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")

##### Analysis #####
names_to_filter <- c("Greg", "Gregory", "Gregg")
# names_to_filter <- c("John", "Johnny", "St John", "Martin-John", "Johnathan", "Johnno", "Jon", "Barrie-Jon", "Jonathan", "Jonathon")
names_to_filter <- c("Tom", "Thomas", "Tomas", "Tommy")
# names_to_filter <- c("Dave", "David", "Davey")
# names_to_filter <- c("Steve", "Steven", "Stephen")
# names_to_filter <- c("Rob", "Robert", "Bob", "Bobby", "Robby", "Robbie")
# names_to_filter <- c("Fletcher")
# names_to_filter <- c("Spencer")
# names_to_filter <- c("Geoff", "Jeff", "Geoffrey")


name_trends <- player_match_data |>
  select(player_id, match_id) |>
  left_join(player_data |> select(player_id, full_name, first_name, last_name),
            by = "player_id") |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  filter(first_name %in% names_to_filter) |>
  distinct(player_id, full_name, year) |>
  group_by(year) |>
  summarise(
    n_players = length(unique(player_id)),
    players = paste(full_name, collapse = ", "),
    .groups =
  ) |>
  complete(year = 1908:2023, fill = list(n_players = 0))

##### Plot #####
font_add_google("Montserrat")
showtext_auto(enable = TRUE)

name_plot <- ggplot() +
  geom_line(data = name_trends,
            aes(x = year, y= n_players), 
            col = "purple") +
  # geom_point(data = name_trends |> filter(n_players > 0),
  #           aes(x = year, y= n_players), 
  #           col = "purple", size = 0.5) +
  scale_x_continuous(breaks = seq(1910, 2020, 10)) +
  scale_y_continuous(breaks = c(0, 1)) +
  xlab("Season") +
  ylab("# of Players") +
  theme_classic() +
  # geom_text_repel(
  #   data = name_trends |> filter(n_players > 0) |> group_by(players) |> arrange(year) |> filter(row_number() == 1),
  #   aes(x = year, y = n_players, label = players),
  #   family = "Montserrat",
  #   size = 6,
  #   min.segment.length = 0.3, 
  #   box.padding = 0.5,
  #   max.overlaps = Inf,
  #   arrow = arrow(length = unit(0.012, "npc"),),
  #   nudge_x = -15,
  #   nudge_y = -0.08,
  #   point.padding = unit(0.02, "npc"),
  #   color = "grey50"
  # ) +
  theme(
    text = element_text(family = "Montserrat"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    plot.title = element_markdown(family = "Montserrat", size = 26),
    plot.subtitle = element_textbox_simple(family = "Montserrat", size = 20, lineheight = 0.3, margin = margin(0,0,10,0))
  ) +
  labs(
    title = "**First Name Frequency: GREG**",
    subtitle = "The number of players with the first name **Greg** that have played in each season of the NSWRL/NRL"
  )
name_plot

ggsave(filename = "plots/Fletcher_name_years.png", plot = name_plot, device = "png", type = "ragg",
       width = 10, height = 8, units = "cm")



saveWidget(final_table, "tables/html/score_dates1.html")
webshot(url = "tables/html/score_dates1.html", 
        file = "tables/png/score_dates1.png", 
        selector = "body", zoom = 4,
        vwidth = 730)
