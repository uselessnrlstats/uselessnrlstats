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
names_to_filter <- c("Bruce", "Gary", "Frank", "Ray", "Ken")

name_trends <- player_match_data |>
  select(player_id, match_id) |>
  left_join(player_data |> select(player_id, full_name, first_name, last_name),
            by = "player_id") |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  filter(first_name %in% names_to_filter) |>
  distinct(full_name, first_name, player_id, year) |>
  group_by(year, first_name) |>
  summarise(
    n_players = length(unique(player_id)),
    players = paste(full_name, collapse = ", "),
    .groups = "drop"
  ) |>
  complete(year = 1908:2023, first_name, fill = list(n_players = 0))

##### Plot #####
font_add_google("Montserrat")
showtext_auto(enable = TRUE)

name_plot <- ggplot() +
  geom_line(data = name_trends,
            aes(x = year, y= n_players, col = first_name),
            alpha = 0.7) +
  scale_colour_manual(values = c("purple","goldenrod2","cyan3","green4","maroon2") |> setNames(names_to_filter),
                      name = "First Name:") +
  scale_x_continuous(breaks = seq(1910, 2020, 10)) +
  scale_y_continuous(breaks = seq(0, 16, 2)) +
  xlab("Season") +
  ylab("# of Players") +
  theme_classic() +
  # geom_text_repel(
  #   data = name_trends[c(1,2),],
  #   aes(x = year, y = n_players, label = players),
  #   family = "Montserrat",
  #   size = 6,
  #   min.segment.length = 0.3, 
  #   box.padding = 0.5,
  #   max.overlaps = Inf,
  #   arrow = arrow(length = unit(0.012, "npc"),),
  #   nudge_x = c(15, -15),
  #   nudge_y = -0.08,
  #   point.padding = unit(0.02, "npc"),
  #   color = "grey50"
  # ) +
  theme(
    legend.position = "top",
    text = element_text(family = "Montserrat"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    plot.title = element_markdown(family = "Montserrat", size = 26),
    plot.subtitle = element_textbox_simple(family = "Montserrat", size = 20, lineheight = 0.3, margin = margin(0,0,10,0))
  ) +
  labs(
    title = "**First Name Frequency: Names of Yesteryear**",
    subtitle = "The number of players with the first name **Bruce**, **Gary**, **Frank**, **Ken** or **Ray** that have played in each season of the NSWRL/NRL"
  )
name_plot

ggsave(filename = "plots/robert_name_years.png", plot = name_plot, device = "png", type = "ragg",
       width = 12, height = 8, units = "cm")



saveWidget(final_table, "tables/html/score_dates1.html")
webshot(url = "tables/html/score_dates1.html", 
        file = "tables/png/score_dates1.png", 
        selector = "body", zoom = 4,
        vwidth = 730)
