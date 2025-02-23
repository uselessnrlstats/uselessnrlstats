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
  library(pdftools)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/player_data.csv")
player_match_data <- read_csv("cleaned_data/player_match_data.csv")
match_data <- read_csv("cleaned_data/match_data.csv")

##### Analysis #####
names_to_filter <- c("Bob", "Jason")

name_trends <- player_match_data |>
  select(player_id, match_id) |>
  left_join(player_data |> select(player_id, full_name, first_name, last_name),
            by = "player_id") |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  filter(first_name %in% names_to_filter) |>
  distinct(player_id, full_name, first_name, last_name, year) |>
  group_by(year, first_name) |>
  arrange(last_name) |>
  summarise(
    n_players = length(unique(player_id)),
    players = paste(full_name, collapse = "\n"),
    .groups = "drop"
  ) |>
  complete(year = 1908:2024, first_name, fill = list(n_players = 0))

##### Plot #####
font_add_google("Montserrat")
showtext_auto(enable = TRUE)

name_plot <- ggplot() +
  geom_line(data = name_trends,
            aes(x = year, y = n_players, col = first_name), 
            linewidth = 0.8) +
  scale_x_continuous(breaks = seq(1910, 2020, 10), expand = c(0.01,0)) +
  scale_y_continuous(breaks = seq(0, 25, by = 2), limits = c(0, 25), expand = c(0.01,0)) +
  scale_colour_manual(
    values = c("#268BD2", "#FF6F09"),
    name = NULL,
    guide = guide_legend(
      override.aes = list(linewidth = 1)
    )
  ) +
  xlab("Season") +
  ylab("# of Players") +
  theme_classic() +
  # geom_text_repel(
  #   data = name_trends |> filter(n_players > 0) |> group_by(players) |> arrange(year) |> filter(row_number() == 1),
  #   aes(x = year, y = n_players, label = players),
  #   family = "Montserrat",
  #   size = 4,
  #   min.segment.length = 0.5,
  #   box.padding = 0.5,
  #   max.overlaps = Inf,
  #   arrow = arrow(length = unit(0.012, "npc"),),
  #   nudge_x = c(20, -20, -20),
  #   nudge_y = -0.08,
  #   point.padding = unit(0.02, "npc"),
  #   color = "grey50"
  # ) +
  theme(
    text = element_text(family = "Montserrat"),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    plot.title = element_markdown(family = "Montserrat", size = 13, face = "bold"),
    plot.subtitle = element_textbox_simple(family = "Montserrat", size = 10, lineheight = 0.3, margin = margin(0,0,10,0)),
    legend.title = element_text(family = "Montserrat", size = 10, hjust = 0.5),
    legend.title.position = "top",
    legend.text = element_text(family = "Montserrat", size = 8, hjust = 0.5),
    legend.position = "bottom",
    legend.margin = margin(t = -5),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(3, "lines")
  ) +
  labs(
    title = "First Name Frequency: FLETCHER",
    subtitle = "The number of players with the first name **Fletcher** that have played in each season of the NSWRL/NRL"
  )
name_plot

ggsave(filename = "plots/fletcher_name_years.png", plot = name_plot, device = "png", type = "cairo",
       width = 6, height = 3.75, units = "in")

ggsave(filename = "plots/fletcher_name_years.pdf", plot = name_plot, 
       width = 6, height = 5, units = "in")
pdf_convert("plots/fletcher_name_years.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/fletcher_name_years.png")
x