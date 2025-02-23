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
  library(ggforce)
  library(ggbump)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")

##### Analysis #####
name_trends <- player_match_data |>
  select(player_id, match_id) |>
  left_join(player_data |> select(player_id, full_name, first_name, last_name),
            by = "player_id") |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  distinct(player_id, full_name, first_name, last_name, year) |>
  group_by(year, first_name) |>
  arrange(last_name) |>
  filter(nchar(first_name) > 1) |>
  summarise(
    n_players = length(unique(player_id)),
    players = paste(full_name, collapse = "\n"),
    .groups = "drop"
  ) |>
  complete(year = 1908:2024, first_name, fill = list(n_players = 0)) |>
  ungroup() |>
  group_by(year) |>
  mutate(top_name = ifelse(n_players == max(n_players), first_name, NA)) |>
  ungroup() |>
  filter(first_name %in% top_name) |>
  mutate(top_name = !is.na(top_name))

##### Plot #####
font_add_google("Montserrat")
showtext_auto(enable = TRUE)

name_plot <- ggplot(name_trends, aes(x = year, y = n_players, col = first_name)) +
  geom_bump(alpha = 0.2, linewidth = 0.8) +
  geom_bump(data = ~. |> filter(top_name) |> mutate(dummy = (first_name != lag(first_name)) |> replace_na(TRUE), group = cumsum(dummy)),
            aes(group = group),
            alpha = 1, linewidth = 0.8) +
  geom_point(data = ~. |> filter(top_name)) +
  scale_x_continuous(breaks = seq(1910, 2020, 10), expand = c(0.01,0)) +
  scale_y_continuous(breaks = seq(0, 41, by = 2), limits = c(0, 40), expand = c(0.01,0)) +
  scale_colour_manual(
    values = c("#9c755f", "#7e0021", "#c5000b", "#e15759", "#ff420e", "#ff9da7", "#f2832b", "#ff950e", "#ffd320", "#edc948", "#aecf00", "#59a14f", "#579d1c","#314004", "#004586", "#0084d1", "#4e79a7", "#83caff", "#76b7b2","#b07aa1", "#4b1f6f"),
    name = NULL,
    guide = guide_legend(
      override.aes = list(linewidth = 1)
    )
  ) +
  scale_alpha_manual(
    values = c(0.2, 1),
    guide = "none"
  ) +
  xlab("Season") +
  ylab("# of Players") +
  theme_classic()
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
    title = "First Name Frequency: BOB vs JASON",
    subtitle = "The number of players with the first name **Bob** or **Jason** that have played in each season of the NSWRL/NRL"
  )
name_plot

ggsave(filename = "plots/bob_jason_name_years.pdf", plot = name_plot,
       width = 6, height = 5, units = "in")
pdf_convert("plots/bob_jason_name_years.pdf",
            format = "png", dpi = 1000,
            filenames = "plots/bob_jason_name_years.png")
