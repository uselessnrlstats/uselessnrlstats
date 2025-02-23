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
  #library(showtext)
  library(ggrepel)
  remotes::install_github("wilkelab/gridtext")
  library(gridtext)
  library(ggtext)
  library(pdftools)
  library(extrafont)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
coach_match_data <- read_csv("cleaned_data/nrl/coach_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")

loadfonts()

##### Analysis #####
names_to_filter <- c("Craig")

name_trends_player <- player_match_data |>
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
  complete(year = 1908:2024, fill = list(n_players = 0)) |>
  mutate(type = "Players")

name_trends_coach <- coach_match_data |>
  select(coach_id, match_id) |>
  left_join(coach_data |> select(coach_id, full_name, first_name, last_name),
            by = "coach_id") |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  filter(first_name %in% names_to_filter) |>
  distinct(coach_id, full_name, last_name, year) |>
  group_by(year) |>
  arrange(last_name) |>
  summarise(
    n_players = length(unique(coach_id)),
    players = paste(last_name, collapse = "\n"),
    .groups =
  ) |>
  complete(year = 1908:2024, fill = list(n_players = 0)) |>
  mutate(type = "Coaches")

name_trends <- rbind(
  name_trends_player,
  name_trends_coach
) |>
  mutate(type = factor(type, levels = c("Players", "Coaches"), ordered = TRUE))

##### Plot #####
#font_add_google("Montserrat")
#showtext_auto(enable = TRUE)

name_plot <- ggplot() +
  geom_line(data = name_trends,
            aes(x = year, y= n_players, col = type),
            linewidth = 1) +
  scale_x_continuous(breaks = seq(1910, 2020, 10)) +
  scale_y_continuous(breaks = seq(0, 24, by = 3), expand = c(0,0.3)) +
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
  geom_text_repel(
    data = name_trends_player |> filter(year %in% c(1995)),
    aes(x = year, y = n_players, label = players),
    family = "Montserrat",
    size = 3,
    min.segment.length = 0.3,
    box.padding = 0.5,
    max.overlaps = Inf,
    arrow = arrow(length = unit(0.012, "npc"),),
    nudge_x = -30,
    nudge_y = -10,
    point.padding = unit(0.02, "npc"),
    color = "grey50",
    lineheight = 0.9,
    hjust = 1,
    vjust = 0.5
  ) +
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
    title = "First Name Frequency: CRAIG",
    subtitle = "The number of players/coaches with the first name **Craig** in each season of the NSWRL/NRL"
  )
name_plot







ggsave(filename = "plots/craig_name_years.png", plot = name_plot, device = "png", type = "cairo",
       width = 5, height = 4, units = "in")

ggsave(filename = "plots/craig_name_years.pdf", plot = name_plot, 
       device = cairo_pdf,
       width = 6, height = 5, units = "in")
pdf_convert("plots/craig_name_years.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/craig_name_years.png")
