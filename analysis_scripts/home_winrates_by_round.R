##### Description #####
# An R script to look at win rate of home teams through the first 3 rounds

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(scales)
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
}

extrafont::loadfonts()

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")
ladder_data <- read_csv("cleaned_data/nrl/ladder_round_data.csv") |>
  group_by(competition_year, year, round) |>
  mutate(ladder_position = row_number()) |>
  ungroup()

##### Helper Stats #####
match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  filter(year >= 1998) |>
  select(competition_year, year, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(year, round, home_away, team, score_for, score_against, result) |>
  filter(grepl("Round", round)) |>
  mutate(round = gsub("Round (\\d+)", "\\1", round) |> as.numeric())

##### Analysis #####
plot_data1 <- match_results |>
  filter(home_away == "H") |>
  group_by(year, round) |>
  summarise(n_games = n(),
            n_wins = sum(result == "W"),
            win_rate = n_wins / n_games,
            .groups = "drop") |>
  mutate(year = as.factor(year))

plot_data2 <- match_results |>
  filter(home_away == "H") |>
  group_by(round) |>
  summarise(n_games = n(),
            n_wins = sum(result == "W"),
            win_rate = n_wins / n_games,
            .groups = "drop")

##### Plot #####

round_plot <- ggplot() +
  geom_point(data = plot_data1, aes(x = round, y = win_rate, size = n_games), 
             alpha = 0.2, position = position_jitter(height = 0, width = 0.1), colour = "grey10") +
  geom_line(data = plot_data2, aes(x = round, y = win_rate), colour = "#EF1932", linewidth = 2) +
  scale_x_continuous(breaks = seq(1, 27, by=1), expand = c(0.01,0)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1), expand = c(0.01,0), labels = label_percent()) +
  scale_size_area(max_size = 5) +
  xlab("Round") +
  ylab("Home-team Win Rate (%)") +
  theme_classic() +
  theme(
    text = element_text(family = "Montserrat"),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    plot.title = element_markdown(family = "Montserrat", size = 13, face = "bold"),
    panel.grid.major.x = element_line()
  ) +
  labs(
    title = "NRL (1998-present) Home-team Win-rates by Round"
  )
round_plot


ggsave(filename = "plots/name_map.pdf", plot = name_map, 
       device = cairo_pdf, width = 10, height = 7, units = "in")
pdf_convert("plots/name_map.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/name_map.png")
