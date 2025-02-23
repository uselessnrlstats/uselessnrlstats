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
}

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
plot_data <- match_results |>
  filter(home_away == "H") |>
  group_by(year, round) |>
  summarise(n_games = n(),
            n_wins = sum(result == "W"),
            .groups = "drop") |>
  group_by(year) |>
  arrange(round) |>
  mutate(cum_win_rate = cumsum(n_wins) / cumsum(n_games)) |>
  ungroup() |>
  mutate(year = as.factor(year))

##### Plot #####

ggplot(plot_data) +
  geom_line(aes(x = round, y = cum_win_rate, group = year, col = (year == 2024), alpha = (year == 2024)), linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1, 27, by=1), expand = c(0.01,0)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1), expand = c(0.01,0), labels = label_percent()) +
  scale_colour_manual(
    values = c("grey10", "#E51932"),
    name = "Season",
    guide = guide_legend(
      override.aes = list(linewidth = 1)
    )
  ) +
  scale_alpha_manual(
    values = c(0.2, 1),
    guide = "none"
  ) +
  xlab("Round") +
  ylab("Cumulative Home-team Win Rate (%)") +
  theme_classic()


ggsave(filename = "plots/name_map.pdf", plot = name_map, 
       device = cairo_pdf, width = 10, height = 7, units = "in")
pdf_convert("plots/name_map.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/name_map.png")
