##### Description #####
# An R script to look at combined match margins for players in their first x games

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")

##### Analysis #####
first_n_games <- 10

player_match_data |>
  select(player_id, match_id, team, opposition_team) |>
  left_join(match_data |> select(match_id, competition_year, round, date, home_team, home_team_score, away_team, away_team_score),
            by = "match_id") |>
  mutate(
    win_loss = case_when(
      team == home_team & home_team_score > away_team_score ~ "W",
      team == home_team & home_team_score < away_team_score ~ "L",
      team == home_team & home_team_score == away_team_score ~ "D",
      team == away_team & home_team_score > away_team_score ~ "L",
      team == away_team & home_team_score < away_team_score ~ "W",
      team == away_team & home_team_score == away_team_score ~ "D",
      .default = NA
    ),
    margin = abs(home_team_score - away_team_score),
    match_date = format(date, format = "%d-%m")
  ) |>
  select(-c(home_team, home_team_score, away_team, away_team_score)) |>
  arrange(date) |>
  group_by(player_id) |>
  filter(row_number() %in% 1:first_n_games) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  group_by(player_id, full_name) |>
  summarise(
    matches = n(),
    total_margin = sum(margin),
    margins = paste0(margin, collapse = ","),
    teams = paste0(unique(team), collapse = ", "),
    debut_year = min(year(date)),
    .groups = "drop"
  ) |>
  filter(matches == first_n_games) |>
  #filter(debut_year >= 1998) |>
  select(-matches) |>
  arrange(total_margin) |>
  View()
