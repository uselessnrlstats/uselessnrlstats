##### Description #####
# An R script to look at players who debuted on their birthdays

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
player_match_data |>
  select(player_id, match_id, team, opposition_team) |>
  left_join(match_data |> select(match_id, competition_year, round, date, home_team, home_team_score, away_team, away_team_score),
            by = "match_id") |>
  mutate(win_loss = case_when(
    team == home_team & home_team_score > away_team_score ~ "W",
    team == home_team & home_team_score < away_team_score ~ "L",
    team == home_team & home_team_score == away_team_score ~ "D",
    team == away_team & home_team_score > away_team_score ~ "L",
    team == away_team & home_team_score < away_team_score ~ "W",
    team == away_team & home_team_score == away_team_score ~ "D",
    .default = NA
  ),
         match_date = format(date, format = "%d-%m")
  ) |>
  select(-c(home_team, home_team_score, away_team, away_team_score)) |>
  arrange(date) |>
  group_by(player_id) |>
  filter(row_number() == 1) |>
  ungroup() |>
  left_join(player_data |> select(player_id, full_name, birthdate), by = "player_id") |>
  filter(match_date == birthdate) |>
  View()
  
