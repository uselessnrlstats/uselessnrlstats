##### Description #####
# An R script to look at matches where the goalkicker

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
  select(player_id, match_id, team, opposition_team, position, goals, goal_attempts) |>
  left_join(match_data |> 
              select(match_id, competition_year, round, date, home_team, home_team_score, away_team, away_team_score) |>
              mutate(date_of_year = format(date, format = "%d-%m"),
                     month = month(date),
                     day = day(date)),
            by = "match_id") |>
  mutate(win_loss = case_when(
    team == home_team & home_team_score > away_team_score ~ "W",
    team == home_team & home_team_score < away_team_score ~ "L",
    team == home_team & home_team_score == away_team_score ~ "D",
    team == away_team & home_team_score > away_team_score ~ "L",
    team == away_team & home_team_score < away_team_score ~ "W",
    team == away_team & home_team_score == away_team_score ~ "D",
    .default = NA
  )) |>
  #select(-c(home_team, home_team_score, away_team, away_team_score)) |>
  arrange(date) |>
  filter(!(is.na(goal_attempts))) |>
  filter(goals == day & goal_attempts == month) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id")

match_data |>
  mutate(date_of_year = format(date, format = "%d-%m"),
         month = month(date),
         day = day(date)) |>
  filter(day == home_team_score & month == away_team_score) |>
  arrange(date) |>
  select(competition_year, round, date_of_year, home_team, home_team_score, away_team, away_team_score)
