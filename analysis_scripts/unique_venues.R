##### Description #####
# An R script to look at players who have played at the most unique venues

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
venue_data <- read_csv("cleaned_data/nrl/venue_data.csv")

##### Analysis #####
player_match_data |>
  select(player_id, match_id, team, opposition_team) |>
  left_join(match_data |> select(match_id, competition_year, round, date, venue_id, home_team, home_team_score, away_team, away_team_score),
            by = "match_id") |>
  filter(competition_year != "NRL 2023") |>
  mutate(win_loss = case_when(
    team == home_team & home_team_score > away_team_score ~ "W",
    team == home_team & home_team_score < away_team_score ~ "L",
    team == home_team & home_team_score == away_team_score ~ "D",
    team == away_team & home_team_score > away_team_score ~ "L",
    team == away_team & home_team_score < away_team_score ~ "W",
    team == away_team & home_team_score == away_team_score ~ "D",
    .default = NA
  )) |>
  select(-c(home_team, home_team_score, away_team, away_team_score)) |>
  group_by(player_id, venue_id) |>
  summarise(total_wins = sum(win_loss == "W"),
            total_draws = sum(win_loss == "D"),
            total_losses = sum(win_loss == "L"),
            .group = "drop") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  left_join(venue_data |> select(venue_id, `non-commercial_name`), by = "venue_id") |>
  View()

player_match_data |>
  select(player_id, match_id, team, opposition_team) |>
  left_join(match_data |> select(match_id, competition_year, round, date, venue_id, home_team, home_team_score, away_team, away_team_score),
            by = "match_id") |>
  filter(competition_year != "NRL 2023") |>
  mutate(win_loss = case_when(
    team == home_team & home_team_score > away_team_score ~ "W",
    team == home_team & home_team_score < away_team_score ~ "L",
    team == home_team & home_team_score == away_team_score ~ "D",
    team == away_team & home_team_score > away_team_score ~ "L",
    team == away_team & home_team_score < away_team_score ~ "W",
    team == away_team & home_team_score == away_team_score ~ "D",
    .default = NA
  )) |>
  select(-c(home_team, home_team_score, away_team, away_team_score)) |>
  group_by(player_id, win_loss) |>
  summarise(num_venues = length(unique(venue_id)),
            unique_venues = paste(unique(`venue_id`), collapse = " | "),
            .group = "drop") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  #left_join(venue_data |> select(venue_id, `non-commercial_name`), by = "venue_id") |>
  View()
