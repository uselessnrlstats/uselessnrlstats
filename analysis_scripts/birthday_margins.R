##### Description #####
# An R script to look at biggest losses and wins on birthdays

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
    margin = case_when(
      team == home_team ~ home_team_score - away_team_score,
      team == away_team ~ away_team_score - home_team_score,
      .default = NA      
    ),
    match_date = format(date, format = "%d-%m")
  ) |>
  select(-c(home_team, home_team_score, away_team, away_team_score)) |>
  left_join(player_data |> select(player_id, full_name, birthdate), by = "player_id") |>
  filter(match_date == birthdate) |>
  arrange(margin) |>
  View()
  mutate(year = format(date, format = "%Y")) |>
  ggplot() +
  geom_bar(aes(x = margin), col = "blue") +
  theme_bw()
