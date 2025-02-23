##### Description #####
# Script to join and clean data scraped from rugby league project.

##### Libraries #####
library(chron)
library(lubridate)
library(tidyverse)
library(tools)

##### Load Data #####
match_data <- read_csv("cleaned_data/nrlw/match_data.csv")

##### Steps #####
###### Identify Teams in Each Season ######
season_teams <- match_data |>
  group_by(competition_year) |>
  summarise(teams = list(sort(unique(c(home_team, away_team)))),
            .groups = "drop")
  
###### Clean Data for each team ######
team_match_data <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(competition_year, year, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(competition_year, year, round, home_away, team, score_for, score_against, result) |>
  filter(grepl("Round", round)) |>
  mutate(round = gsub("Round (\\d+)", "\\1", round) |> as.numeric())

###### Identify Byes ######
bye_team_rounds <- team_match_data |>
  group_by(competition_year, round) |>
  summarise(teams_played = list(sort(unique(c(team)))),
            .groups = "drop") |>
  left_join(season_teams, by = "competition_year") |>
  rowwise() |>
  mutate(bye_team = list(teams[which(!(teams %in% teams_played))])) |>
  unnest(bye_team) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         home_away = NA,
         score_for = NA,
         score_against = NA,
         result = "B") |>
  rename(team = bye_team) |>
  select(competition_year, year, round, home_away, team, score_for, score_against, result)

###### Produce ladders ######
round_ladders <- team_match_data |>
  rbind(bye_team_rounds) |>
  arrange(year, round) |>
  # Apply Points
  mutate(points = case_when(
                    result == "W" ~ 2,
                    result == "L" ~ 0,
                    result == "D" ~ 1,
                    result == "B" ~ 2,
                    .default = 0
  )) |>
  group_by(competition_year, year, round, team) |>
  summarise(points = sum(points, na.rm = TRUE),
            played = sum(result %in% c("W", "L", "D"), na.rm = TRUE),
            wins = sum(result == "W", na.rm = TRUE),
            losses = sum(result == "L", na.rm = TRUE),
            draws = sum(result == "D", na.rm = TRUE),
            byes = sum(result == "B", na.rm = TRUE),
            score_for = sum(score_for, na.rm = TRUE),
            score_against = sum(score_against, na.rm = TRUE),
            .groups = "drop") |>
  arrange(year, round) |>
  group_by(competition_year, year, team) |>
  mutate(points = cumsum(points),
         played = cumsum(played),
         wins = cumsum(wins),
         losses = cumsum(losses),
         draws = cumsum(draws),
         byes = cumsum(byes),
         score_for = cumsum(score_for),
         score_against = cumsum(score_against),
         score_diff = score_for - score_against) |>
  select(competition_year, year, round, team, points, played, wins, losses, draws, byes, score_for, score_against, score_diff) |>
  arrange(year, round, desc(points), desc(score_diff), desc(score_for), desc(score_against)) |>
  group_by(competition_year, round) |>
  mutate(ladder_position = row_number()) |>
  ungroup() |>
  relocate(ladder_position, .after = round)

write_csv(round_ladders, 
          "cleaned_data/nrlw/ladder_round_data.csv")
