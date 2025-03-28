##### Description #####
# Script to join and clean data scraped from rugby league project.

##### Libraries #####
library(chron)
library(lubridate)
library(tidyverse)
library(tools)

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")

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
  ##### Apply special cases
  # Remove second South Sydney bye in 1908
  mutate(result = ifelse(year == 1908 & round == 4 & team == "South Sydney Rabbitohs", NA, result)) |>
  # Apply Points
  mutate(points = case_when(
                    result == "W" ~ 2,
                    result == "L" ~ 0,
                    result == "D" ~ 1,
                    result == "B" ~ 2,
                    .default = 0
  )) |>
  ##### Apply special cases
  mutate(
    points = case_when(
      # Bulldogs 2009 -2 points Rd2 Interchange
      year == 2009 & round == 2 & team == "Bulldogs" ~ 0,
      # Cowboys 2000 -2 points Rd4 Interchange
      year == 2000 & round == 4 & team == "North Queensland Cowboys" ~ 0,
      # Warriors 1995 -2 points Rd3 Interchange
      year == 1995 & round == 3 & team == "Auckland Warriors" ~ 0,
      # Tigers 1993 -2 points Rd19 Interchange
      year == 1993 & round == 19 & team == "Balmain Tigers" ~ 0,
      # Gold Coast 1992 -2 points Rd3 Interchange
      year == 1992 & round == 3 & team == "Gold Coast Seagulls" ~ 0,
      # Rabbitohs 1988 -2 points Rd8 Interchange
      year == 1988 & round == 8 & team == "South Sydney Rabbitohs" ~ 0,
      # Western Suburbs 1975 -1 point Rd8 Interchange
      year == 1975 & round == 8 & team == "Western Suburbs Magpies" ~ -1,
      # Bulldogs 2002 Salary Cap -37 points Rd24
      year == 2002 & round == 24 & team == "Bulldogs" ~ points - 37,
      # Warriors 2006 Salary Cap -4 points Rd1
      year == 2006 & round == 1 & team == "Warriors" ~ points - 4,
      # Melbourne 2010 Salary Cap -8 and then none
      year == 2010 & round == 7 & team == "Melbourne Storm" ~ -8,
      year == 2010 & round > 7 & team == "Melbourne Storm" ~ 0,
      # Parramatta 2016 salary cap -12, -PF/PA, 
      year == 2016 & round == 10 & team == "Parramatta Eels" ~ -12,
      .default = points
    ),
    score_for = case_when(
      # Parramatta 2016 salaray cap -12, -PF/PA, 
      year == 2016 & round <= 9 & team == "Parramatta Eels" ~ 0,
      .default = score_for
    ),
    score_against = case_when(
      # Parramatta 2016 salaray cap -12, -PF/PA, 
      year == 2016 & round <= 9 & team == "Parramatta Eels" ~ 0,
      .default = score_against
    )
  ) |>
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
          "cleaned_data/nrl/ladder_round_data.csv")
