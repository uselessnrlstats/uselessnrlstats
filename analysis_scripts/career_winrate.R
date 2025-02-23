##### Description #####
# An R script to look at player career win rates

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
  #filter(competition_year != "NRL 2023") |>
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
  arrange(date) |>
  group_by(player_id) |>
  mutate(cum_wins = cumsum(win_loss == "W"),
         cum_games = cumsum(win_loss %in% c("W", "L", "D")),
         cum_win_rate = cum_wins / cum_games) |>
  filter(all(cum_win_rate < 0.5)) |>
  filter(row_number() == n()) |>
  left_join(player_data |> select(player_id, full_name), 
            by = "player_id")

##### Individual Players #####
# cory paix = 29827
# connor tracey = 28614
# tom ale = 30069
# jamayne tauno-brown = 29755
# taniel paseka = 25749
# scott drinkwater = 26828
# patrick carrigan = 28411
# kodi nikorima = 21474
# tohu harris = 20221

player_match_data |>
  filter(player_id %in% c(28614, 29827, 30069, 29755, 25749, 26828, 28411, 21474, 20221)) |>
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
  )) |>
  select(-c(home_team, home_team_score, away_team, away_team_score)) |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(cum_wins = cumsum(win_loss == "W"),
         cum_games = cumsum(win_loss %in% c("W", "L", "D")),
         cum_win_rate = cum_wins / cum_games) |>
  filter(all(cum_win_rate < 0.5) | all(cum_win_rate > 0.5)) |>
  left_join(player_data |> select(player_id, full_name), 
            by = "player_id")  |>
  ggplot() +
  theme_classic() +
  geom_line(aes(x = cum_games, y = cum_win_rate, colour = full_name)) +
  geom_segment(x = 0, y = 0.50, xend = 250, yend = 0.50, col = "red") +
  scale_colour_brewer(name = "Player", palette = "Set2") +
  xlim(c(0,250)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  ylab("Career Win Rate") + xlab("Game Number") +
  theme(legend.position = "bottom")
