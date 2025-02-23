##### Description #####
# An R script to look at unique positions and jersey numbers for players

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

##### Helper Stats #####
player_career_summaries <- player_match_data |>
  group_by(player_id) |>
  summarise(total_matches = n(),
            teams = list(unique(team)),
            numbers = list(unique(number, na.rm = TRUE)),
            positions = list(unique(position, na.rm = TRUE)),
            captaincy = sum(captain),
            total_tries = sum(tries + penalty_tries),
            total_goals = sum(goals),
            total_goal_attempts = sum(goal_attempts, na.rm = TRUE),
            total_field_goals = sum(field_goals),
            total_field_goals2 = sum(field_goals2),
            total_sin_bins = sum(sin_bins5 + sin_bins),
            total_send_offs = sum(send_offs),
            total_points = sum(points))

player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year),
            career = paste0(first_year, "-", last_year))

##### Analysis #####
# unique positions
player_match_data |>
  select(player_id, position) |>
  distinct() |>
  group_by(player_id) |>
  arrange(position) |>
  summarise(number_positions = n(),
            unique_positions = paste0(position, collapse = " ")) |>
  left_join(player_data |> select(player_id, full_name, total_matches), 
            by = "player_id") |>
  arrange(desc(number_positions), total_matches) |>
  View()

# unique jersey numbers
player_match_data |>
  count(player_id, number) |>
  rename(number_matches = n) |>
  group_by(player_id) |>
  arrange(number) |>
  summarise(n_numbers = n(),
            unique_numbers = paste0(number, collapse = " "),
            jersey_sum = sum(number)) |>
  left_join(player_data |> select(player_id, full_name), 
            by = "player_id") |>
  left_join(player_career_years,
            by = "player_id") |>
  left_join(player_career_summaries |> select(player_id, total_points, total_matches),
            by = "player_id") |>
  arrange(desc(n_numbers), total_matches) |>
  View()
