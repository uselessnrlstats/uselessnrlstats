##### Description #####
# An R script to look at the jersey numbers with most players in a year

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
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
all_numbers_df <- player_match_data |>
  left_join(player_data |> select(player_id, full_name),
            by = "player_id") |>
  left_join(match_data |> select(match_id, competition, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(competition, year, team, number) |>
  summarise(unique_n = length(unique(player_id)),
            unique_players = paste0(unique(full_name), collapse = ", ")) |>
  arrange(desc(unique_n))

all_numbers_df |>
  filter(competition == "NRL") |>
  group_by(number) |>
  arrange(number, desc(unique_n)) |>
  mutate(max_n = max(unique_n)) |>
  ungroup() |>
  filter(unique_n == max_n, unique_n > 1) |>
  arrange(number, year) |>
  View()
