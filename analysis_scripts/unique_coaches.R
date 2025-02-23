##### Description #####
# An R script to look at players who have played under the most coaches

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
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
coach_match_data <- read_csv("cleaned_data/nrl/coach_match_data.csv")

##### Analysis #####
player_match_data |>
  left_join(coach_match_data, by = c("match_id", "team"), relationship = "many-to-many") |>
  group_by(player_id, coach_id) |>
  summarise(matches = n()) |>
  filter(!is.na(coach_id)) |>
  ungroup() |>
  left_join(coach_data |> select(coach_id, full_name), by = "coach_id") |>
  group_by(player_id) |>
  arrange(desc(matches)) |>
  summarise(n_coaches = n(),
            unique_coaches = paste0(full_name, collapse = ", ")) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  relocate(full_name, .after = player_id) |>
  arrange(desc(n_coaches)) |>
  View()
