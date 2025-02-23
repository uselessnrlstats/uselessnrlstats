##### Description #####
# An R script to look at team lists with the most letters

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
player_match_data |>
  select(match_id, team, player_id, number) |>
  left_join(player_data |> select(player_id, full_name),
            by = "player_id") |>
  mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |>
           str_to_upper() |>
           str_split(pattern = "")) |>
  group_by(match_id, team) |>
  summarise(letters_used = list(unique(unlist(full_name_letters))),
            .groups = "drop") |>
  rowwise() |>
  mutate(n_letters_used = length(letters_used)) |>
  arrange(desc(n_letters_used)) |>
  View()
