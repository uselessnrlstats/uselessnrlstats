##### Description #####
# An R script to look at games with lots of the same initials

##### Libraries #####
{
  library(ggtext)
  library(lubridate)
  library(RColorBrewer)
  library(readr)
  library(stringr)
  library(tidyverse)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")

##### Analysis #####
player_match_initials <- player_match_data |>
  left_join(player_data |> select(player_id, full_name, first_name, last_name),
            by = "player_id") |>
  mutate(first_name = ifelse(first_name == "?", NA, first_name)) |>
  mutate(first_name_initial = substr(first_name, 1, 1) |> 
           str_to_upper(),
         last_name_initial = substr(last_name, 1, 1) |> 
           str_to_upper(),
         initials = ifelse(is.na(first_name),
                           NA,
                           paste0(first_name_initial, last_name_initial)))

# Most same initials in team/game
player_match_initials |>
  filter(!is.na(initials)) |>
  count(match_id, last_name_initial) |>
  arrange(desc(n)) |>
  head(n = 10) |>
  left_join(match_data, by = "match_id")

player_match_initials |>
  select(player_id, match_id, team, number, position, first_name, last_name, full_name, initials) |>
  filter(match_id == 31215) |>
  View()

