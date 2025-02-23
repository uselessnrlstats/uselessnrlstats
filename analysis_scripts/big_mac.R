##### Description #####
# An R script to look at Mac and Mc lovers

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
player_match_data |>
  left_join(player_data |> select(player_id, full_name, first_name, last_name),
            by = "player_id") |>
  filter(str_detect(last_name, "Mac[A-Z]") |
           str_detect(last_name, "Mc[A-Z]")) |>
  group_by(match_id) |>
  summarise(count = n(),
            McPlayers = paste0(full_name, collapse = ", ")) |>
  left_join(match_data, by = "match_id") |>
  arrange(desc(count)) |>
  View()


