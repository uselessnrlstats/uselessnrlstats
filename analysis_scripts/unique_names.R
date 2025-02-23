##### Description #####
# An R script to look at unique player names

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")

##### Analysis #####
###### Unique First Names ######
player_data |>
  filter(nchar(first_name) > 1) |>
  group_by(first_name) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  View()

###### Unique Last Names ######
player_data |>
  filter(nchar(last_name) > 1) |>
  group_by(last_name) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  View()

###### Unique Full Names ######
player_data |>
  filter(nchar(first_name) > 1) |>
  group_by(full_name) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  filter(count > 1) |>
  View()
