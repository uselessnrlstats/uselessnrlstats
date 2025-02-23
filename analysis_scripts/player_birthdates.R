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
###### Birthdates ######
player_data |>
  group_by(birthdate) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  drop_na() |>
  View()

###### Birth months ######
player_data |>
  mutate(birthmonth = as.numeric(month(birthday))) |>
  group_by(birthmonth) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  drop_na() |>
  ggplot() +
  geom_bar(aes(x = birthmonth, y = count), stat = "identity") +
  theme_classic() +
  scale_x_continuous(breaks = 1:12)

###### Birth days ######
player_data |>
  mutate(birth_day = as.numeric(day(birthday))) |>
  group_by(birth_day) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  drop_na() |>
  ggplot() +
  geom_bar(aes(x = birth_day, y = count), stat = "identity") +
  theme_classic() +
  scale_x_continuous(breaks = 1:31)


###### Team of birthday ######
player_data |>
  filter(year(birthday) > 1980) |>
  group_by(birthdate) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  drop_na() |>
  View()

player_data |>
  filter(year(birthday) > 1980) |>
  filter(birthdate == "20-03")
