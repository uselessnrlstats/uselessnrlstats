##### Description #####
# An R script to look at teams who have won in their year of the zodiac

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(showtext)
}

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")
ladder_data <- read_csv("cleaned_data/nrl/ladder_round_data.csv")
premiership_data <- read_csv("helper_stats/premiership_data.csv")

##### Analysis #####
# zodiac signs beginning from 1908
zodiac_animals <- tibble(
  animal = c("Monkey", "Rooster", "Dog", "Pig", "Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat"),
  remainder = 0:11
) |>
  mutate(
    NRL_team = case_when(
      animal == "Rooster" ~ "Sydney Roosters" |> list(),
      animal == "Dog" ~ "Canterbury Bankstown Bulldogs" |> list(),
      animal == "Tiger" ~ c("Balmain Tigers", "Wests Tigers") |> list(),
      animal == "Rabbit" ~ "South Sydney Rabbitohs" |> list(),
      animal == "Dragon" ~ c("St George Dragons", "St George Illawarra Dragons") |> list(),
      animal == "Horse" ~ "Brisbane Broncos" |> list(),
      .default = NA |> as.character() |> list()
    )
  )

zodiac_trophies <- premiership_data |>
  pivot_longer(cols = 3:5, names_to = "trophy", values_to = "team_name") |>
  left_join(team_data, by = "team_name") |>
  left_join(team_logos, by = "team_unique") |>
  mutate(remainder = year %% 12) |>
  left_join(zodiac_animals, by = "remainder") |>
  rename(team = team_unique) |>
  select(competition_year, year, trophy, team, animal, NRL_team) |>
  rowwise() |>
  filter(team %in% NRL_team) |>
  select(competition_year, year, animal, team, trophy) |>
  group_by(competition_year, year, animal, team) |>
  summarise(trophy = ifelse(n() == 2, "Premiership + MP", trophy),
            .groups = "drop") |>
  mutate(trophy = case_when(
    trophy %in% c("Premiership + MP", "Premiership") ~ paste(trophy, "&#x1F3C6;"),
    trophy == "Wooden Spoon" ~ paste(trophy, "&#x1F944;"),
    .default = trophy
  )) |>
  arrange(year)
  

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `background-color` = data.frame(team_name = x) |>
                left_join(team_data, by = "team_name") |>
                left_join(team_logos, by = "team_unique") |>
                pull(team_colour),
              color = data.frame(team_name = x) |>
                left_join(team_data, by = "team_name") |>
                left_join(team_logos, by = "team_unique") |>
                mutate(lum = luminance(team_colour),
                       text_col = ifelse(lum < 0.35, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- zodiac_trophies |>
  select(competition_year, animal, team, trophy) |>
  rename(
    Year = competition_year,
    Zodiac = animal,
    Team = team,
    Trophy = trophy
  ) |>
  formattable(
    list(
      Team = team_formatter
    ),
    align = c("r", "c", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Chinese Zodiac Trophies")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL teams that have won the Premiership, Minor Premiership or Wooden Spoon in their mascot's respective year of the Chinese zodiac."))
final_table

saveWidget(final_table, "tables/html/zodiac_trophies.html")
webshot(url = "tables/html/zodiac_trophies.html", 
        file = "tables/png/zodiac_trophies.png", 
        selector = "body", zoom = 4,
        vwidth = 600)
