##### Description #####
# An R script to look at unique names scoring on debut

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

##### Helper Stats #####

##### Analysis #####
unique_names <- player_data |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  group_by(first_name) |>
  mutate(first_name_n = n()) |>
  ungroup() |>
  group_by(last_name) |>
  mutate(last_name_n = n()) |>
  ungroup() |>
  mutate(unique_name = (first_name_n == 1 & last_name_n == 1)) |>
  select(player_id, full_name, unique_name)

tries_on_debut <- player_match_data |>
  select(player_id, match_id, team, opposition_team, position, tries) |>
  left_join(match_data |> select(match_id, competition_year, round, date),
            by = "match_id") |>
  arrange(date) |>
  group_by(player_id) |>
  filter(row_number() == 1) |>
  ungroup() |>
  left_join(unique_names, by = "player_id") |>
  filter(tries > 0) |>
  filter(unique_name) |>
  select(competition_year, round, team, full_name, position, tries)

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
final_table <- tries_on_debut |>
  filter(row_number() %in% c(34, 36, 38, 39, 41:47)) |>
  mutate(
    round = gsub("Round (\\d+)", "\\1", round) |> as.numeric()
  ) |>
  rename(
    Year = competition_year,
    Round = round,
    Team = team,
    Player = full_name,
    Position = position,
    Tries = tries
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Player = bold_formatter,
      Tries = bold_formatter
    ),
    align = c("r", "c", "r", "r", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Unique Names Scoring on Debut (current players)")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "Current NRL players with unique first and last names that scored on debut"))
final_table

saveWidget(final_table, "tables/html/unique_try_debuts.html")
webshot(url = "tables/html/unique_try_debuts.html", 
        file = "tables/png/unique_try_debuts.png", 
        selector = "body", zoom = 4,
        vwidth = 750)
