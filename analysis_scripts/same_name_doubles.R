##### Description #####
# An R script to look at players with the same name scoring doubles in the same round

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
same_name_doubles <- player_match_data |>
  mutate(tries = tries + penalty_tries) |>
  filter(tries >= 2) |>
  left_join(player_data |> select(player_id, first_name, full_name), by = "player_id") |>
  left_join(match_data |> select(match_id, competition_year, round), by = "match_id") |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "Rd\\1", round)
  )|>
  select(match_id, year, round, team, player_id, first_name, full_name, tries) |>
  rowwise() |>
  mutate(label = paste0(full_name, " (", tries, ")")) |>
  ungroup() |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  group_by(year, round, first_name) |>
  arrange(desc(tries)) |>
  summarise(
    n_players = n(),
    n_tries = sum(tries),
    players = paste0(label, collapse = ", "),
    .groups = "drop"
  ) |>
  filter(n_players >= 2)
  

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
