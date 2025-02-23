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
team_list_data <- player_match_data |>
  group_by(match_id, team) |>
  summarise(
    players = list(c(player_id)[1:17]),
    .groups = "drop"
  ) |>
  left_join(match_data |> select(match_id, date)) |>
  left_join(team_data |> select(team_name, team_unique), by = c("team" = "team_name")) |>
  select(-team) |> relocate(team_unique, .after = "match_id") |>
  arrange(date) |>
  filter(year(date) >= 2001) |>
  rowwise() |>
  mutate(players_print = paste(players, collapse = ",")) |>
  ungroup()

unchanged_matches <- team_list_data |>
  split(team_list_data$team_unique) |>
  map(.f = function(df) {
    df |>
      mutate(lag_players = lag(players),
             identical_17 = (players_print == lag(players_print)) |> replace_na(FALSE)) |>
      rowwise() |>
      mutate(same_17 = (all(players %in% lag_players) & all(lag_players %in% players))) |>
      ungroup() |>
      select(-lag_players)
  }) |>
  list_rbind() |>
  group_by(match_id) |>
  filter(all(identical_17)) |>
  pull(match_id) |>
  unique()

match_data |>
  filter(match_id %in% unchanged_matches) |>
  select(competition_year, round, date, home_team, away_team) |>
  arrange(date)
  

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
