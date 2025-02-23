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
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
coach_match_data <- read_csv("cleaned_data/nrl/coach_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
player_match_data |>
  group_by(match_id, team) |>
  summarise(
    jerseys = list(c(number)),
    jerseys_print = paste(number, collapse = ","),
    .groups = "drop"
  ) |>
  left_join(match_data |> select(match_id, competition_year, round)) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  left_join(coach_match_data, by = c("match_id", "team")) |>
  left_join(coach_data |> select(coach_id, full_name),
            by = "coach_id") |>
  filter(year >= 2017) |>
  rowwise() |>
  mutate(as_named = (all(jerseys == 1:length(jerseys)))) |>
  ungroup() |>
  select(-competition_year) |>
  group_by(team) |>
  summarise(matches = n(),
            n_1_to_17 = sum(as_named),
            perc = sum(as_named) / n()) |>
  arrange(perc) |>
  filter(matches > 24) |>
  View()
  

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
