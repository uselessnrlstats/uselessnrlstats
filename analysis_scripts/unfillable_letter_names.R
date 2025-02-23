##### Description #####
# An R script to look at players whose names are made entirely of letters you can't colour in

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
open_letters_lower <- c("c", "f", "h", "i", "j", "k", "l", "m", "n", "r", "s", "t", "u", "v", "w", "x", "y", "z")
open_letters_upper <- c("C", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "S", "T", "U", "V", "W", "X", "Y", "Z")
open_letters <- c(open_letters_lower, open_letters_upper)

##### Helper Stats #####
player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year)) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

##### Analysis #####
not_coloured_in <- player_data |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  select(player_id, full_name, first_name, last_name) |>
  mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |> 
           str_split(pattern = ""),
         full_name_upper = gsub("[^a-zA-Z]", "", full_name) |> 
           str_to_upper() |>
           str_split(pattern = "")) |>
  rowwise() |>
  mutate(
    n_letters = length(full_name_letters),
    full_name_open = all(full_name_letters %in% open_letters),
    full_name_open_upper = all(full_name_upper %in% open_letters)
  ) |>
  filter(full_name_open | full_name_open_upper) |>
  arrange(desc(n_letters)) |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id")

coloured_in <- player_data |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  select(player_id, full_name, first_name, last_name) |>
  mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |> 
           str_split(pattern = ""),
         full_name_upper = gsub("[^a-zA-Z]", "", full_name) |> 
           str_to_upper() |>
           str_split(pattern = "")) |>
  rowwise() |>
  mutate(
    n_letters = length(full_name_letters),
    full_name_open = any(full_name_letters %in% open_letters),
    full_name_open_upper = any(full_name_upper %in% open_letters)
  ) |>
  arrange(desc(n_letters)) |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id")

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
final_table <- ha_date_matches |>
  mutate(date_of_year = format(date, format = "%d/%m"),
         match_score = paste0(home_team_score, "\u2013", away_team_score)) |>
  select(-c(date, home_team_score, away_team_score)) |>
  relocate(match_score, .after = home_team) |>
  rename(
    Year = competition_year,
    Round = round,
    Date = date_of_year,
    `Home Team` = home_team,
    `Away Team` = away_team,
    `Match Score` = match_score
  ) |>
  formattable(
    list(
      `Date` = bold_formatter,
      `Match Score` = bold_formatter,
      `Home Team` = team_formatter,
      `Away Team` = team_formatter
    ),
    align = c("r", "r", "c", "r", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Match score = Match date")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL matches where the full-time score (written as H-A) is the date the match was played (dd/mm)"))
final_table

saveWidget(final_table, "tables/html/score_dates1.html")
webshot(url = "tables/html/score_dates1.html", 
        file = "tables/png/score_dates1.png", 
        selector = "body", zoom = 4,
        vwidth = 730)
