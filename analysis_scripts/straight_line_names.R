##### Description #####
# An R script to look at players whose names are made entirely of straight lines

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
straight_lines_lower <- c("i", "k", "l", "t", "v", "w", "x", "y", "z")
straight_lines_upper <- c("A", "E", "F", "H", "I", "K", "L", "M", "N", "T", "V", "W", "X", "Y", "Z")
straight_lines <- c(straight_lines_lower, straight_lines_upper)

##### Analysis #####
straight_lines_dataset <- player_data |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  select(full_name) |>
  mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |> 
           str_split(pattern = ""),
         full_name_upper = gsub("[^a-zA-Z]", "", full_name) |> 
           str_to_upper() |>
           str_split(pattern = "")) |>
  rowwise() |>
  mutate(
    n_letters = length(full_name_letters),
    full_name_straight = all(full_name_letters %in% straight_lines),
    full_name_straight_upper = all(full_name_upper %in% straight_lines)
  ) |>
  filter(full_name_straight | full_name_straight_upper)

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
