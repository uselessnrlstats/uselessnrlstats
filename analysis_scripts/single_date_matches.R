##### Description #####
# An R script to look at dates that have only had one match played

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
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
single_date_games <- match_data |>
  mutate(date_of_year = format(date, format = "%d/%m"),
         month = month(date),
         day = day(date)) |>
  group_by(date_of_year, day, month) |>
  summarise(n_matches = n(),
            .groups = "drop") |>
  arrange(month, day) |>
  filter(n_matches == 1) |>
  left_join(match_data |> mutate(date_of_year = format(date, format = "%d/%m")),
            by = "date_of_year") |>
  select(date_of_year, competition_year, round, home_team, home_team_score, away_team_score, away_team)

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
final_table <- single_date_games |>
  mutate(
    home_team_score_a = ifelse(home_team_score > away_team_score,
                             paste0("<b>", home_team_score, "</b>"),
                             home_team_score),
    away_team_score_a = ifelse(home_team_score < away_team_score,
                             paste0("<b>", away_team_score, "</b>"),
                             away_team_score),
    match_score = paste0(home_team_score_a, "\u2013", away_team_score_a)) |>
  select(-c(home_team_score, away_team_score, home_team_score_a, away_team_score_a)) |>
  relocate(match_score, .after = home_team) |>
  rename(
    Date = date_of_year,
    Year = competition_year,
    Round = round,
    `Home Team` = home_team,
    `Match Score` = match_score,
    `Away Team` = away_team,
  ) |>
  formattable(
    list(
      `Date` = bold_formatter,
      `Home Team` = team_formatter,
      `Away Team` = team_formatter
    ),
    align = c("r", "r", "l", "r", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Single Match Dates")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "Dates of the year to have only had one NRL/NSWRL match played"))
final_table

saveWidget(final_table, "tables/html/single_date_matches.html")
webshot(url = "tables/html/single_date_matches.html", 
        file = "tables/png/single_date_matches.png", 
        selector = "body", zoom = 4,
        vwidth = 700)
