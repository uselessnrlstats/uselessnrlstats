##### Description #####
# An R script to look at matches where the halftime or fulltime score have been
# equal to the date as month-day

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

##### Analysis #####
ha_date_matches <- match_data |>
  mutate(date_of_year = format(date, format = "%m-%d"),
         month = month(date),
         day = day(date)) |>
  filter(month == home_team_score & day == away_team_score + 1) |>
  arrange(date) |>
  select(competition_year, round, date, date_of_year, home_team, home_team_score, away_team, away_team_score)

match_data |>
  mutate(date_of_year = format(date, format = "%d-%m"),
         month = month(date),
         day = day(date)) |>
  filter(day == home_team_ht_score & month == away_team_ht_score) |>
  arrange(date) |>
  select(competition_year, round, date, date_of_year, home_team, home_team_ht_score, away_team, away_team_ht_score)

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
  mutate(date_of_year = format(date, format = "%m/%d"),
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
                    "Match score = Match date (American)")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL matches where the full-time score (written as H-A) is the American date the match was played (mm/dd)"))
final_table

saveWidget(final_table, "tables/html/score_dates2.html")
webshot(url = "tables/html/score_dates2.html",
        file = "tables/png/score_dates2.png", 
        selector = "body", zoom = 4,
        vwidth = 730)
