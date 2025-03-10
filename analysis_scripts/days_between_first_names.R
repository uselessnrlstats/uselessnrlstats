##### Description #####
# An R script to look at rounds where teams had a points diff of 0

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot)
  library(scales)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
time_years_days <- function(start_date, end_date) {
  year_span <- interval(start_date, end_date) |>
    time_length("years") |>
    floor()
  day_span <- interval(start_date %m+% lubridate::years(year_span), end_date) |>
    time_length("days") |>
    as.numeric()
  time_span <- paste0(year_span, "yrs ", day_span, " days")
  return(time_span)
}

##### Analysis #####
time_between_first_name <- player_match_data |>
  select(player_id, match_id, team) |>
  left_join(player_data |> select(player_id, first_name, full_name), by = "player_id") |>
  left_join(match_data |> select(match_id, competition_year, round, date), 
            by = "match_id") |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  group_by(first_name) |>
  arrange(date) |>
  mutate(day_gap = ifelse(row_number() == 1,
                          NA,
                          difftime(date, lag(date), units = "days")),
         time_gap = ifelse(is.na(day_gap),
                           NA,
                           time_years_days(lag(date), date))) |>
  mutate(prev_player = lag(full_name),
         prev_player_id = lag(player_id),
         prev_team = lag(team),
         prev_date = lag(date)) |>
  ungroup() |>
  filter(!is.na(day_gap)) |>
  filter(player_id != prev_player_id) |>
  select(first_name, prev_date, prev_team, prev_player, date, team, full_name, time_gap, day_gap) |>
  arrange(desc(day_gap))

time_between_first_name |>
  filter(first_name == "Owen")


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
final_table <- points_diff_twice |>
  mutate(
    ladder_position = ordinal(ladder_position),
    record = paste(wins, draws, losses, sep = "-")) |>
  group_by(competition_year, round) |>
  mutate(competition_year = ifelse(row_number() == 2, "", competition_year),
         round = ifelse(row_number() == 2, "", round)) |>
  ungroup() |>
  select(competition_year, round, team, ladder_position, points, played, record, byes, score_for, score_against, score_diff) |>
  rename(
    `Year` = competition_year,
    Rd = round,
    `Team` = team,
    `Pos.` = ladder_position,
    Pts = points,
    `Pld` = played,
    `W-D-L` = record,
    B = byes,
    PF = score_for,
    PA = score_against,
    PD = score_diff
  ) |>
  formattable(
    list(
      `Team` = team_formatter,
      PD = bold_formatter
    ),
    align = c("r", "r", "r", "r", "r", "r", "c", "c", "r", "r", "c"),
    table.attr = 'style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("table tr:nth-of-type(2n-1) td {border-top: 2px solid #ccc;}")) |>
  prependContent(tags$style("table tr:nth-of-type(4n-2) td {border-top: 0.5px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(4n) td {border-top: 0.5px solid #f5f5f5;}")) |>
  prependContent(tags$style("table tr:nth-of-type(4n-1) td {background-color: #f5f5f5;}")) |>
  prependContent(tags$style("table tr:nth-of-type(4n) td {background-color: #f5f5f5;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Multiple Teams with Points Difference of 0")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL Rounds where two or more teams have finished with points differences of 0 each"))
final_table

saveWidget(final_table, "tables/html/points_diff_0.html")
webshot(url = "tables/html/points_diff_0.html", 
        file = "tables/png/points_diff_0.png", 
        selector = "body", zoom = 4,
        vwidth = 700)
