##### Description #####
# An R script to look at a ladder involving only bulldogs games since 2019

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(scales)
}

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
team_focus <- "Wests Tigers"
##### Analysis #####
match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  filter(year >= 2020) |>
  filter(home_team == team_focus | away_team == team_focus) |>
  select(competition_year, year, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(competition_year, year, round, home_away, team, score_for, score_against, result) |>
  filter(grepl("Round", round)) |>
  # Apply Points
  mutate(points = case_when(
    result == "W" ~ 2,
    result == "L" ~ 0,
    result == "D" ~ 1,
    result == "B" ~ 2,
    .default = 0
  )) |>
  group_by(team) |>
  summarise(points = sum(points, na.rm = TRUE),
            played = sum(result %in% c("W", "L", "D"), na.rm = TRUE),
            wins = sum(result == "W", na.rm = TRUE),
            losses = sum(result == "L", na.rm = TRUE),
            draws = sum(result == "D", na.rm = TRUE),
            score_for = sum(score_for, na.rm = TRUE),
            score_against = sum(score_against, na.rm = TRUE),
            score_diff = score_for - score_against,
            .groups = "drop") |>
  arrange(desc(points), desc(score_diff), desc(score_for), desc(score_against)) |>
  mutate(pos = row_number()) |>
  relocate(pos, .before = team)

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
