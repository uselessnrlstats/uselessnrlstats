##### Description #####
# An R script to look at tries by wings/fbs in the first 3 rounds of the season

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
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")
ladder_data <- read_csv("cleaned_data/nrl/ladder_round_data.csv")

##### Helper Stats #####
first_three_matches <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, date, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, date, round, home_away, team, score_for, score_against, result) |>
  group_by(team, competition_year) |>
  arrange(date) |>
  filter(row_number() <= 3) |>
  select(match_id, competition_year, round, date, team) |>
  mutate(first_three = TRUE) |>
  ungroup()
  
##### Analysis #####
player_match_data |>
  left_join(player_data |> select(player_id, full_name),
            by = "player_id") |>
  left_join(first_three_matches, by = c("match_id", "team")) |>
  mutate(first_three = replace_na(first_three, FALSE)) |>
  filter(first_three) |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "\\1", round) |> as.numeric()
  ) |>
  group_by(competition_year, year, team) |>
  filter(tries > 0) |>
  summarise(tries = sum(tries),
            positions = list(sort(unique(position))),
            n_positions = length(unique(position)),
            scorers = list(sort(unique(full_name))),
            n_scorers = length(unique(player_id)),
            .groups = "drop") |>
  filter(n_scorers < 4, year >= 1998) |>
  ungroup() |>
  arrange(desc(tries)) |> View()
  

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
