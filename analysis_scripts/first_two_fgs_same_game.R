##### Description #####
# An R script to look at players whokicked their first two field goals in the same game

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
# Match Results for each team
match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, team, score_for, score_against, result)

##### Analysis #####
two_fgs <- player_match_data |>
  select(player_id, match_id, team, opposition_team, position, field_goals, field_goals2) |>
  left_join(match_data |> select(match_id, competition, competition_year, round, date),
            by = "match_id") |>
  left_join(match_results, by = c("match_id", "team")) |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(match_no = row_number()) |>
  filter((field_goals + field_goals2) > 0) |>
  filter(row_number() == 1) |>
  ungroup() |>
  left_join(player_data |> select(player_id, full_name),
            by = "player_id") |>
  arrange(date) |>
  filter((field_goals + field_goals2) > 1) |>
  select(competition_year, round, team, opposition_team, score_for, score_against, full_name, position, field_goals, match_no)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `vertical.align` = "center",
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

number_formatter <-
  formatter("span",
            style = x ~ style(
              font.size = "12px"
            ))

# Final table formatting
final_table <- two_fgs |>
  mutate(
    round = gsub("Round (\\d+)", "\\1", round),
    score_for_a = ifelse(score_for > score_against,
                         paste0("<b>", score_for, "</b>"),
                         score_for),
    score_against_a = ifelse(score_for < score_against,
                             paste0("<b>", score_against, "</b>"),
                             score_against),
    match_score = paste0(score_for_a, "\u2013", score_against_a)) |>
  select(-c(score_for, score_against, score_for_a, score_against_a)) |>
  relocate(match_score, .after = team) |>
  rename(
    Year = competition_year,
    Rd = round,
    Team = team,
    vs = match_score,
    Opposition = opposition_team,
    Player = full_name,
    `Pos.` = position,
    FGs = field_goals,
    `Match #` = match_no
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Opposition = team_formatter,
      Player = bold_formatter,
      FGs = bold_formatter
    ),
    align = c("r", "c", "r", "c", "l", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica; vertical-align: middle"'
  )  |>
  as.htmlwidget() |>
  prependContent(tags$style("table td:nth-of-type(9) { width: 100px;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 10px;",
                    "First 2+ field goals in the same match")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 10px;",
                    "NRL/NSWRL players to have kicked their first two career field goals in the one match"))
final_table

saveWidget(final_table, "tables/html/two_fgs.html")
webshot(url = "tables/html/two_fgs.html", 
        file = "tables/png/two_fgs.png", 
        selector = "body", zoom = 4,
        vwidth = 900)
