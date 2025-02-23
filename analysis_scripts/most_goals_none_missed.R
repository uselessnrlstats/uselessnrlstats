##### Description #####
# An R script to look at players who kicked at 100% on debut

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

##### Analysis #####
goalkicking_data <- player_match_data |>
  select(player_id, match_id, team, goals, goal_attempts) |>
  left_join(team_data |> select(team_name, team_abbr), by = c("team" = "team_name")) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  filter(!is.na(goal_attempts)) |>
  mutate(player_summary = paste0(full_name, " (", team_abbr, ") ", goals, "/", goal_attempts)) |>
  group_by(match_id) |>
  arrange(desc(goals)) |>
  summarise(
    goals = sum(goals),
    goal_attempts = sum(goal_attempts),
    goalkickers = paste0(player_summary, collapse = "<br>"),
    .groups = "drop"
  ) |>
  filter(goals == goal_attempts) |>
  left_join(match_data |> select(match_id, competition_year, round, date, home_team, home_team_score, away_team, away_team_score),
            by = "match_id") |>
  arrange(desc(goals), year(date), round) |>
  mutate(
    home_team_score_a = ifelse(home_team_score > away_team_score,
                               paste0("<b>", home_team_score, "</b>"),
                               home_team_score),
    away_team_score_a = ifelse(home_team_score < away_team_score,
                               paste0("<b>", away_team_score, "</b>"),
                               away_team_score),
    match_score = paste0(home_team_score_a, "\u2013", away_team_score_a)) |>
  filter(year(date) >= 1998) |>
  select(competition_year, round, home_team, match_score, away_team, goals, goalkickers)

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
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- goalkicking_data |>
  mutate(round = gsub("Round (\\d+)", "\\1", round)) |>
  filter(goals >= 13) |>
  rename(
    Year = competition_year,
    Round = round,
    `Home Team` = home_team,
    `Score` = match_score,
    `Away Team` = away_team,
    `Goals` = goals,
    `Goal-kickers` = goalkickers
  ) |>
  formattable(
    list(
      Goals = bold_formatter,
      `Home Team` = team_formatter,
      `Away Team` = team_formatter
    ),
    align = c("r", "c", "r", "c", "l", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 1px  !important;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "<b>Most Goals at 100%:</b> NRL era (1998-) matches with the most goals kicked by both teams without a miss" |> HTML()))
final_table

saveWidget(final_table, "tables/html/goals_no_misses.html")
webshot(url = "tables/html/goals_no_misses.html", 
        file = "tables/png/goals_no_misses.png", 
        selector = "body", zoom = 4,
        vwidth = 840)
