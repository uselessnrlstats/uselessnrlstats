##### Description #####
# An R script to look at rounds where the first game and last game had th same score

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
match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  rowwise() |>
  mutate(result = paste0(max(c(home_team_score, away_team_score)), "-", min(c(home_team_score, away_team_score)))) |>
  ungroup() |>
  left_join(team_data |> select(team_name, team_abbr), by = c("home_team" = "team_name")) |>
  rename(home_team_abbr = team_abbr) |> relocate(home_team_abbr, .after = home_team) |>
  left_join(team_data |> select(team_name, team_abbr), by = c("away_team" = "team_name")) |>
  rename(away_team_abbr = team_abbr) |> relocate(away_team_abbr, .after = away_team) |>
  select(match_id, competition_year, year, round, date, time, home_team, home_team_abbr, home_team_score, away_team, away_team_abbr, away_team_score, result)
  
##### Analysis #####
first_and_last <- match_results |>
  filter(grepl("Round", round)) |>
  arrange(date, time) |>
  group_by(competition_year, round) |>
  filter(row_number() %in% c(1, max(row_number()))) |>
  filter(result[1] == result[2]) |>
  mutate(match_no = c("First match:", "Last match:")) |>
  select(competition_year, round, match_no, date, time, home_team, home_team_score, away_team, away_team_score, result) |>
  filter((date[1] != date[2]) | (time[1] != time[2]))

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
final_table <- first_and_last |>
  mutate(round = gsub("Round (\\d+)", "\\1", round) |> as.numeric(),
         score = paste0(home_team_score, "-", away_team_score)) |>
  group_by(competition_year, round) |>
  mutate(competition_year = ifelse(row_number() == 2, "", competition_year),
         round = ifelse(row_number() == 2, "", round)) |>
  ungroup() |>
  select(competition_year, round, match_no, home_team, score, away_team) |>
  rename(
    `Year` = competition_year,
    `Round` = round,
    `Match #` = match_no,
    `Home Team` = home_team,
    `vs` = score,
    `Away Team` = away_team
  ) |>
  formattable(
    list(
      `Home Team` = team_formatter,
      `Away Team` = team_formatter,
      vs = bold_formatter
    ),
    align = c("r", "l", "r", "r", "c", "l"),
    table.attr = 'style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("table tr:nth-of-type(2n-1) td {border-top: 2px solid #ccc;}")) |>
  prependContent(tags$style("table tr:nth-of-type(4n) td {border-top: 0.5px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(4n-2) td {border-top: 0.5px solid #f1f1f1;}")) |>
  prependContent(tags$style("table tr:nth-of-type(4n-2) td {background-color: #f1f1f1;}")) |>
  prependContent(tags$style("table tr:nth-of-type(4n-3) td {background-color: #f1f1f1;}"))
final_table

saveWidget(final_table, "tables/html/round_score_first_last.html")
webshot(url = "tables/html/round_score_first_last.html",
        file = "tables/png/round_score_first_last.png",
        selector = "body", zoom = 4,
        vwidth = 750)
