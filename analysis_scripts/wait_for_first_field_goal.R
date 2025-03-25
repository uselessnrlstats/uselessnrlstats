##### Description #####
# An R script to look at which year had the mos forwards as captains

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
  library(slider)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")
  
##### Helper Stats #####

##### Analysis #####
match_numbers <- match_data |>
  arrange(date, time) |>
  group_by(competition_year) |>
  mutate(match_no = row_number()) |>
  select(match_id, competition_year, round, match_no)

player_match_data |>
  select(match_id, player_id, team, field_goals, field_goals2) |>
  filter(field_goals > 0 | field_goals2 > 0) |>
  left_join(match_numbers, by = "match_id") |>
  left_join(player_data |> select(player_id, full_name)) |>
  group_by(competition_year) |>
  filter(match_no == min(match_no)) |>
  select(competition_year, round, match_no, team, full_name) |>
  arrange(desc(match_no)) |>
  View()

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "4px 4px", 
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
              font.weight = "bold",
              display = "block"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat"
            ))

num_bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat",
              font.weight = "bold"
            ))

# Final table formatting
final_table <- beat_prems_after_50 |>
  ungroup() |>
  left_join(team_logos |> select(team_unique, team_colour), by = c("opposition_team" = "team_unique")) |>
  mutate(opposition_team = paste0('<span style="display: block; padding: 2px 4px 2px 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', opposition_team, '</span>')) |>
  select(-team_colour) |>
  left_join(team_logos |> select(team_unique, team_colour), by = c("opposition_team_next" = "team_unique")) |>
  mutate(opposition_team_next = paste0('<span style="display: block; padding: 2px 4px 2px 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', opposition_team_next, '&#x1F3C6;</span>')) |>
  rowwise() |>
  mutate(
    rounds = paste0(c(round, round_next), collapse = '<p style="line-height:5px; margin:0px;"><br></p>'),
    match_score = paste0("<span style='color: #FF3030'>", match_score, "</span>"),
    match_score_next = paste0("<span style='color: #008B00'>", match_score_next, "</span>"),
    match_scores = paste0(c(match_score, match_score_next), collapse = '<p style="line-height:5px; margin:0px;"><br></p>'),
    match_scores = paste0("<b style='font-size: 13px; font-family: Montserrat;'>", match_scores, "</b>"),
    opposition_teams = paste0(c(opposition_team, opposition_team_next), collapse = '<p style="line-height:5px; margin:0px;"><br></p>'),
  ) |>
  select(year, team, rounds, match_scores, opposition_teams) |>
  rename(
    Year = year,
    Team = team,
    Round = rounds,
    Score = match_scores,
    Opposition = opposition_teams
  ) |>
  formattable(
    list(
      Team = team_formatter
    ),
    align = c("c", "r", "c", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(3) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "<u>CHANGING FORTUNES</u><br>NRL/NSWRL teams to <span style='color: #FF3030'>concede 50+ points</span>, and then <span style='color: #008B00'>defeat the reigning premiers</span> in their next match (same season)" |> HTML()))
final_table

saveWidget(final_table, "tables/html/concede_50_beat_premiers.html")
webshot(url = "tables/html/concede_50_beat_premiers.html", 
        file = "tables/png/concede_50_beat_premiers.png", 
        selector = "div", zoom = 4,
        vwidth = 580)
