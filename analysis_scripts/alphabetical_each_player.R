##### Description #####
# An R script to look at players who were the last left playing from their birth decade

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
matchups_by_team <- match_data |>
  select(match_id, home_team, away_team) |>
  distinct()

##### Analysis #####
player_match_ups <- player_match_data |>
  select(match_id, team, position, player_id) |>
  group_by(match_id, team, position) |>
  mutate(position_unique = case_when(
    row_number() > 1 ~ paste0(position, row_number()),
    .default = position
  )) |>
  ungroup() |>
  left_join(matchups_by_team, by = "match_id") |>
  mutate(team = case_when(
    team == home_team ~ "home_player",
    team == away_team ~ "away_player",
    .default = NA)) |>
  pivot_wider(id_cols = c(match_id, home_team, away_team, position, position_unique), 
              names_from = team, values_from = player_id) |>
  left_join(player_data |> select(player_id, full_name, last_name), by = c("home_player" = "player_id"))  |>
  rename(full_name_home = full_name, last_name_home = last_name) |>
  relocate(away_player, .after = last_name_home) |>
  left_join(player_data |> select(player_id, full_name, last_name), by = c("away_player" = "player_id"))  |>
  rename(full_name_away = full_name, last_name_away = last_name) |>
  mutate(
    last_name_home = ifelse(is.na(last_name_home), gsub(" (.+)$", "\\1", full_name_home), last_name_home),
    last_name_away = ifelse(is.na(last_name_away), gsub(" (.+)$", "\\1", full_name_away), last_name_away),
    home_before_away = (last_name_home > last_name_away))

player_match_ups |>

  filter(position != "B") |>
  group_by(match_id, home_team, away_team) |>
  summarise(total = sum(home_before_away),
            .groups = "drop") |>
  left_join(match_data |> select(match_id, competition_year, round), by = "match_id") |>
  relocate(c(competition_year, round), .after = "match_id") |>
  filter(total == 0 | total == 13) 



##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "2px 4px 2px 4px", 
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
final_table <- time_to_wait |>
  filter(wait_time >= ddays(10)) |>
  select(competition_year, team, season_opener_date, date, wait_time) |>
  mutate(season_opener_date = format(season_opener_date, "%d/%m"),
         date = format(date, "%d/%m")) |>
  rename(
    Season = competition_year,
    Team = team,
    `Season<br>Opener` = season_opener_date,
    `First<br>Match` = date,
    `Wait<br>Time` = wait_time
  ) |>
  formattable(
    list(
      Season = bold_formatter,
      Team = team_formatter,
      `Wait<br>Time` = num_bold_formatter
    ),
    align = c("r", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(2) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "<u>Sitting, Waiting, Wishing</u><br>Longest waits after the season opener for an NRL/NSWRL team to play their first match of the season" |> HTML()))
final_table

saveWidget(final_table, "tables/html/wait_to_play.html")
webshot(url = "tables/html/wait_to_play.html", 
        file = "tables/png/wait_to_play.png", 
        selector = "div", zoom = 4,
        vwidth = 570)
