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
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
tries_by_game <- player_match_data |>
  group_by(match_id, team) |>
  summarise(n_tries = sum(tries + penalty_tries),
            .groups = "drop")

current_teams <- match_data |>
  filter(competition_year == "NRL 2023") |>
  distinct(away_team) |>
  rename(team_name = away_team) |>
  left_join(team_data |> select(team_name, team_unique, team_mascots),
            by = c("team_name")) |>
  left_join(team_logos, by = "team_unique") |>
  arrange(team_unique)

##### Analysis #####
points_ladder <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  filter(year >= 2007) |>
  left_join(tries_by_game, by = c("match_id", "home_team" = "team")) |>
  rename(home_team_tries = n_tries) |>
  left_join(tries_by_game, by = c("match_id", "away_team" = "team")) |>
  rename(away_team_tries = n_tries) |>
  filter(home_team_tries == away_team_tries) |>
  select(competition_year, year, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  left_join(team_data |> select(team_name, team_unique),
            by = c("team" = "team_name")) |>
  select(competition_year, year, round, home_away, team_unique, score_for, score_against, result) |>
  filter(grepl("Round", round)) |>
  # Apply Points
  mutate(points = case_when(
    result == "W" ~ 2,
    result == "L" ~ 0,
    result == "D" ~ 1,
    result == "B" ~ 2,
    .default = 0
  )) |>
  group_by(team_unique) |>
  summarise(points = sum(points, na.rm = TRUE),
            played = sum(result %in% c("W", "L", "D"), na.rm = TRUE),
            wins = sum(result == "W", na.rm = TRUE),
            losses = sum(result == "L", na.rm = TRUE),
            draws = sum(result == "D", na.rm = TRUE),
            score_for = sum(score_for, na.rm = TRUE),
            score_against = sum(score_against, na.rm = TRUE),
            score_diff = score_for - score_against,
            .groups = "drop") |>
  filter(team_unique %in% current_teams$team_unique) |>
  mutate(win_perc = percent(round(wins / played, 4))) |>
  arrange(desc(win_perc), desc(points), desc(score_diff), desc(score_for), desc(score_against)) |>
  mutate(pos = row_number()) |>
  select(pos, team_unique, played, win_perc, wins, losses, draws, score_diff, points)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `background-color` = data.frame(team_unique = x) |>
                left_join(team_logos, by = "team_unique") |>
                pull(team_colour),
              color = data.frame(team_unique = x) |>
                left_join(team_logos, by = "team_unique") |>
                mutate(lum = luminance(team_colour),
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

xnormalize <- function(x) {
  x <- c(x, 0, 100)
  normalize(x)[1:(length(x) - 2)]
}

pd_formatter <-
  formatter("span", style = function(x) {
    colours <- gsub("%", "", x) |>
      as.numeric() |>
      xnormalize() |>
      colorRamp(c("firebrick1",'white', "green4"))() |>
      as.integer() |>
      matrix(byrow = TRUE, dimnames = list(c("red", "green", "blue"), NULL), nrow = 3) |>
      csscolor()
    style(display = "block",
          'text-align' = 'center',
          padding = "0 4px", 
          `border-radius` = "4px",
          color = ifelse(luminance(colours) < 0.35, "#F2F2F2", "#1A1A1A"),
          `background-color` = colours,
          font.weight = "bold")
  })

# Final table formatting
final_table <- points_ladder |>
  mutate(
    pos = ordinal(pos),
    record = paste(wins, draws, losses, sep = "-")) |>
  select(pos, team_unique, points, played, record, score_diff, win_perc) |>
  rename(
    `Pos.` = pos,
    `Team` = team_unique,
    Pts = points,
    `Pld` = played,
    `W-D-L` = record,
    PD = score_diff,
    `Win %` = win_perc
  ) |>
  formattable(
    list(
      `Team` = team_formatter,
      `Win %` = pd_formatter
    ),
    align = c("r", "r", "c", "c", "c", "r", "r"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Equal Tries Ladder")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL Ladder from 2007-2024 with only matches where both teams scored the same number of tries"))
final_table

saveWidget(final_table, "tables/html/ladder_equal_tries.html")
webshot(url = "tables/html/ladder_equal_tries.html", 
        file = "tables/png/ladder_equal_tries.png", 
        selector = "body", zoom = 4,
        vwidth = 700)
