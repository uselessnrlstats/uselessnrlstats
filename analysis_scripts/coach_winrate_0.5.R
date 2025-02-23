##### Description #####
# An R script to look at closest coach winrates to 0.5

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot)
}

##### Load Data #####
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
coach_match_data <- read_csv("cleaned_data/nrl/coach_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

coach_career_years <- coach_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(coach_id) |>
  summarise(first_year = min(year),
            last_year = max(year)) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))


match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, round, date, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, round, date, home_away, team, score_for, score_against, result)

##### Analysis #####
coach_win_rates <- coach_match_data |>
  select(match_id, coach_id, team) |>
  left_join(match_results, by = c("match_id", "team")) |>
  arrange(date) |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), 
            by = c("team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), 
            by = c("team_unique")) |>
  mutate(team_label = paste0('<span style="display: inline; padding: 3px 4px 3px 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', team_abbr, '</span>')) |>
  group_by(coach_id) |>
  summarise(
    total_matches = n(),
    total_wins = sum(result == "W"),
    total_draws = sum(result == "D"),
    total_losses = sum(result == "L"),
    win_rate = total_wins / total_matches,
    teams = paste0(unique(team_label), collapse = " "),
    .groups = "drop"
  ) |>
  mutate(record = paste0(total_wins, "-",
                         ifelse(total_draws > 0, paste0(total_draws, "-"), ""), 
                         total_losses)) |>
  left_join(coach_data |> select(coach_id, full_name), by = "coach_id") |>
  left_join(coach_career_years |> select(coach_id, last_year, career_years), by = "coach_id") |>
  filter(total_matches >= 50) |>
  mutate(total_matches = ifelse(last_year == 2024 & coach_id != 226, paste0(total_matches, "*"), total_matches) |> as.character()) |>
  arrange(abs(0.5 - win_rate)) |>
  select(career_years, full_name, teams, total_matches, record, win_rate)

##### Table Formatting #####
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
              font.weight = "bold",
              display = "block", 
              padding = "2px 4px 2px 4px"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat"
            ))

# Final table formatting
final_table <- coach_win_rates |>
  mutate(win_rate =  percent(round(win_rate, 3))) |>
  filter(row_number() <= 10) |>
  rename(
    Career = career_years,
    Coach = full_name,
    Teams = teams,
    Matches = total_matches, 
    `W-(D)-L` = record,
    `Win Rate` = win_rate
  ) |>
  formattable(
    list(
      Coach = bold_formatter,
      Matches = num_formatter,
      `W-(D)-L` = num_formatter,
      `Win Rate` = num_formatter
    ),
    align = c("l", "l", "c", "c", "c", "r"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(7) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "NSWRL/NRL Coaches with the closest win-rate to 50% (\u226550 games)"))
final_table

saveWidget(final_table, "tables/html/coach_win_rate.html")
webshot(url = "tables/html/coach_win_rate.html", 
        file = "tables/png/coach_win_rate.png", 
        selector = "div", zoom = 4,
        vwidth = 650)
