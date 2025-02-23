##### Description #####
# An R script to look at player win-rates after 250 games

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

##### Helper Stats #####
matchups_by_team <- player_match_data |>
  select(match_id, team, opposition_team) |>
  distinct()

match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, date, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  left_join(matchups_by_team, by = c("match_id", "team")) |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, date, round, home_away, team, opposition_team, score_for, score_against, result)

player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year)) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

##### Analysis #####
win_rates <- player_match_data |>
  select(player_id, match_id, team) |>
  left_join(match_results |> select(match_id, date, team, result), 
            by = c("match_id", "team")) |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(
    match_no = row_number(),
    wins = cumsum(result == "W"),
    draws = cumsum(result == "D"),
    losses = cumsum(result == "L"),
    win_rate = wins / match_no,
    final_win_rate = max(wins) / max(match_no)) |>
  rowwise() |>
  mutate(record = paste0(c(wins, draws, losses), collapse = "-")) |>
  ungroup() |>
  filter(match_no == 250) |>
  arrange(win_rate) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  left_join(player_career_years |> select(player_id, career_years), by  ="player_id") |>
  select(career_years, full_name, team, match_no, record, win_rate, final_win_rate)

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
final_table <- rbind(
  tibble(year = "2024", round = "Rd2", match_summary = "2024 Rd16 v MEL ?-?", full_name = "Tevita Pangai Junior", team_from = "Canterbury Bankstown Bulldogs", team_to = "Dolphins", last_win_summary ="2023 Rd22 v DOL 23-22", n_losses = 3),
  last_team_they_beat) |>
  filter(row_number() <= 16) |>
  select(full_name, team_from, last_win_summary, n_losses, team_to, match_summary) |>
  rename(
    Player = full_name,
    `Team From` = team_from,
    `Last Win` = last_win_summary,
    `# Losses Since` = n_losses,
    `Team To` = team_to,
    `Next Game` = match_summary
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `Team From` = team_formatter,
      `Team To` = team_formatter
    ),
    align = c("r", "r", "l", "c", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 10px;",
                    "Joining the Last Team they Beat")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 10px;",
                    "The last 15 players to win a match, lose 3+ matches straight, and then join that team they last beat." |> HTML()))
final_table

saveWidget(final_table, "tables/html/last_team_they_beat.html")
webshot(url = "tables/html/last_team_they_beat.html", 
        file = "tables/png/last_team_they_beat.png", 
        selector = "body", zoom = 4,
        vwidth = 1050)
