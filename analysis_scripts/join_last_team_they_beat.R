##### Description #####
# An R script to look at players who kicked increasing numbers of goals

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
  select(match_id, competition_year, year, round, home_away, team, score_for, score_against, result)

##### Analysis #####
last_team_they_beat <- player_match_data |>
  left_join(match_results |> select(match_id, team, score_for, score_against, result), by = c("match_id", "team")) |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("team" = "team_name")) |>
  left_join(team_data |> select(team_name, team_unique, team_abbr) |> rename(opp_team_unique = team_unique, opp_team_abbr = team_abbr), by = c("opposition_team" = "team_name")) |>
  left_join(match_data, by = "match_id") |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year),
    round = gsub("Round (\\d+)", "Rd\\1", round),
    round = case_when(
      round %in% c("Prelim Final", "Prelim Playoff", "Major Prelim Semi", "Minor Prelim Semi", "Minor Prelim", "Major Prelim") ~ "PF",
      round %in% c("Semi Final", "Minor Semi", "Major Semi", "Minor Semi Rep.") ~ "SF",
      round %in% c("Qualif Final", "Qualifier", "Minor Qualif", "Major Qualif") ~ "QF",
      round %in% c("Elim Qualif", "Elim Prelim Final") ~ "EF",
      round %in% c("Grand Final", "Final", "Grand Final Rep.") ~ "GF",
      round %in% c("Grand Final Chall.") ~ "GFC",
      round %in% c("Playoff") ~ "PO",
      .default = round
    )) |>
  select(player_id, match_id, year, round, date, team_unique, team_abbr, opp_team_unique, opp_team_abbr, score_for, score_against, result) |>
  arrange(date) |>
  mutate(match_summary = paste0(year, " ", round, " ", " v ", opp_team_abbr, " ", score_for, "-", score_against)) |>
  group_by(player_id) |>
  mutate(
    last_win_team = ifelse(lag(result) == "W", lag(team_unique), NA),
    last_win_opp = ifelse(lag(result) == "W", lag(opp_team_unique), NA),
    last_win_opp_abbr = ifelse(lag(result) == "W", lag(opp_team_abbr), NA),
    last_win_summary = ifelse(lag(result) == "W", lag(match_summary), NA)) |>
  fill(c(last_win_team, last_win_opp, last_win_opp_abbr, last_win_summary), .direction = "down") |>
  arrange(player_id, date) |>
  group_by(player_id, last_win_summary) |>
  mutate(n_losses = ifelse(lag(result) == "W", 1, 1:n()) |> replace_na(1)) |>
  ungroup() |>
  group_by(player_id) |>
  filter(team_abbr == last_win_opp_abbr) |>
  filter(row_number() == 1, n_losses >= 3) |>
  ungroup() |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  rename(
    team_from = last_win_team,
    team_to = team_unique
  ) |>
  arrange(desc(date)) |>
  select(year, round, match_summary, full_name, team_from, team_to, last_win_summary, n_losses)


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
