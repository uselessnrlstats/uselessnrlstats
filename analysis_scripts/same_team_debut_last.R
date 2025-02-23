##### Description #####
# An R script to look at players who debuted and finished their career against the same team

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
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
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
         margin = score_for - score_against,
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, date, round, home_away, team, opposition_team, score_for, score_against, margin, result)

match_summary <- match_data |>
  select(match_id, competition_year, round, date, home_team, away_team, home_team_score, away_team_score) |>
  left_join(team_data |> select(team_name, team_abbr), by = c("home_team" = "team_name")) |>
  rename(home_team_abbr = team_abbr) |>
  left_join(team_data |> select(team_name, team_abbr), by = c("away_team" = "team_name")) |>
  rename(away_team_abbr = team_abbr) |>
  mutate(teams = paste0(home_team_abbr, "\u2013", away_team_abbr),
         round = gsub("Round (\\d+)", "R\\1", round),
         round = case_when(
           round %in% c("Prelim Final", "Prelim Playoff", "Major Prelim Semi", "Minor Prelim Semi", "Minor Prelim", "Major Prelim") ~ "PF",
           round %in% c("Semi Final", "Minor Semi", "Major Semi", "Minor Semi Rep.") ~ "SF",
           round %in% c("Qualif Final", "Qualifier", "Minor Qualif", "Major Qualif") ~ "QF",
           round %in% c("Elim Qualif", "Elim Prelim Final") ~ "EF",
           round %in% c("Grand Final", "Final", "Grand Final Rep.") ~ "GF",
           round %in% c("Grand Final Chall.") ~ "GFC",
           round %in% c("Playoff") ~ "PO",
           .default = round
         ),
         year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         match = paste0(year, " ", round, " (", teams, ")"),
         score = paste0(home_team_score, "\u2013", away_team_score)) |>
  select(match_id, year, round, date, teams, match, score)

##### Analysis #####
same_team_debut_last <- player_match_data |>
  select(player_id, match_id, team, opposition_team) |>
  left_join(match_data |> select(match_id, date), by = "match_id") |>
  left_join(team_data |> select(team_name, team_unique), by = c("opposition_team" = "team_name")) |>
  rename(opposition_team_unique = team_unique) |>
  group_by(player_id) |>
  arrange(date) |>
  #filter(year(min(date)) >= 1998) |>
  mutate(game_number = row_number()) |>
  filter(game_number == 1 | game_number == max(game_number),
         max(game_number) > 1) |>
  filter(length(unique(opposition_team_unique)) == 1) |>
  ungroup() |>
  mutate(game_number = ifelse(game_number == 1, "debut", "last_match")) |>
  pivot_wider(id_cols = c(player_id, opposition_team_unique), names_from = game_number, values_from = match_id) |>
  left_join(match_summary |> select(match_id, match, score), by = c("debut" = "match_id")) |>
  left_join(player_match_data |> select(player_id, match_id, team), by = c("player_id", "debut" = "match_id")) |>
  left_join(match_results |> select(match_id, team, margin), by = c("debut" = "match_id", "team")) |>
  select(-debut, -team) |>
  rename(debut = match, debut_score = score, debut_margin = margin) |>
  left_join(match_summary |> select(match_id, match, score), by = c("last_match" = "match_id")) |>
  left_join(player_match_data |> select(player_id, match_id, team), by = c("player_id", "last_match" = "match_id")) |>
  left_join(match_results |> select(match_id, team, margin), by = c("last_match" = "match_id", "team")) |>
  select(-last_match, -team) |>
  rename(last_match = match, last_match_score = score, last_match_margin = margin) |>
  mutate(combined_margin = debut_margin + last_match_margin) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(full_name, opposition_team_unique, debut, debut_score, last_match, last_match_score, combined_margin) |>
  arrange(desc(combined_margin))

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block",
              padding = "2px 4px 2px 4px",
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

# Final table formatting
final_table <- same_team_debut_last |> 
  #filter(row_number() <= 10) |>
  mutate(num = ifelse(combined_margin == lag(combined_margin), "=", row_number() |> as.character()) |> replace_na("1")) |>
  relocate(num, .before= full_name) |>
  rename(
    `#` = num,
    Player = full_name,
    `Debut & Last Opposition` = opposition_team_unique,
    Debut = debut,
    Score = debut_score,
    `Last Match` = last_match,
    `Score ` = last_match_score,
    `Combined<br>Margin` = combined_margin
  ) |>
  formattable(
    list(
      `#` = bold_formatter,
      Player = bold_formatter,
      `Debut & Last Opposition` = team_formatter,
      Score = bold_formatter,
      `Score ` = bold_formatter,
      `Combined<br>Margin` = bold_formatter
    ),
    align = c("c", "l", "l", "r", "c", "r", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(9) td {background-color: #ffd6f0;}")) |>
  #prependContent(tags$style("table td:nth-child(4), td:nth-child(5) { width: 180px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-family: Montserrat; font-weight: bold; padding: 3px 0 0 0;",
                    "Largest combined losing margin for NSWRL/NRL players who<br>played their debut and final match against the same team" |> HTML()))
final_table

saveWidget(final_table, "tables/html/same_team_debut_last.html")
webshot::webshot(url = "tables/html/same_team_debut_last.html", 
        file = "tables/png/same_team_debut_last.png", 
        selector = "div", zoom = 4,
        vwidth = 775)
