##### Description #####
# An R script to look at players who played in all 4 spine positions in a season

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
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, date, round, home_away, team, opposition_team, score_for, score_against, result)

##### Analysis #####
most_before_finals_loss <- player_match_data |>
  select(match_id, player_id, team) |>
  left_join(match_results |> select(match_id, competition_year, round, date, team, result),
            by = c("match_id", "team")) |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(match_no = row_number(),
         n_matches = n()) |>
  filter(!(grepl("Round", round))) |>
  summarise(
    had_a_loss = ("W" %in% result),
    first_loss_index = ifelse(had_a_loss, min(which(result == "W")), NA),
    match_no = match_no[first_loss_index],
    n_matches = max(n_matches),
    match_id_first_loss = match_id[first_loss_index],
    team_first_loss = team[first_loss_index],
    .groups = "drop"
  ) |>
  mutate(match_no = ifelse(had_a_loss, match_no, n_matches)) |>
  select(player_id, had_a_loss, match_id_first_loss, match_no) |>
  left_join(match_data |> select(match_id, competition_year, round),
            by = c("match_id_first_loss" = "match_id")) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = case_when(
           round %in% c("Prelim Final", "Prelim Playoff", "Major Prelim Semi", "Minor Prelim Semi", "Minor Prelim", "Major Prelim") ~ "PF",
           round %in% c("Semi Final", "Minor Semi", "Major Semi", "Minor Semi Rep.") ~ "SF",
           round %in% c("Qualif Final", "Qualifier", "Minor Qualif", "Major Qualif") ~ "QF",
           round %in% c("Elim Qualif", "Elim Final", "Elim Prelim Final") ~ "EF",
           round %in% c("Grand Final", "Final", "Grand Final Rep.") ~ "GF",
           round %in% c("Grand Final Chall.") ~ "GFC",
           round %in% c("Playoff") ~ "PO",
           .default = round
         ),
         first_loss = ifelse(!had_a_loss, "-", paste0(year, " ", round))) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(player_id, full_name, first_loss, match_no, had_a_loss) |>
  arrange(desc(match_no), first_loss) |>
  mutate(match_no = ifelse(!had_a_loss, paste0(match_no, "*"), match_no)) |>
  select(-player_id, -had_a_loss)

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
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
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
final_table <- most_before_finals_loss |>
  mutate(num = ifelse(match_no == lag(match_no), "=", row_number() |> as.character()) |> replace_na("1")) |>
  filter(row_number() <= 15) |>
  relocate(num, .before = full_name) |>
  rename(
    `#` = num,
    Player = full_name,
    `1st Finals Loss` = first_loss,
    `Match #` = match_no
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `Match #` = bold_formatter
    ),
    align = c("r", "l", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 14px; font-family: Helvetica; vertical-align: middle"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {font-size: 14px; background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(1) td {background-color: #ffd6f0;}")) |>
  #prependContent(tags$style("table td:nth-child(1), td:nth-child(3) { width: 220px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 17px; font-weight: bold; font-family: Montserrat; padding: 5px 3px 0 3px;",
                    "Most NSWRL/NRL matches until first finals loss (\u22651 final played)"))
final_table

saveWidget(final_table, "tables/html/most_before_finals_loss.html")
webshot(url = "tables/html/most_before_finals_loss.html", 
        file = "tables/png/most_before_finals_loss.png", 
        selector = "div", zoom = 4,
        vwidth = 415)
