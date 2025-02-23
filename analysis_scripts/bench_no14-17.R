##### Description #####
# An R script to look at pi related stats

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
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv") |>
  mutate(position = case_when(
    position == "FB" ~ "Fullback",
    position == "W" ~ "Wing",
    position == "C" ~ "Centre",
    position == "FE" ~ "5/8th",
    position == "HB" ~ "Halfback",
    position == "FR" ~ "Prop",
    position == "HK" ~ "Hooker",
    position == "2R" ~ "2nd Row",
    position == "L" ~ "Lock",
    position == "B" ~ "Bench"
  ))
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

##### Analysis #####
bench_1417 <- player_match_data |>
  left_join(match_data |> select(match_id, competition_year, round, date),
            by = "match_id") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  arrange(date) |>
  filter(position == "Bench") |>
  group_by(match_id, team) |>
  filter(all(!(number %in% 14:17))) |>
  filter(n() >= 4) |>
  ungroup() |>
  mutate(label = paste0(number, ". ", full_name)) |>
  group_by(match_id, competition_year, round, date, team, opposition_team) |>
  summarise(
    position = paste0(position, collapse = "<br>"),
    num_players = paste0(label, collapse = "<br>"),
    .groups = "drop"
  ) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round)) |>
  arrange(date) |>
  left_join(match_results |> select(match_id, team, result), by = c("match_id", "team")) |>
  select(year, round, team, opposition_team, position, num_players, result) |>
  filter(year >= 1998)

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
              font.weight = "bold"
            ))

# Final table formatting
final_table <- bench_78 |> 
  rbind(tibble(year = "2024", round = "Rd26", team = "Dolphins", opposition_team = "Brisbane Broncos", position = "Bench<br>Bench", num_players = "7. Isaiya Katoa<br>8. Kenneath Bromwich", result = "W")) |>
  mutate(
    year_rd = paste0(year, " ", round),
    num_players = paste0("<b>", num_players, "</b>"),
    ` ` = "v") |>
  select(year_rd, team, ` `, opposition_team, position, num_players, result) |>
  rename(
    Match = year_rd,
    Team = team,
    Opposition = opposition_team,
    Position = position,
    `#. Player` = num_players,
    Result = result
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Opposition = team_formatter
    ),
    align = c("l", "r", "c", "l", "c", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(7) td {background-color: #ffd6f0;}")) |>
  prependContent(tags$style("table td:nth-child(2), td:nth-child(4) { width: 130px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-family: Montserrat; font-weight: bold; padding: 3px 0 0 0;",
                    "NSWRL/NRL Teams with #7 and #8 starting on the bench"))
final_table

saveWidget(final_table, "tables/html/bench_78.html")
webshot(url = "tables/html/bench_78.html", 
        file = "tables/png/bench_78.png", 
        selector = "div", zoom = 4,
        vwidth = 690)
