##### Description #####
# An R script to look at players who kicked at 100% on debut

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

##### Analysis #####
player_match_data |>
  left_join(match_data, by = "match_id") |>
  select(match_id, competition_year, round, date, team, player_id) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  filter(round %in% c("Grand Final", "Final", "Grand Final Rep.")) |>
  mutate(round = case_when(
    round == "Grand Final" ~ "GF",
    round == "Final" ~ "GF",
    round == "Grand Final Rep." ~ "GF Rep.",
    .default = NA
  )) |>
  mutate(match_summary = paste0(year, " ", round)) |>
  left_join(player_data |> select(player_id, first_name, full_name), by = "player_id") |>
  arrange(first_name, date) |>
  group_by(first_name) |>
  mutate(prev_final = lag(match_summary),
         prev_player = lag(full_name),
         prev_team = lag(team),
         year_gap = year - lag(year)) |>
  select(match_summary, full_name, team, prev_final, prev_player, prev_team, year_gap) |>
  arrange(desc(year_gap))

most_spine_tries <- player_match_data |>
  mutate(tries = tries + penalty_tries) |>
  select(player_id, match_id, team, opposition_team, position, tries) |>
  filter(position %in% c("FB", "FE", "HB", "HK")) |>
  arrange(match_id, position) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  mutate(player_summary = paste0("<b>", full_name, "</b> (", tries, ")")) |>
  group_by(match_id, team, opposition_team) |>
  summarise(
    spine_positions = paste0(position, collapse = "<br>"),
    spine_players = paste0(player_summary, collapse = "<br>"),
    n_spine_tries = sum(tries),
    .groups = "drop"
  ) |>
  left_join(match_tries, by = c("match_id", "team")) |>
  left_join(match_data |> select(match_id, competition_year, round, date), by = "match_id") |>
  #filter(!(grepl("Round", round))) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round),
         round = case_when(
           round %in% c("Prelim Final", "Prelim Playoff", "Major Prelim Semi", "Minor Prelim Semi", "Minor Prelim", "Major Prelim") ~ "PF",
           round %in% c("Semi Final", "Minor Semi", "Major Semi", "Minor Semi Rep.") ~ "SF",
           round %in% c("Qualif Final", "Qualifier", "Minor Qualif", "Major Qualif") ~ "QF",
           round %in% c("Elim Qualif", "Elim Final", "Elim Prelim Final") ~ "EF",
           round %in% c("Grand Final", "Final", "Grand Final Rep.") ~ "GF",
           round %in% c("Grand Final Chall.") ~ "GFC",
           round %in% c("Playoff") ~ "PO",
           .default = round
         )) |>
  mutate(spine_try_frac = paste0(n_spine_tries, "/", n_tries)) |>
  arrange(desc(n_spine_tries), n_tries, date) |>
  mutate(match = paste(year, round)) |>
  select(match, team, opposition_team, spine_positions, spine_players, n_spine_tries, n_tries)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "3px 4px 3px 4px", 
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

bold_num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat",
              font.weight = "bold"
            ))

# Final table formatting
final_table <- most_spine_tries |>
  filter(n_spine_tries >= 7) |>
  rbind(tibble(
    match = "",
    team = paste0("\u00D7", most_spine_tries |> filter(n_spine_tries == 6) |> nrow()),
    opposition_team = "",
    spine_positions = "",
    spine_players = "",
    n_spine_tries = 6,
    n_tries = ""
  )) |>
  select(-opposition_team) |>
  rename(
    Match = match,
    Team = team,
    ` ` = spine_positions,
    `Spine Players (Tries)` = spine_players,
    `# Spine<br>Tries` = n_spine_tries,
    `Total<br>Tries` = n_tries
  ) |>
  formattable(
    list(
      Team = team_formatter,
      `# Spine<br>Tries` = bold_num_formatter,
      `Total<br>Tries` = num_formatter
    ),
    align = c("l", "l", "r", "l", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(2) td {background-color: #ffd6f0;}")) |>
  prependContent(tags$style("table td:nth-of-type(2) { width: 120px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "Most tries by the 4 spine players (FB, FE, HB, HK) in a team in a single NRL/NSWRL match"))
final_table

saveWidget(final_table, "tables/html/most_spine_tries.html")
webshot(url = "tables/html/most_spine_tries.html", 
        file = "tables/png/most_spine_tries.png", 
        selector = "div", zoom = 4,
        vwidth = 550)
