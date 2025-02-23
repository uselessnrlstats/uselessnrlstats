##### Description #####
# An R script to look at tries per name in a single round

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
tries_per_name <- player_match_data |>
  mutate(tries = tries + penalty_tries) |>
  left_join(player_data |> select(player_id, first_name, full_name), by = "player_id") |>
  left_join(match_data |> select(match_id, competition_year, round), by = "match_id") |>
  select(match_id, competition_year, round, team, player_id, first_name, full_name, tries) |>
  rowwise() |>
  mutate(label = paste0("<b>", full_name, "</b> (", tries, ")")) |>
  ungroup() |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  group_by(competition_year, round, first_name) |>
  arrange(desc(tries)) |>
  summarise(
    n_players = n(),
    n_tryscorers = sum(tries > 0),
    n_tries = sum(tries),
    players = paste0(label, collapse = ", "),
    .groups = "drop"
  ) |>
  mutate(tries_per_player = n_tries / n_players) |>
  #filter(n_players >= 2) |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "Rd\\1", round)
  ) |>
  arrange(desc(tries_per_player), year, round)
  
  

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
                       text_col = ifelse(lum < 0.35, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- tries_per_name |>
  filter(tries_per_player > 2.5) |>
  select(year, round, first_name, players, n_tries, tries_per_player) |>
  mutate(tries_per_player = round(tries_per_player, 2)) |>
  rename(
    Year = year,
    Round = round,
    Name = first_name,
    `Players (# Tries)` = players,
    `Total<br>Tries` = n_tries,
    `Tries per<br>Player` = tries_per_player
  ) |>
  formattable(
    list(
      Name = bold_formatter,
      `Tries per<br>Player` = bold_formatter
    ),
    align = c("c", "l", "l", "l", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(7) td {background-color: #ffd6f0;}")) |>
  prependContent(tags$style("table td:nth-child(4) { width: 320px;}")) |>
  prependContent(h5(class = "title",
                    style = "font-family: Montserrat; font-weight: bold; margin-left: 5px;",
                    "First names with the most 'Tries per Player' in a single NRL round (\u22652 players)"))
final_table

saveWidget(final_table, "tables/html/tries_per_name.html")
webshot(url = "tables/html/tries_per_name.html", 
        file = "tables/png/tries_per_name.png", 
        selector = "body", zoom = 4,
        vwidth = 760)
