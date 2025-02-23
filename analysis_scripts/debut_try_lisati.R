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
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")
VOWELS <- c("A", "E", "I", "O", "U")

##### Helper Stats #####

##### Analysis #####
alternating_vowels <- player_data |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  select(player_id, full_name) |>
  mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |>
           str_to_upper() |>
           str_split(pattern = "")) |>
  rowwise() |>
  mutate(n_letters = length(full_name_letters),
         vowels = list(which(full_name_letters %in% VOWELS))) |>
  mutate(alternating = (identical(vowels, as.integer(seq(2, n_letters, by = 2))) | 
                          identical(vowels, as.integer(seq(1, n_letters, by = 2))))) |>
  select(player_id, alternating)

try_on_debut_alternating <- player_match_data |>
  left_join(match_data |> select(match_id, competition_year, round, date),
            by = "match_id") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  group_by(player_id) |>
  arrange(date) |>
  mutate(game_number = row_number()) |>
  ungroup() |>
  filter(game_number == 1 & (tries > 0 | penalty_tries > 0)) |>
  #filter(year(date) >= 1998) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round)) |>
  select(player_id, year, round, date, team, opposition_team, full_name, tries) |>
  left_join(alternating_vowels, by = "player_id") |>
  filter(alternating)

try_on_debut16 <- player_match_data |>
  left_join(match_data |> select(match_id, competition_year, round, date),
            by = "match_id") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  group_by(player_id) |>
  arrange(date) |>
  mutate(game_number = row_number()) |>
  ungroup() |>
  filter(game_number == 1 & (tries > 0 | penalty_tries > 0) & number == 16) |>
  filter(year(date) >= 1998) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round)) |>
  select(player_id, year, round, date, team, opposition_team, full_name, number, position, tries)

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
final_table1 <- try_on_debut16 |>
  mutate(year_rd = paste0(year, " " , round)) |>
  select(year_rd, team, number, full_name, tries) |>
  rename(
    Match = year_rd,
    Team = team,
    `No.` = number,
    Player = full_name,
    Tries = tries
  ) |>
  formattable(
    list(
      Team = team_formatter,
      `No.` = bold_formatter,
      Player = bold_formatter
    ),
    align = c("l", "l", "r", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(9) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-family: Montserrat; font-weight: bold; padding: 3px 0 0 0;",
                    "Scoring on debut in jersey #16 (NRL-era)"))
final_table1

saveWidget(final_table1, "tables/html/debut_try_lisati1.html")
webshot(url = "tables/html/debut_try_lisati1.html", 
        file = "tables/png/debut_try_lisati1.png", 
        selector = "div", zoom = 4,
        vwidth = 600)

# Final table formatting
final_table2 <- try_on_debut_alternating |>
  mutate(year_rd = paste0(year, " " , round),
         full_name = toupper(full_name)) |>
  select(year_rd, team, full_name, tries) |>
  rename(
    Match = year_rd,
    Team = team,
    Player = full_name,
    Tries = tries
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Player = bold_formatter
    ),
    align = c("l", "l", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(7) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-family: Montserrat; font-weight: bold; padding: 3px 0 0 0;",
                    "Alternating vowels/consonants scoring on debut (all-time)"))
final_table2

saveWidget(final_table2, "tables/html/debut_try_lisati2.html")
webshot(url = "tables/html/debut_try_lisati2.html", 
        file = "tables/png/debut_try_lisati2.png", 
        selector = "div", zoom = 4,
        vwidth = 600)
