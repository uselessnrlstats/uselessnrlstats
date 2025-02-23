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
largest_losing_score <- match_data |>
  arrange(date) |>
  select(competition_year, round, home_team, home_team_score, away_team, away_team_score) |>
  rbind(tibble(competition_year = "NRL 2024", round = "Round 26", home_team = "Parramatta Eels", home_team_score = 44, away_team = "St George Illawarra Dragons", away_team_score = 40)) |>
  rowwise() |>
  mutate(min_score = min(home_team_score, away_team_score)) |>
  arrange(desc(min_score)) |>
  ungroup() |>
  filter(min_score >= 36) |>
  mutate(
    home_team_score_a = ifelse(home_team_score > away_team_score,
                               paste0("<b>", home_team_score, "</b>"),
                               home_team_score),
    away_team_score_a = ifelse(home_team_score < away_team_score,
                               paste0("<b>", away_team_score, "</b>"),
                               away_team_score),
    match_score = paste0(home_team_score_a, "\u2013", away_team_score_a),
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "Rd\\1", round),
    year_rd = paste0(year, " ", round)) |>
  select(year_rd, home_team, match_score, away_team, min_score)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block",
              padding = "2px 4px",
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
final_table1 <- largest_losing_score |>
  rename(
    Match = year_rd,
    `Home Team` = home_team,
    vs = match_score,
    `Away Team` = away_team,
    `Losing<br>Score` = min_score
  ) |>
  formattable(
    list(
      `Home Team` = team_formatter,
      `Away Team` = team_formatter,
      `Losing<br>Score` = bold_formatter
    ),
    align = c("l", "r", "c", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(1) td {background-color: #ffd6f0;}")) |>
  prependContent(tags$style("table td:nth-child(2), td:nth-child(4) { width: 220px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-family: Montserrat; font-weight: bold; padding: 3px 0 0 0;",
                    "NSWRL/NRL: Largest losing scores"))
final_table1

saveWidget(final_table1, "tables/html/largest_losing_score.html")
webshot(url = "tables/html/largest_losing_score.html", 
        file = "tables/png/largest_losing_score.png", 
        selector = "div", zoom = 4,
        vwidth = 720)

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
