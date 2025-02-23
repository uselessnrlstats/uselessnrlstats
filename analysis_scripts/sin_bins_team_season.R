##### Description #####
# An R script to look at sin bins / send-offs by team and season

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
current_teams <- match_data |>
  filter(competition_year == "NRL 2024") |>
  distinct(away_team) |>
  rename(team_name = away_team) |>
  left_join(team_data |> select(team_name, team_unique, team_mascots),
            by = c("team_name")) |>
  left_join(team_logos, by = "team_unique") |>
  arrange(team_unique)

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
  select(match_id, competition_year, year, date, round, home_away, team, opposition_team, score_for, score_against, result) |>
  left_join(team_data |> select(team_name, team_unique), by = c("team" = "team_name")) |>
  left_join(team_data |> select(team_name, team_unique) |> rename(opp_unique = team_unique), by = c("opposition_team" = "team_name"))

##### Analysis #####
sin_bin_match_data <- player_match_data |>
  select(match_id, team, player_id, sin_bins5, sin_bins, send_offs) |>
  left_join(match_data |> select(match_id, competition_year, round), by = "match_id") |>
  filter(grepl("Round", round)) |>
  group_by(competition_year, team) |>
  summarise(sin_bins = sum(sin_bins + sin_bins5),
            send_offs = sum(send_offs),
            total = sin_bins + send_offs,
            .groups = "drop") |>
  arrange(desc(total))

sin_bin_ladder <- match_results |>
  select(match_id, competition_year, year, round, team, opposition_team) |>
  filter(competition_year == "NRL 2024", grepl("Round", round)) |>
  left_join(sin_bin_match_data, by =  c("match_id", "team")) |>
  rename(sin_bins_for = sin_bins, send_offs_for = send_offs) |>
  left_join(sin_bin_match_data, by =  c("match_id", "opposition_team" = "team")) |>
  rename(sin_bins_against = sin_bins, send_offs_against = send_offs) |>
  group_by(team) |>
  summarise(played = n(),
            sb_for = sum(sin_bins_for),
            so_for = sum(send_offs_for),
            cards_for = sum(sin_bins_for + send_offs_for),
            sb_against = sum(sin_bins_against),
            so_against = sum(send_offs_against),
            cards_against = sum(sin_bins_against + send_offs_against),
            cards_diff = cards_for - cards_against,
            .groups = "drop") |>
  arrange(cards_diff, cards_for, desc(cards_against)) |>
  mutate(pos = row_number())

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "2px 4px 2px 4px",
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
              font.size = "14px",
              font.family = "Montserrat"
            ))

xnormalize <- function(x) {
  x <- c(x, -12, 12)
  normalize(x)[1:(length(x) - 2)]
}

pd_formatter <-
  formatter("span", style = function(x) {
    colours <- gsub("+", "", x) |>
      as.numeric() |>
      xnormalize() |>
      colorRamp(c("green4",'white', "firebrick1"))() |>
      as.integer() |>
      matrix(byrow = TRUE, dimnames = list(c("red", "green", "blue"), NULL), nrow = 3) |>
      csscolor()
    style(display = "block",
          'text-align' = 'center',
          padding = "2px 4px 2px 4px", 
          `border-radius` = "4px",
          color = ifelse(luminance(colours) < 0.52, "#F2F2F2", "#1A1A1A"),
          `background-color` = colours,
          font.weight = "bold",
          font.size = "14px",
          font.family = "Montserrat")
  })

for_formatter <- formatter("span", style = function(x) {
  colours <- (x |>
    c(0) |>
    as.numeric() |>
    gradient('white', "firebrick1") |>
    csscolor())[1:(length(x))]
  style(display = "block",
        'text-align' = 'center',
        padding = "2px 4px 2px 4px", 
        `border-radius` = "4px",
        color = ifelse(luminance(colours) < 0.52, "#F2F2F2", "#1A1A1A"),
        `background-color` = colours,
        font.weight = "bold",
        font.size = "14px",
        font.family = "Montserrat")
})

against_formatter <- formatter("span", style = function(x) {
  colours <- (x |>
                c(0) |>
                as.numeric() |>
                gradient('white', "green4") |>
                csscolor())[1:(length(x))]
  style(display = "block",
        'text-align' = 'center',
        padding = "2px 4px 2px 4px", 
        `border-radius` = "4px",
        color = ifelse(luminance(colours) < 0.52, "#F2F2F2", "#1A1A1A"),
        `background-color` = colours,
        font.weight = "bold",
        font.size = "14px",
        font.family = "Montserrat")
})

# Final table formatting
final_table <- sin_bin_ladder |>
  mutate(cards_diff = paste0(ifelse(cards_diff > 0, "+", ""), cards_diff)) |>
  mutate(pos = row_number()) |>
  select(pos, team, cards_for, cards_against, cards_diff) |>
  rename(
    `#` = pos,
    Team = team,
    `SB+SO<br>For` = cards_for,
    `SB+SO<br>Against` = cards_against,
    `SB+SO<br>Difference` = cards_diff
  ) |>
  formattable(
    list(
      `#` = num_formatter,
      Team = team_formatter,
      `SB+SO<br>For` = for_formatter,
      `SB+SO<br>Against` = against_formatter,
      `SB+SO<br>Difference` = num_formatter 
    ),
    align = c("r", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13.5px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  # prependContent(tags$style("table tr:nth-child(6) td {background-color: #ffd6f0;}")) |>
  # prependContent(tags$style("table td:nth-child(4) { width: 320px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-family: Montserrat; padding: 3px 0 0 0;",
                    "<b style='font-size: 20px;'>2024 Sin Bin (+ Send Off) Ladder</b><br>2024 NRL teams ranked by their sin-bin + send-off for/against differential" |> HTML()))
final_table

saveWidget(final_table, "tables/html/sin_bin_ladder24.html")
webshot(url = "tables/html/sin_bin_ladder24.html",
        file = "tables/png/sin_bin_ladder24.png",
        selector = "div", zoom = 4,
        vwidth = 730)
