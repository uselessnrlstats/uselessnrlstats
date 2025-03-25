##### Description #####
# An R script to look at which year had the mos forwards as captains

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
  library(slider)
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
  select(match_id, competition_year, year, date, round, home_team, home_team_ht_score, home_team_score, away_team, away_team_ht_score, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  left_join(matchups_by_team, by = c("match_id", "team")) |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for_ht = ifelse(home_away == "H", home_team_ht_score, away_team_ht_score),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against_ht = ifelse(home_away == "H", away_team_ht_score, home_team_ht_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D"),
         match_score = paste0(score_for, "\u2013", score_against)) |>
  select(match_id, competition_year, year, date, round, home_away, team, match_score, opposition_team, score_for_ht, score_for, score_against_ht, score_against, result)

##### Analysis #####
half_difference <- match_results |>
  filter(!is.na(score_for_ht)) |>
  mutate(
    match = paste(year, gsub("Round (\\d+)", "Rd\\1", round)),
    second_half_score = score_for - score_for_ht,
    half_score_diff = score_for_ht - second_half_score) |>
  select(match_id, year, round, match, team, match_score, opposition_team, score_for_ht, second_half_score, result, half_score_diff) |>
  arrange(desc(half_score_diff), year, round) |>
  filter(second_half_score >= 0)
  

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
              display = "block"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat"
            ))

num_bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat",
              font.weight = "bold"
            ))

# Final table formatting
final_table <- half_difference |>
  ungroup() |>
  # left_join(team_data |> select(team_name, team_unique, team_mascots), by = c("opposition_team" = "team_name")) |>
  # left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  # mutate(opp_mascot_col = paste0('<span style="display: inline; padding: 2px 4px 2px 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', team_mascots, '</span>')) |>
  select(match, team, match_score, opposition_team, score_for_ht, second_half_score, half_score_diff) |>
  filter(half_score_diff >= 32) |>
  rename(
    Match = match,
    Team = team,
    Score = match_score,
    Opposition = opposition_team,
    `1st Half<br>Points` = score_for_ht,
    `2nd Half<br>Points` = second_half_score,
    `1st - 2nd<br>Diff.` = half_score_diff
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Score = num_bold_formatter,
      Opposition = team_formatter,
      `1st Half<br>Points` = num_formatter,
      `2nd Half<br>Points` = num_formatter,
      `1st - 2nd<br>Diff.` = num_bold_formatter
    ),
    align = c("l", "r", "c", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(3) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "<u>FOOT OFF THE PEDAL</u><br>NRL/NSWRL teams with the largest difference in a match<br>between points scored in the 1st half and 2nd half" |> HTML()))
final_table

saveWidget(final_table, "tables/html/foot_off_pedal.html")
webshot(url = "tables/html/foot_off_pedal.html", 
        file = "tables/png/foot_off_pedal.png", 
        selector = "div", zoom = 4,
        vwidth = 800)
