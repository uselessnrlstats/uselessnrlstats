##### Description #####
# An R script to look at rock names scoring on debut

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
match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, round, date, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, round, date, home_away, team, score_for, score_against, result)

##### Analysis #####


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
final_table <- matches_per_bin |>
  mutate(
    `Sin Bins/Send Offs` = paste(total_sin_bins, "/", total_send_offs),
    matches_per_card = round(matches_per_card, 2)
  ) |>
  select(career_years, full_name, total_matches, `Sin Bins/Send Offs`, total_cards, matches_per_card) |>
  filter(row_number() <= 15) |>
  rename(
    Career = career_years,
    Player = full_name,
    Matches = total_matches, 
    `Total SB/SO` = total_cards,
    `Matches per SB/SO` = matches_per_card
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `Matches per SB/SO` = bold_formatter
    ),
    align = c("c", "r", "r", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "<b>Early Showers:</b> Players with the fewest matches per sin bin or send off in the NRL era (1998-present). Minimum 3 SB + SO." |> HTML()))
final_table

saveWidget(final_table, "tables/html/matches_per_bin_NRL.html")
webshot(url = "tables/html/matches_per_bin_NRL.html", 
        file = "tables/png/matches_per_bin_NRL.png", 
        selector = "body", zoom = 4,
        vwidth = 700)
