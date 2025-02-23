##### Description #####
# An R script to look at matches where field goals being 2 points would've reversed the result

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
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
team_scoring_data <- player_match_data |>
  group_by(match_id, team) |>
  summarise(
    tries = sum(tries + penalty_tries, na.rm = TRUE),
    goals = sum(goals),
    field_goals = sum(field_goals),
    field_goals2 = sum(field_goals2)
  )

match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  filter(year >= 1983) |>
  select(match_id, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, team, score_for, score_against, result) |>
  left_join(team_scoring_data, by = c("match_id", "team")) |>
  mutate(points_recalc = 4 * (tries) + 2 * (goals + field_goals2 + field_goals))
  filter(points_recalc > score_against, result == "L")


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
final_table <- ht_ft_multiples |>
  filter(ht_ratio >= 3) |>
  mutate(
    match_ht_score = paste0(home_team_ht_score, "\u2013", away_team_ht_score),
    match_score = paste0(home_team_score, "\u2013", away_team_score),
    score_text = paste0("", match_ht_score, " \u21A6 <b>", match_score, "</b>"),
    ratio_text = paste0("\u00D7", round(ht_ratio, 3))) |>
  select(competition_year, round, home_team, score_text, away_team, ratio_text) |>
  rename(
    Year = competition_year,
    Round = round,
    `Home Team` = home_team,
    `HT Score - Match Score` = score_text,
    `Away Team` = away_team,
    `Multiplier` = ratio_text
  ) |>
  formattable(
    list(
      `Year` = bold_formatter,
      `Home Team` = team_formatter,
      `Away Team` = team_formatter
    ),
    align = c("r", "r", "r", "c", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Halftime - Fulltime Multiples")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL matches where the full-time score has been equal to some multiplier times the half-time score. Shown are such matches with the 11 highest multipliers."))
final_table

saveWidget(final_table, "tables/html/ht_ft_multiples.html")
webshot(url = "tables/html/ht_ft_multiples.html", 
        file = "tables/png/ht_ft_multiples.png", 
        selector = "body", zoom = 4,
        vwidth = 830)

##### table 2 #####
final_table <- ht_ft_multiples |>
  filter(year(date) >= 2020) |>
  arrange(date) |>
  mutate(
    match_ht_score = paste0(home_team_ht_score, "\u2013", away_team_ht_score),
    match_score = paste0(home_team_score, "\u2013", away_team_score),
    score_text = paste0("", match_ht_score, " \u21A6 <b>", match_score, "</b>"),
    ratio_text = paste0("\u00D7", round(ht_ratio, 3))) |>
  select(competition_year, round, home_team, score_text, away_team, ratio_text) |>
  rename(
    Year = competition_year,
    Round = round,
    `Home Team` = home_team,
    `HT Score - Match Score` = score_text,
    `Away Team` = away_team,
    `Multiplier` = ratio_text
  ) |>
  formattable(
    list(
      `Year` = bold_formatter,
      `Home Team` = team_formatter,
      `Away Team` = team_formatter
    ),
    align = c("r", "r", "r", "c", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Halftime - Fulltime Multiples: 2020s")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL matches where the full-time score has been equal to some multiplier times the half-time score. Shown are all such matches in the 2020s."))
final_table

saveWidget(final_table, "tables/html/ht_ft_multiples2.html")
webshot(url = "tables/html/ht_ft_multiples2.html", 
        file = "tables/png/ht_ft_multiples2.png", 
        selector = "body", zoom = 4,
        vwidth = 830)
