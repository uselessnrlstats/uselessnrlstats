##### Description #####
# An R script to look at matches where the halftime score has been an even multiple of the full time score

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
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
ht_ft_multiples <- match_data |>
  filter(home_team_ht_score != 0, home_team_score != 0,
         away_team_ht_score != 0, away_team_score != 0) |>
  mutate(ht_ratio = home_team_score / home_team_ht_score,
         at_ratio = away_team_score / away_team_ht_score) |>
  filter(ht_ratio == at_ratio, ht_ratio > 1) |>
  select(competition_year, round, date, home_team, home_team_ht_score, home_team_score, away_team, away_team_ht_score, away_team_score, ht_ratio) |>
  arrange(desc(ht_ratio), date)

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
  #filter(ht_ratio >= 3) |>
  filter(competition_year == "NRL 2024") |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "\\1", round),
    match_ht_score = paste0(home_team_ht_score, "\u2013", away_team_ht_score),
    match_score = paste0(home_team_score, "\u2013", away_team_score),
    score_text = paste0("", match_ht_score, " \u21A6 <b>", match_score, "</b>"),
    ratio_text = paste0("\u00D7", round(ht_ratio, 2))) |>
  select(year, round, home_team, score_text, away_team, ratio_text) |>
  rename(
    Year = year,
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
