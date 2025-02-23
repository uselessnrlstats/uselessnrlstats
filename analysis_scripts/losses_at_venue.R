##### Description #####
# An R script to look at players with only losses at a single venue

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(showtext)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
venue_data <- read_csv("cleaned_data/nrl/venue_data.csv")

##### Helper Stats #####
match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, round, home_away, team, score_for, score_against, result)


##### Analysis #####
player_match_data |>
  select(player_id, match_id, team, captain) |>
  left_join(match_data |> select(match_id, venue_id), by = "match_id") |>
  left_join(match_results |> select(match_id, team, result), 
            by = c("match_id", "team")) |>
  group_by(player_id, venue_id) |>
  summarise(n_wins = sum(result == "W"),
            n_matches = n(),
            .groups = "drop") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  left_join(venue_data |> select(venue_id, venue_name, `non-commercial_name`),
            by = "venue_id") |>
  filter(n_wins == 0) |>
  arrange(desc(n_matches)) |>
  select(full_name, venue_name, `non-commercial_name`, n_matches, n_wins)

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
final_table1 <- scrabble |>
  arrange(desc(full_name_score)) |>
  filter(row_number() %in% 1:19) |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id") |>
  mutate(score_per_letter = digits(score_per_letter, 2),
         ` ` = row_number()) |>
  select(` `, full_name, career_years, n_letters, full_name_score, score_per_letter) |>
  rename(
    Player = full_name,
    Career = career_years,
    Letters = n_letters,
    Score = full_name_score,
    `Score / Letter` = score_per_letter
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      Score = bold_formatter
    ),
    align = c("c", "r", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Highest NRL Name Scrabble Scores"))
final_table1

saveWidget(final_table1, "tables/html/scrabble1.html")
webshot(url = "tables/html/scrabble1.html", 
        file = "tables/png/scrabble1.png", 
        selector = "body", zoom = 4,
        vwidth = 500)

# Final Table 2 Formatting
final_table2 <- scrabble |>
  arrange(desc(score_per_letter)) |>
  filter(row_number() %in% 1:20) |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id") |>
  mutate(score_per_letter = digits(score_per_letter, 2),
         ` ` = row_number()) |>
  select(` `, full_name, career_years, n_letters, full_name_score, score_per_letter) |>
  rename(
    Player = full_name,
    Career = career_years,
    Letters = n_letters,
    Score = full_name_score,
    `Score / Letter` = score_per_letter
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `Score / Letter` = bold_formatter
    ),
    align = c("c", "r", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Highest NRL Name Scrabble Scores per Letter"))
final_table2

saveWidget(final_table2, "tables/html/scrabble2.html")
webshot(url = "tables/html/scrabble2.html", 
        file = "tables/png/scrabble2.png", 
        selector = "body", zoom = 4,
        vwidth = 500)
