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
premierships <- read_csv("helper_stats/premiership_data.csv") |>
  left_join(team_data |> select(team_name, team_unique) |> distinct(),
            by = c("Premiership" = "team_name")) |>
  mutate(Premiership = team_unique) |> select(-team_unique)

reigning_premiers <- premierships |>
  select(competition_year, year, Premiership) |>
  mutate(prev_year = year - 1) |>
  left_join(premierships |> rename(reigning = Premiership) |> select(year, reigning),
            by = c("prev_year" = "year"), relationship = "many-to-many") |>
  group_by(competition_year, year, Premiership) |>
  summarise(reigning = list(unique(reigning)),
            .groups= "drop")
  
##### Helper Stats #####
matchups_by_team <- player_match_data |>
  select(match_id, team, opposition_team) |>
  distinct()

match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round)) |>
  select(match_id, competition_year, year, date, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  left_join(matchups_by_team, by = c("match_id", "team")) |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D"),
         match_score = paste0(score_for, "\u2013", score_against)) |>
  left_join(team_data |> select(team_name, team_unique) |> distinct(),
            by = c("team" = "team_name")) |>
  mutate(team = team_unique) |> select(-team_unique) |>
  left_join(team_data |> select(team_name, team_unique) |> distinct(),
            by = c("opposition_team" = "team_name")) |>
  mutate(opposition_team = team_unique) |> select(-team_unique) |>
  select(match_id, competition_year, year, date, round, home_away, team, match_score, opposition_team, score_for, score_against, result)

##### Analysis #####
beat_prems_after_50 <- match_results |>
  select(-home_away) |>
  arrange(date) |>
  group_by(team) |>
  mutate(match_id_next = lead(match_id)) |>
  distinct() |>
  left_join(match_results |> select(-home_away) |> distinct(),
            by = c("match_id_next" = "match_id", "team"), suffix = c("", "_next")) |>
  left_join(reigning_premiers |> select(competition_year, reigning) |> na.omit(), by = "competition_year") |>
  rowwise() |>
  filter(
    score_against >= 50,
    result == "L",
    opposition_team_next %in% reigning,
    result_next == "W"
  ) |>
  ungroup() |>
  select(year, team, round, match_score, opposition_team, round_next, match_score_next, opposition_team_next)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "4px 4px", 
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
final_table <- beat_prems_after_50 |>
  ungroup() |>
  left_join(team_logos |> select(team_unique, team_colour), by = c("opposition_team" = "team_unique")) |>
  mutate(opposition_team = paste0('<span style="display: block; padding: 2px 4px 2px 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', opposition_team, '</span>')) |>
  select(-team_colour) |>
  left_join(team_logos |> select(team_unique, team_colour), by = c("opposition_team_next" = "team_unique")) |>
  mutate(opposition_team_next = paste0('<span style="display: block; padding: 2px 4px 2px 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', opposition_team_next, '&#x1F3C6;</span>')) |>
  rowwise() |>
  mutate(
    rounds = paste0(c(round, round_next), collapse = '<p style="line-height:5px; margin:0px;"><br></p>'),
    match_score = paste0("<span style='color: #FF3030'>", match_score, "</span>"),
    match_score_next = paste0("<span style='color: #008B00'>", match_score_next, "</span>"),
    match_scores = paste0(c(match_score, match_score_next), collapse = '<p style="line-height:5px; margin:0px;"><br></p>'),
    match_scores = paste0("<b style='font-size: 13px; font-family: Montserrat;'>", match_scores, "</b>"),
    opposition_teams = paste0(c(opposition_team, opposition_team_next), collapse = '<p style="line-height:5px; margin:0px;"><br></p>'),
  ) |>
  select(year, team, rounds, match_scores, opposition_teams) |>
  rename(
    Year = year,
    Team = team,
    Round = rounds,
    Score = match_scores,
    Opposition = opposition_teams
  ) |>
  formattable(
    list(
      Team = team_formatter
    ),
    align = c("c", "r", "c", "c", "l"),
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
                    "<u>CHANGING FORTUNES</u><br>NRL/NSWRL teams to <span style='color: #FF3030'>concede 50+ points</span>, and then <span style='color: #008B00'>defeat the reigning premiers</span> in their next match (same season)" |> HTML()))
final_table

saveWidget(final_table, "tables/html/concede_50_beat_premiers.html")
webshot(url = "tables/html/concede_50_beat_premiers.html", 
        file = "tables/png/concede_50_beat_premiers.png", 
        selector = "div", zoom = 4,
        vwidth = 580)
