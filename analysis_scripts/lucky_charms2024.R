##### Description #####
# An R script to look at unique names scoring on debut

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
  select(match_id, competition_year, year, date, round, home_away, team, opposition_team, score_for, score_against, result)

##### Analysis #####
team_matches <- match_results |>
  filter(competition_year == "NRL 2024") |>
  group_by(team) |>
  summarise(matches_all = list(match_id),
            .groups = "drop")

player_team_matches <- player_match_data |>
  select(match_id, team, player_id) |>
  left_join(match_data |> select(match_id, competition_year), by = "match_id") |>
  filter(competition_year == "NRL 2024") |>
  group_by(player_id, team) |>
  summarise(match_in = list(match_id),
            .groups = "drop") |>
  left_join(team_matches, by = "team") |>
  rowwise() |>
  mutate(match_out = list(matches_all[which(!(matches_all %in% match_in))])) |>
  ungroup() |>
  select(-matches_all) |>
  pivot_longer(cols = c(match_in, match_out), names_to = "in_match", values_to = "match_id") |>
  mutate(in_match = ifelse(in_match == "match_in", "Yes", "No")) |>
  unnest(match_id)

win_rate_change <- player_team_matches |>
  select(match_id, team, player_id, in_match) |>
  left_join(match_results |> select(match_id, team, result), by = c("match_id", "team")) |>
  group_by(team, player_id) |>
  summarise(
    matches_in = sum(in_match == "Yes"),
    wins_in = sum(in_match == "Yes" & result == "W"),
    draws_in = sum(in_match == "Yes" & result == "D"),
    losses_in = sum(in_match == "Yes" & result == "L"),
    matches_out = sum(in_match == "No"),
    wins_out = sum(in_match == "No" & result == "W"),
    draws_out = sum(in_match == "No" & result == "D"),
    losses_out = sum(in_match == "No" & result == "L"),
    .groups = "drop"
  ) |>
  mutate(win_rate_in = wins_in / matches_in,
         win_rate_out = wins_out / matches_out,
         record_in = paste0(wins_in, "-",
                           ifelse(draws_in > 0, paste0(draws_in, "-"), ""), 
                           losses_in),
         record_out = paste0(wins_out, "-",
                             ifelse(draws_out > 0, paste0(draws_out, "-"), ""), 
                             losses_out),
         diff_when_in = win_rate_in - win_rate_out) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(player_id, team, full_name, matches_in, record_in, win_rate_in, matches_out, record_out, win_rate_out, diff_when_in) |>
  filter(matches_in >= 5 & matches_out >= 5) |>
  arrange(team, desc(diff_when_in), desc(matches_in + matches_out)) |>
  group_by(team)

lucky_charms <- win_rate_change |>
  filter(row_number() == 1)

harbingers <- win_rate_change |>
  filter(row_number() == max(row_number()))

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
  x <- c(x, -60, 60)
  normalize(x)[1:(length(x) - 2)]
}

perc_formatter <-
  formatter("span", style = function(x) {
    colours <- gsub("%", "", gsub("+", "", x)) |>
      as.numeric() |>
      xnormalize() |>
      colorRamp(c("firebrick1",'white', "green4"))() |>
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

# Final table formatting
final_table1 <- lucky_charms |>
  mutate(diff_when_in = paste0(ifelse(diff_when_in > 0, "+", ""), percent(diff_when_in, digits = 1))) |>
  select(team, full_name, record_in, record_out, diff_when_in) |>
  rename(
    Team = team,
    Player = full_name,
    `W-(D)-L<br>With` = record_in,
    `W-(D)-L<br>Without` = record_out,
    `&#x0394; Win-Rate` = diff_when_in
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Player = bold_formatter,
      `W-(D)-L<br>With` = num_formatter,
      `W-(D)-L<br>Without` = num_formatter,
      `&#x0394; Win-Rate` = perc_formatter  
    ),
    align = c("l", "l", "c", "c", "c"),
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
                    "<b style='font-size: 20px;'>Lucky Charms</b><br>2024 NRL players for each team with the biggest <b style='color: #008B00;'>improvement</b> in win-rate when they play vs when they don't (min 5 games played, 5 games missed)" |> HTML()))
final_table1

saveWidget(final_table1, "tables/html/lucky_charms1.html")
webshot(url = "tables/html/lucky_charms1.html", 
        file = "tables/png/lucky_charms1.png", 
        selector = "body", zoom = 4,
        vwidth = 690)

# Final table formatting 2
final_table2 <- harbingers |>
  mutate(diff_when_in = paste0(ifelse(diff_when_in > 0, "+", ""), percent(diff_when_in, digits = 1))) |>
  select(team, full_name, record_in, record_out, diff_when_in) |>
  rename(
    Team = team,
    Player = full_name,
    `W-(D)-L<br>With` = record_in,
    `W-(D)-L<br>Without` = record_out,
    `&#x0394; Win-Rate` = diff_when_in
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Player = bold_formatter,
      `W-(D)-L<br>With` = num_formatter,
      `W-(D)-L<br>Without` = num_formatter,
      `&#x0394; Win-Rate` = perc_formatter  
    ),
    align = c("l", "l", "c", "c", "c"),
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
                    "<b style='font-size: 20px;'>Harbingers of Loss</b><br>2024 NRL players for each team with the biggest <b style='color: #FF3030;'>decline</b> in win-rate when they play vs when they don't (min 5 games played, 5 games missed)" |> HTML()))
final_table2

saveWidget(final_table2, "tables/html/lucky_charms2.html")
webshot(url = "tables/html/lucky_charms2.html", 
        file = "tables/png/lucky_charms2.png", 
        selector = "div", zoom = 4,
        vwidth = 690)
