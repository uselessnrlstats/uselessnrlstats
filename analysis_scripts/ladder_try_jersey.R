##### Description #####
# An R script to look at a ladder involving only bulldogs games since 2019

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
}

##### Load Data #####
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")
ladder_data <- read_csv("cleaned_data/nrl/ladder_round_data.csv")

##### Helper Stats #####
match_points <- player_match_data |>
  mutate(tries = tries + penalty_tries) |>
  select(match_id, team, number, tries, goals, field_goals, field_goals2, penalty_tries) |>
  group_by(match_id, team) |>
  summarise(try_jersey_points = sum(tries * number),
            total_goals = sum(goals),
            total_field_goals = sum(field_goals),
            total_field_goals2 = sum(field_goals2),
            .groups = "drop") |>
  left_join(match_data |> select(match_id, date), by = "match_id") |>
  mutate(year = year(date),
         points = case_when(
           year < 1971 ~ try_jersey_points + 2 * (total_goals + total_field_goals),
           year >= 1971 & year < 1983 ~ try_jersey_points + 2 * total_goals + 1 * total_field_goals,
           year >= 1983 ~ try_jersey_points + 2 * (total_goals + total_field_goals2) + 1 * total_field_goals
         )) |>
  select(match_id, team, points)

###### Identify Teams in Each Season ######
season_teams <- match_data |>
  group_by(competition_year) |>
  summarise(teams = list(sort(unique(c(home_team, away_team)))),
            .groups = "drop")

###### Clean Data for each team ######
team_match_data <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, round, home_team, away_team) |>
  left_join(match_points, by = c("match_id", "home_team" = "team")) |> 
  rename(home_team_score = points) |>
  left_join(match_points, by = c("match_id", "away_team" = "team")) |> 
  rename(away_team_score = points) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(competition_year, year, round, home_away, team, score_for, score_against, result) |>
  filter(grepl("Round", round)) |>
  mutate(round = gsub("Round (\\d+)", "\\1", round) |> as.numeric())

###### Identify Byes ######
bye_team_rounds <- team_match_data |>
  group_by(competition_year, round) |>
  summarise(teams_played = list(sort(unique(c(team)))),
            .groups = "drop") |>
  left_join(season_teams, by = "competition_year") |>
  rowwise() |>
  mutate(bye_team = list(teams[which(!(teams %in% teams_played))])) |>
  unnest(bye_team) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         home_away = NA,
         score_for = NA,
         score_against = NA,
         result = "B") |>
  rename(team = bye_team) |>
  select(competition_year, year, round, home_away, team, score_for, score_against, result)

###### Produce ladders ######
round_ladders <- team_match_data |>
  rbind(bye_team_rounds) |>
  arrange(year, round) |>
  ##### Apply special cases
  # Remove second South Sydney bye in 1908
  mutate(result = ifelse(year == 1908 & round == 4 & team == "South Sydney Rabbitohs", NA, result)) |>
  # Apply Points
  mutate(points = case_when(
    result == "W" ~ 2,
    result == "L" ~ 0,
    result == "D" ~ 1,
    result == "B" ~ 2,
    .default = 0
  )) |>
  group_by(competition_year, year, round, team) |>
  summarise(points = sum(points, na.rm = TRUE),
            played = sum(result %in% c("W", "L", "D"), na.rm = TRUE),
            wins = sum(result == "W", na.rm = TRUE),
            losses = sum(result == "L", na.rm = TRUE),
            draws = sum(result == "D", na.rm = TRUE),
            byes = sum(result == "B", na.rm = TRUE),
            score_for = sum(score_for, na.rm = TRUE),
            score_against = sum(score_against, na.rm = TRUE),
            .groups = "drop") |>
  arrange(year, round) |>
  group_by(competition_year, year, team) |>
  mutate(points = cumsum(points),
         played = cumsum(played),
         wins = cumsum(wins),
         losses = cumsum(losses),
         draws = cumsum(draws),
         byes = cumsum(byes),
         score_for = cumsum(score_for),
         score_against = cumsum(score_against),
         score_diff = score_for - score_against) |>
  select(competition_year, year, round, team, points, played, wins, losses, draws, byes, score_for, score_against, score_diff) |>
  arrange(year, round, desc(points), desc(score_diff), desc(score_for), desc(score_against)) |>
  group_by(competition_year, round) |>
  mutate(ladder_position = row_number()) |>
  ungroup() |>
  relocate(ladder_position, .after = round)

##### Analysis #####
ladder2024 <- ladder_data |>
  filter(year == 2024) |>
  filter(round == max(round)) |>
  mutate(pos_real = row_number()) |>
  select(pos_real, team)

ladder_tries2024 <- round_ladders |>
  filter(year == 2024) |>
  filter(round == max(round)) |>
  arrange(desc(points), desc(score_diff), desc(score_for), desc(score_against)) |>
  mutate(pos = row_number()) |>
  select(pos, team, played, wins, losses, draws, byes, score_diff, points) |>
  left_join(ladder2024, by = "team")
  
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
              font.size = "13px",
              font.family = "Montserrat"
            ))

xnormalize <- function(x) {
  x <- c(x, -400, 450)
  normalize(x)[1:(length(x) - 2)]
}

pd_formatter <-
  formatter("span", style = function(x) {
    colours <- gsub("%", "", x) |>
      as.numeric() |>
      xnormalize() |>
      colorRamp(c("firebrick1",'white', "green4"))() |>
      as.integer() |>
      matrix(byrow = TRUE, dimnames = list(c("red", "green", "blue"), NULL), nrow = 3) |>
      csscolor()
    style(display = "block",
          'text-align' = 'right',
          padding = "2px 4px 2px 4px", 
          `border-radius` = "4px",
          color = ifelse(luminance(colours) < 0.52, "#F2F2F2", "#1A1A1A"),
          `background-color` = colours,
          font.size = "13px",
          font.family = "Montserrat")
  })

# Final table formatting
final_table <- ladder_tries2024 |>
  mutate(
    change = pos_real - pos,
    change = case_when(
      change > 0 ~ paste0("<b style='color: #008B00; font-family: Montserrat'> <span style='font-size: 16px'>\u25B2</span>", change, "</b>"),
      change < 0 ~ paste0("<b style='color: #FF3030; font-family: Montserrat'> <span style='font-size: 16px'>\u25BC</span>", abs(change), "</b>"),
      change == 0 ~ paste0("<b style='color: #777777;'>\uFF0D</b>"),
    ),
    record = paste0(wins, "-",
                    ifelse(draws > 0, paste0(draws, "-"), ""), 
                    losses)) |>
  select(pos, change, team, points, played, record, score_diff) |>
  rename(
    `#` = pos,
    ` ` = change,
    `Team` = team,
    Pts = points,
    `Pld` = played,
    `W-D-L` = record,
    PD = score_diff
  ) |>
  formattable(
    list(
      `#` = num_formatter,
      `Team` = team_formatter,
      Pts = num_formatter,
      Pld = num_formatter,
      `W-D-L` = num_formatter,
      PD = pd_formatter
    ),
    align = c("r", "l", "l", "c", "c", "c", "r"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style(paste0("table tr:nth-child(9) td {border-top: 2px solid #120b2f;}"))) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 18px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "2024 NRL Ladder if tries are worth the jersey number of the try-scorer"))
final_table

saveWidget(final_table, "tables/html/ladder_try_jersey.html")
webshot(url = "tables/html/ladder_try_jersey.html", 
        file = "tables/png/ladder_try_jersey.png", 
        selector = "div", zoom = 4,
        vwidth = 485)
