##### Description #####
# An R script to look at team records in afternoon games

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(scales)
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
  select(match_id, competition_year, year, round, time, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  left_join(match_data |> select(match_id, home_team, away_team), by = "match_id") |>
  mutate(opposition = ifelse(team == home_team, away_team, home_team)) |>
  select(-c(home_team, away_team)) |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, round, time, home_away, team, opposition, score_for, score_against, result) |>
  left_join(team_data |> select(team_name, team_unique), by = c("team" = "team_name")) |>
  left_join(team_data |> select(team_name, team_unique) |> rename(opp_unique = team_unique), by = c("opposition" = "team_name"))

current_teams <- match_data |>
  filter(competition_year == "NRL 2023") |>
  distinct(away_team) |>
  rename(team_name = away_team) |>
  left_join(team_data |> select(team_name, team_unique, team_mascots),
            by = c("team_name")) |>
  left_join(team_logos, by = "team_unique") |>
  arrange(team_unique)

##### Analysis #####
wins_table <- match_results |>
  filter(time <= hms("16:30:00"), year >= 2007) |>
  group_by(team_unique) |>
  summarise(played = sum(result %in% c("W", "L", "D"), na.rm = TRUE),
            wins = sum(result == "W", na.rm = TRUE),
            losses = sum(result == "L", na.rm = TRUE),
            draws = sum(result == "D", na.rm = TRUE),
            score_for = sum(score_for, na.rm = TRUE),
            score_against = sum(score_against, na.rm = TRUE),
            score_diff = score_for - score_against,
            .groups = "drop") |>
 # filter(team_unique %in% current_teams$team_unique) |>
  mutate(win_perc = percent(round(wins / played, 4))) |>
  arrange(desc(wins / played), desc(score_diff), desc(score_for), desc(score_against)) |>
  mutate(pos = row_number()) |>
  select(pos, team_unique, played, win_perc, wins, losses, draws, score_diff)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `background-color` = data.frame(team_unique = x) |>
                left_join(team_logos, by = "team_unique") |>
                pull(team_colour),
              color = data.frame(team_unique = x) |>
                left_join(team_logos, by = "team_unique") |>
                mutate(lum = luminance(team_colour),
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

xnormalize <- function(x) {
  x <- c(x, 0, 100)
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
          'text-align' = 'center',
          padding = "0 4px", 
          `border-radius` = "4px",
          color = ifelse(luminance(colours) < 0.35, "#F2F2F2", "#1A1A1A"),
          `background-color` = colours,
          font.weight = "bold")
  })

# Final table formatting
final_table <- wins_table |>
  mutate(
    pos = ordinal(pos),
    record = paste(wins, draws, losses, sep = "-")) |>
  select(pos, team_unique, played, record, score_diff, win_perc) |>
  rename(
    `Pos.` = pos,
    `Team` = team_unique,
    `Pld` = played,
    `W-D-L` = record,
    PD = score_diff,
    `Win %` = win_perc
  ) |>
  formattable(
    list(
      `Team` = team_formatter,
      `Win %` = pd_formatter
    ),
    align = c("r", "r", "c", "c", "r", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "<b>Winter Solstice</b><br>NRL records of current teams on the Winter Solstice" |> HTML()))
final_table

saveWidget(final_table, "tables/html/winter_solstice.html")
webshot(url = "tables/html/winter_solstice.html", 
        file = "tables/png/winter_solstice.png", 
        selector = "body", zoom = 4,
        vwidth = 700)
