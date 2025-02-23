##### Description #####
# An R script to look at players who debuted in rd1

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

##### Analysis #####
rd1_debuts <- player_match_data |>
  select(player_id, match_id, team, opposition_team, number, position) |>
  left_join(match_data |> select(match_id, competition, competition_year, round, date, home_team, home_team_score, away_team, away_team_score),
            by = "match_id") |>
  mutate(win_loss = case_when(
    team == home_team & home_team_score > away_team_score ~ "W",
    team == home_team & home_team_score < away_team_score ~ "L",
    team == home_team & home_team_score == away_team_score ~ "D",
    team == away_team & home_team_score > away_team_score ~ "L",
    team == away_team & home_team_score < away_team_score ~ "W",
    team == away_team & home_team_score == away_team_score ~ "D",
    .default = NA
  )) |>
  select(-c(home_team, home_team_score, away_team, away_team_score)) |>
  arrange(date) |>
  group_by(player_id) |>
  filter(row_number() == 1) |>
  ungroup() |>
  filter(round == "Round 1", competition == "NRL") |>
  left_join(team_data |> select(team_name, team_unique), 
            by = c("team" = "team_name")) |>
  left_join(player_data |> select(player_id, full_name),
            by = "player_id") |>
  arrange(date) |>
  group_by(team_unique) |>
  summarise(count = n(),
            count_starting = sum(position != "B"),
            players = paste0(full_name, collapse = "<br> ")) |>
  arrange((count)) |>
  filter(!(team_unique %in% c("North Sydney Bears", "St George Dragons", "Balmain Tigers", "Gold Coast Chargers", "Western Suburbs Magpies")))
  
##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `vertical.align` = "center",
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

number_formatter <-
  formatter("span",
            style = x ~ style(
              font.size = "12px"
            ))

# Final table formatting
final_table <- rd1_debuts |>
  select(-players) |>
  rename(
    Team = team_unique,
    `Rd1 Debuts` = count,
    `Rd1 Debuts<br>(starting)` = count_starting
  ) |>
  formattable(
    list(
      Team = team_formatter,
      `Rd1 Debuts` = bold_formatter
    ),
    align = c("r", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica; vertical-align: middle"'
  )  |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Rd1 Debuts")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "The number of Rd1 debuts handed out by each of the 17 current NRL clubs in the NRL era (1998-), and the number of those that were in a starting position (1-13)."))
final_table

saveWidget(final_table, "tables/html/rd1_debuts.html")
webshot(url = "tables/html/rd1_debuts.html", 
        file = "tables/png/rd1_debuts.png", 
        selector = "body", zoom = 4,
        vwidth = 550)
