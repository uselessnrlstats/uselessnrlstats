##### Description #####
# An R script to look at teams that came from last in Rd13 to finish higher up

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
ladder_data <- read_csv("cleaned_data/nrl/ladder_round_data.csv") |>
  group_by(competition_year, year, round) |>
  mutate(ladder_position = row_number()) |>
  ungroup()

##### Helper Stats #####
# Summarise each team and round 
ladder_data_2 <- ladder_data |>
  mutate(pts_record = paste0(points, " (", paste(wins, draws, losses, sep = "-"), ")")) |>
  select(competition_year, year, round, ladder_position, team, played, pts_record)

season_summary <- ladder_data |>
  group_by(competition_year, year) |>
  summarise(round = max(round), 
            teams = length(unique(team)),
            .groups = "drop") |>
  arrange(desc(year))

##### Analysis #####
# Teams in last in Rd13
teams_in_last13 <- ladder_data_2 |>
  group_by(competition_year, year) |>
  filter(ladder_position == max(ladder_position), round == 13) |>
  ungroup() |>
  rename(ladder_position_rd13 = ladder_position, pts_record_rd13 = pts_record) |>
  select(-round, -played)
  
final_pos <- ladder_data_2 |>
  group_by(competition_year, year) |>
  filter(round == max(round)) |>
  ungroup() |>
  left_join(teams_in_last13, by = c("competition_year", "year", "team")) |>
  filter(!is.na(ladder_position_rd13)) |>
  arrange(desc(ladder_position_rd13 - ladder_position)) |>
  select(competition_year, team, ladder_position_rd13, pts_record_rd13, ladder_position, pts_record) |>
  rename(
    Year = competition_year,
    Team = team,
    `Pos. Rd13` = ladder_position_rd13,
    `Pts (W-D-L) Rd13` = pts_record_rd13,
    `Pos. Final` = ladder_position,
    `Pts (W-D-L) Final` = pts_record
  ) |>
  mutate(
    `Pos. Rd13` = ordinal(`Pos. Rd13`),
    `Pos. Final` = ordinal(`Pos. Final`),
  ) |>
  filter(Year != "NRL 2024")
  
##### #####
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
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- final_pos |>
  filter(row_number() <= 6) |>
  formattable(
    list(
      `Team` = team_formatter,
      `Pos. Rd13` = bold_formatter,
      `Pos. Final` = bold_formatter
    ),
    align = c("l", "r", "c", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 10px;",
                    "Improving from Last in Rd13")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 10px;",
                    "NRL/NSWRL teams that were last on the ladder in Rd13 and managed to improve by 4 or more places by the end of the season."))
final_table

saveWidget(final_table, "tables/html/rd13_last_to_finals.html")
webshot(url = "tables/html/rd13_last_to_finals.html", 
        file = "tables/png/rd13_last_to_finals.png", 
        selector = "body", zoom = 4,
        vwidth = 750)
