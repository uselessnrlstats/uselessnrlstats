##### Description #####
# An R script to look at players who reached 4 different teams quickest

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

##### Analysis #####
fewest_to_four <- player_match_data |>
  left_join(team_data, by = c("team" = "team_name")) |>
  left_join(match_data, by = "match_id") |>
  select(player_id, match_id, competition_year, date, team_unique, team_abbr) |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(match_number = row_number(),
         teams_so_far = map(1:n(), ~ list(unique(team_unique[1:.x]))),
         teams_so_far_abbr = map_chr(1:n(), ~ paste0(unique(team_abbr[1:.x]), collapse = ",")),
         team_count = map_int(1:n(), ~ length(unique(team_abbr[1:.x])))
         ) |>
  filter(team_count == 3) |>
  filter(match_number == min(match_number)) |>
  ungroup() |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(player_id, full_name, match_number, teams_so_far_abbr, team_count) |> 
  arrange(match_number)


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
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- most_decreasing_goals |>
  rename(
    Year = competition_year,
    Round = round,
    `Team` = team,
    `Opposition` = opposition_team,
    `Player` = full_name,
    `Goals` = goals,
    `Match #` = career_match
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      Goals = bold_formatter,
      `Team` = team_formatter,
      `Opposition` = team_formatter
    ),
    align = c("r", "c", "r", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 10px;",
                    "Sequentially Decreasing Goals")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 10px;",
                    "<b>David Brooks</b> once kicked 6,5,4,3,2,1 and 0 goals in 7 consecutive matches that he played in. This 7-match decreasing sequence is an NRL/NSWRFL record." |> HTML()))
final_table

saveWidget(final_table, "tables/html/decreasing_goals.html")
webshot(url = "tables/html/decreasing_goals.html", 
        file = "tables/png/decreasing_goals.png", 
        selector = "body", zoom = 4,
        vwidth = 700)
