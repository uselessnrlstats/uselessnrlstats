##### Description #####
# An R script to look at INITIAGAMI - unique player initials

##### Libraries #####
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

# extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")

fm <- 2

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

##### Helper Stats #####
ha_data <- match_data |>
  select(match_id, home_team, away_team) |>
  pivot_longer(c(home_team, away_team), names_to = "H_A", values_to = "team") |>
  mutate(H_A = ifelse(H_A == "home_team", "H", "A"))

##### Analysis #####
initial_data <- player_data |>
  select(player_id, full_name, first_name, last_name) |>
  filter(first_name != "?") |>
  mutate(first_name_initial = substr(first_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE),
         last_name_initial = substr(last_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE))

opposing_players <- player_match_data |>
  left_join(ha_data, by = c("match_id", "team")) |>
  select(match_id, player_id, team, H_A, number, position) |>
  left_join(player_data |> select(player_id, full_name, first_name), 
            by = "player_id") |>
  group_by(match_id, number, position) |>
  filter(n() > 1) |>
  arrange(desc(H_A)) |>
  summarise(
    home_team = team[1],
    away_team = team[2],
    full_name1 = full_name[1],
    first_name1 = first_name[1],
    full_name2 = full_name[2],
    first_name2 = first_name[2],
    .groups = "drop"
  ) |>
  mutate(initial1 = substr(first_name1, 1, 1) |> str_to_upper(),
         initial2 = substr(first_name2, 1, 1) |> str_to_upper()) |>
  left_join(match_data |> select(match_id, competition, competition_year, round, date), by = "match_id") |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "\\1", round)
  ) |>
  select(-competition_year) |>
  relocate(c(year, round), .after = match_id)

initial_counts <- opposing_players |>
  filter(first_name1 == first_name2) |>
  filter(str_detect(full_name1, first_name1),
         str_detect(full_name2, first_name2)) |>
  filter(nchar(first_name1) > 1) |>
  arrange(desc(date)) |>
  group_by(initial1) |>
  summarise(n = n(),
            most_recent = paste0(number[1], " ", position[1], ": ", full_name1[1], " / ", full_name2[1], " (", year[1], ")")) |>
  #filter(nchar(first_name1) > 1) |>
  arrange(desc(n)) |>
  mutate(initial1 = factor(initial1, levels = LETTERS)) |>
  complete(initial1, fill = list(n = 0, most_recent = ""))

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
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- initial_counts[14:26,] |>
  rename(
    `Initial` = initial1,
    Count = n,
    `Most recent pair w. same first name` = most_recent
  ) |>
  formattable(
    list(
      `Initial` = bold_formatter,
      Count = color_tile("transparent", "lightskyblue")
    ),
    align = c("c", "r", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table tr:nth-of-type(11n) td {background-color: #FFB90555;}"))

final_table

saveWidget(final_table, "tables/html/commentators_nightmare2.html")
webshot(url = "tables/html/commentators_nightmare2.html", 
        file = "tables/png/commentators_nightmare2.png", 
        selector = "body", zoom = 4,
        vwidth = 500)
