##### Description #####
# An R script to look at players whose names are made entirely of straight lines

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

##### Helper Stats #####
player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year)) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  )) |>
  select(player_id, career_years)

##### Analysis #####
player_match_data |>
  left_join(match_data |> select(match_id, round), by = "match_id") |>
  select(match_id, player_id, round, goals) |>
  filter(grepl("Round", round)) |>
  mutate(round = gsub("Round (\\d+)", "\\1", round) |> as.numeric()) |>
  mutate(round_odd = (round %% 2) |> as.logical(),
         goals_odd = (goals %% 2) |> as.logical()) |>
  group_by(player_id) |>
  summarise(
    n_games = n(),
    n_goals = sum(goals),
    n_same = sum(round_odd == goals_odd),
    prop = n_same / n_games,
    .groups = "drop"
  ) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  left_join(player_career_years |> select(player_id, career_years), by = "player_id") |>
  select(full_name, career_years, n_games, n_same, prop, n_goals) |>
  arrange(desc(prop)) |>
  filter(n_games >= 100, n_goals >= 50, grepl("2024", career_years))

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
final_table <- mirror_names |>
  left_join(player_career_years, by = "player_id") |>
  select(career_years, full_name, full_name_rev) |>
  rename(
    `Career` = career_years,
    `Player Name` = full_name,
    `Player Name (Mirrored)` = full_name_rev
  ) |>
  formattable(
    list(
      `Player Name` = bold_formatter,
      `Player Name (Mirrored)` = bold_formatter
    ),
    align = c("r", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Mirror Names")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NSWRL/NRL player names where each letter has vertical symmetry (in capitals)"))
final_table

saveWidget(final_table, "tables/html/mirror_names.html")
webshot(url = "tables/html/mirror_names.html", 
        file = "tables/png/mirror_names.png", 
        selector = "body", zoom = 4,
        vwidth = 400)
