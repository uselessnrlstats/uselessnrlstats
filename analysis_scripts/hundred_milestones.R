##### Description #####
# An R script to look at pi related stats

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

##### Helper Stats #####

##### Analysis #####
hundreds <- player_match_data |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  left_join(match_data |> select(match_id, competition_year, round, date),
            by = "match_id") |>
  group_by(player_id) |>
  arrange(date) |>
  mutate(game_number = row_number()) |>
  filter(game_number %% 100 == 0) |>
  group_by(competition_year, round) |>
  mutate(hundred_games = all(c(100,200,300) %in% game_number)) |>
  filter(hundred_games) |>
  select(competition_year, round, full_name, game_number) |>
  arrange(game_number) |>
  pivot_wider(id_cols = c(competition_year, round), 
              names_from = game_number, values_from = full_name,
              values_fn = ~ paste(.x, collapse = "<br>"))
  # rbind(
  #   tibble(
  #     competition_year = "NRL 2024",
  #     round = "Round 3",
  #     `100` = "Corey Jensen<br>Hudson Young<br>Max King<br>Taniela Paseka",
  #     `200` = "Reagan<br>Campbell-Gillard",
  #     `300` = "Jared<br>Waerea-Hargreaves"
  #   )
  # )

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
final_table <- hundreds |>
  mutate(round = gsub("Round (\\d+)", "\\1", round) |> as.numeric()) |>
  rename(
    Year = competition_year,
    Round = round,
    `100th Game` = `100`,
    `200th Game` = `200`,
    `300th Game` = `300`
  ) |>
  formattable(
    list(),
    align = c("r", "c", "l", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "100, 200, 300...")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL Rounds with 100, 200 and 300 game milestones"))
final_table

saveWidget(final_table, "tables/html/hundred_milestones.html")
webshot(url = "tables/html/hundred_milestones.html", 
        file = "tables/png/hundred_milestones.png", 
        selector = "body", zoom = 4,
        vwidth = 640)
