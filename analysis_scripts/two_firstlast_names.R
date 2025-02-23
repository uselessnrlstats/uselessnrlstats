##### Description #####
# An R script to look at unique names or names with first/last names

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
  ))

player_career_summaries <- player_match_data |>
  group_by(player_id) |>
  summarise(total_matches = n(),
            teams = list(unique(team)),
            numbers = list(unique(number, na.rm = TRUE)),
            positions = list(unique(position, na.rm = TRUE)),
            captaincy = sum(captain),
            total_tries = sum(tries + penalty_tries),
            total_goals = sum(goals),
            total_goal_attempts = sum(goal_attempts, na.rm = TRUE),
            total_field_goals = sum(field_goals),
            total_field_goals2 = sum(field_goals2),
            total_sin_bins = sum(sin_bins5 + sin_bins),
            total_send_offs = sum(send_offs),
            total_points = sum(points))

##### Analysis #####
player_data |>
  filter(first_name %in% last_name,
         last_name %in% first_name) |>
  select(player_id, full_name, first_name, last_name, birthday) |>
  left_join(player_career_summaries, by = "player_id") |>
  View()

player_data |>
  group_by(first_name) |>
  mutate(first_name_n = n()) |>
  ungroup() |>
  group_by(last_name) |>
  mutate(last_name_n = n()) |>
  ungroup() |>
  filter(first_name_n == 1, last_name_n == 1) |>
  select(player_id, full_name, first_name, last_name, birthday) |>
  left_join(player_career_summaries, by = "player_id") |>
  View()

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
final_table <- six_unique_letters |>
  filter(row_number() <= 15) |>
  mutate(
    full_name = toupper(full_name),
    career_years = gsub("-2023", "-", career_years),
    career_years = gsub("^2021$", "2021-", career_years),
    unique_letters = paste0(n_unique_letters, ": ", unique_letters)) |>
  select(-n_unique_letters) |>
  rename(
    `Player Name` = full_name,
    Career = career_years,
    `Unique Letters` = unique_letters,
    `Name Length` = n_letters,
  ) |>
  formattable(
    list(
      `Player Name` = bold_formatter,
      `Name Length` = bold_formatter
    ),
    align = c("r", "l", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Names with \u22646 unique letters")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "The longest NRL/NSWRL player names that consist of 6 or less unique letters"))
final_table

saveWidget(final_table, "tables/html/longest_unique_6.html")
webshot(url = "tables/html/longest_unique_6.html", 
        file = "tables/png/longest_unique_6.png", 
        selector = "body", zoom = 4,
        vwidth = 475)
