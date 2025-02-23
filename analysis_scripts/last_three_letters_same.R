##### Description #####
# An R script to look at team lists with the most letters

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
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
ref_data <- read_csv("cleaned_data/nrl/ref_data.csv")
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
last_three_letters <- player_data |>
  filter(first_name != "?",
        !is.na(first_name),
        nchar(first_name) > 1) |>
  select(player_id, first_name, last_name, full_name) |>
  mutate(first_name_letters = gsub("[^a-zA-Z]", "", first_name) |>
           str_to_upper() |> str_split(pattern = ""),
         last_name_letters = gsub("[^a-zA-Z]", "", last_name) |>
           str_to_upper() |> str_split(pattern = "")) |>
  rowwise() |>
  mutate(first_name_length = length(first_name_letters),
         last_name_length = length(last_name_letters)) |>
  filter(identical(
    first_name_letters[(first_name_length - 2):first_name_length],
    last_name_letters[(last_name_length - 2):last_name_length]
  )) |>
  mutate(last_three = toupper(last_name_letters[(last_name_length - 2):last_name_length]) |> list()) |>
  select(-c(first_name_length, last_name_length)) |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id") |>
  left_join(player_career_summaries, by = "player_id") |>
  arrange(career_years) |>
  mutate(full_name = toupper(full_name),
         full_name = gsub(paste0(last_three, collapse = ""),
                          paste0("<b><u>", paste0(last_three, collapse = ""), "</u></b>"),
                          full_name, ignore.case = TRUE)) |>
  select(career_years, full_name, total_matches)

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
final_table <- last_three_letters |>
  rename(
    `Player Name` = full_name,
    Career = career_years,
    Matches = total_matches
  ) |>
  formattable(
    list(
    ),
    align = c("r", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("table td:nth-of-type(3n) { width: 160px;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 5px;",
                    "Last 3 Letters")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 5px;",
                    "NRL/NSWRL players whose first and last names end with the same 3-letter sequence"))
final_table

saveWidget(final_table, "tables/html/last_three_letters.html")
webshot(url = "tables/html/last_three_letters.html", 
        file = "tables/png/last_three_letters.png", 
        selector = "body", zoom = 4,
        vwidth = 480)
