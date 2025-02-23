##### Description #####
# An R script to look at players who have all the letters of POTATO in their names

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
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
# Anagram Solver
anagram_finder <- function(word, player) {
  word_split <- sort(strsplit(word |> tolower(), "")[[1]]) |> table()
  player_split <- sort(strsplit(player |> tolower(), "")[[1]]) |> table()
  
  has_word <- ifelse(
    all(names(word_split) %in% names(player_split)),
    ifelse(all(player_split[names(word_split)] >= word_split), TRUE, FALSE),
    FALSE)
}

# Player Career year Span
player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year),
            total_matches = n()) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

##### Analysis #####
potatoes <- player_data |>
  rowwise() |>
  mutate(has_word = anagram_finder("potato", full_name)) |>
  filter(has_word) |>
  select(player_id, full_name) |>
  left_join(player_career_years, by = "player_id") |>
  select(career_years, full_name, total_matches) |>
  arrange(career_years)

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

fixed_width_formatter <- 
  formatter(.tag = "html",
            style = style(
              width = "150px"
            ))

# Final table formatting
final_table <- potatoes |>
  rename(
    Career = career_years,
    Player = full_name,
    `# Matches` = total_matches
  ) |>
  formattable(
    list(
      Player = bold_formatter
    ),
    align = c("r", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-family: Roboto; margin-left: 10px; font-size: 16px",
                    "NSWRL/NRL Players with all the letters of 'POTATO' in their name"))

final_table

saveWidget(final_table, "tables/html/world_potato_day.html")
webshot(url = "tables/html/world_potato_day.html", 
        file = "tables/png/world_potato_day.png", 
        selector = "body", zoom = 4,
        vwidth = 375)
