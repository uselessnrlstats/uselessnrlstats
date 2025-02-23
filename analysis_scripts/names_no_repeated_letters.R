##### Description #####
# An R script to look at player names with no repeated letters

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

##### Analysis #####
names_no_repeats <- player_data |>
  filter(first_name != "?",
        !is.na(first_name),
        nchar(first_name) > 1) |>
  select(player_id, full_name) |>
  mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |>
           str_to_upper() |>
           str_split(pattern = "")) |>
  rowwise() |>
  mutate(n_letters = length(full_name_letters),
         n_unique_letters = length(unique(full_name_letters))) |>
  ungroup() |>
  filter(n_letters == n_unique_letters) |>
  arrange(desc(n_letters)) |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id")

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
final_table <- names_no_repeats |>
  rowwise() |>
  mutate(full_name_letters = paste(sort(full_name_letters), collapse = "")) |>
  filter(n_letters >= 12)
  arrange(desc(n_letters), career_years) |>
  select(full_name, career_years, vowels, n_letters) |>
  rename(
    `Player Name` = full_name,
    Career = career_years,
    Vowel = vowels,
    `Number of Letters` = n_letters
  ) |>
  formattable(
    list(
      `Player Name` = bold_formatter,
      `Number of Letters` = bold_formatter
    ),
    align = c("r", "l", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Longest Names with One Vowel")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL players with the longest names that only use one of the five vowels."))
final_table

saveWidget(final_table, "tables/html/names_one_vowel.html")
webshot(url = "tables/html/names_one_vowel.html", 
        file = "tables/png/names_one_vowel.png", 
        selector = "body", zoom = 4,
        vwidth = 450)
