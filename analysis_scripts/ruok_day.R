##### Description #####
# An R script to look at players with the letters RUOK in their name

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
GOAT <- c("G", "O", "A", "T")

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
player_data |>
  filter(first_name != "?",
        !is.na(first_name),
        nchar(first_name) > 1) |>
  select(player_id, full_name) |>
  mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |>
           str_to_upper() |>
           str_split(pattern = "")) |>
  rowwise() |>
  mutate(n_letters = length(full_name_letters),
         uniq_goat = list(GOAT[which(GOAT %in% full_name_letters)]),
         goat = list(full_name_letters[which(full_name_letters %in% GOAT)]),
         n_uniq_goat = length(uniq_goat),
         n_goat = length(goat)) |>
  filter(n_goat == 4, n_uniq_goat == 4) |>
  filter(paste0(goat, collapse = "") == "GOAT") |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id") |>
  ungroup() |>
  arrange(n_letters, career_years)
  

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
final_table <- alternating_vowels |>
  select(career_years, full_name, n_letters) |>
  mutate(full_name = toupper(full_name)) |>
  filter(n_letters >= 12) |>
  rename(
    `Player Name` = full_name,
    Career = career_years,
    `# Letters` = n_letters
  ) |>
  formattable(
    list(
      `Player Name` = bold_formatter,
      `# Letters` = bold_formatter
    ),
    align = c("r", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Alternating Vowels/Consonants")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL players with the longest names that consist of alternating vowels and consonants."))
final_table

saveWidget(final_table, "tables/html/names_alternating_vowels.html")
webshot(url = "tables/html/names_alternating_vowels.html", 
        file = "tables/png/names_alternating_vowels.png", 
        selector = "body", zoom = 4,
        vwidth = 500)
