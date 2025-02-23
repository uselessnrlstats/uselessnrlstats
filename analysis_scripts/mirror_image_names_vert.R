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

mirror_lines_lower <- c("b", "d", "i", "l", "m", "n", "o", "p", "q", "t", "u", "v", "w", "x")
mirror_lines_upper <- c("A", "H", "I", "M", "O", "T", "U", "V", "W", "X", "Y")

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
mirror_names <- player_data |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  select(player_id, first_name, last_name, full_name) |>
  mutate(first_name_upper = gsub("[^a-zA-Z]", "", first_name) |> 
           str_to_upper() |>
           str_split(pattern = ""),
         last_name_upper = gsub("[^a-zA-Z]", "", last_name) |> 
           str_to_upper() |>
           str_split(pattern = "")) |>
  rowwise() |>
  mutate(
    first_name = first_name |> str_to_upper(),
    last_name = last_name |> str_to_upper(),
    full_name = full_name |> str_to_upper(),
    first_name_mirror = all(first_name_upper %in% mirror_lines_upper),
    last_name_mirror = all(last_name_upper %in% mirror_lines_upper)
  ) |>
  filter(first_name_mirror | last_name_mirror) |>
  select(player_id, full_name) |>
  rowwise() |>
  mutate(full_name_rev = str_split(full_name, pattern = "")[[1]] |> rev() |> paste(collapse = "")) |>
  ungroup()

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
