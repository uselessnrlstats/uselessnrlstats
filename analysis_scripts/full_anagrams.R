##### Description #####
# An R script to look at player names that are anagrams of a full word

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(hash)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

dict1 <- readLines("analysis_scripts/extra_data/dict_list_enable.txt") %>%
  gsub("[^a-zA-ZÀ-ž]", "", .) |>
  str_to_lower() |>
  unique()
dict2 <- readLines("analysis_scripts/extra_data/dict_list_scowl_large.txt") %>%
  gsub("[^a-zA-ZÀ-ž]", "", .) |>
  str_to_lower() |>
  unique()

##### Helper Stats #####
# Career years
player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(debut = min(date),
            first_year = min(year),
            last_year = max(year),
            final_team = team[max(row_number())]) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

# Build dictionary of words
dict1_env <- new.env(hash = TRUE, parent = emptyenv())
dict2_env <- new.env(hash = TRUE, parent = emptyenv())

walk(
  .x = dict1,
  .f = function(x) {
    hashword <- strsplit(x, "") |> unlist() |> sort() |> paste(collapse = "")
    if (!is.null(dict1_env[[hashword]])) {
      dict1_env[[hashword]] <- c(dict1_env[[hashword]], x)
    } else {
      dict1_env[[hashword]] <- list(x)
    }
    return()
  },
  .progress = TRUE
)

walk(
  .x = dict2,
  .f = function(x) {
    hashword <- strsplit(x, "") |> unlist() |> sort() |> paste(collapse = "")
    if (!is.null(dict2_env[[hashword]])) {
      dict2_env[[hashword]] <- c(dict2_env[[hashword]], x)
    } else {
      dict2_env[[hashword]] <- list(x)
    }
    return()
  },
  .progress = TRUE
)

# retrieval function
get_anagrams <- function(x, dict_env) {
  word_letters <- strsplit(x, "") |> unlist() |> sort() |> paste(collapse = "")
  return(dict_env[[word_letters]])
}

##### Analysis #####
full_anagrams2 <- player_data |>
  select(player_id, full_name, first_name, last_name, total_matches) |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  rowwise() |>
  mutate(full_name_letters = gsub("[^a-zA-ZÀ-ž]", "", full_name) |> 
           str_to_lower() |>
           str_split(pattern = "") |>
           unlist() |> 
           sort() |> 
           paste(collapse = "")) |>
  rowwise() |>
  mutate(anagram = (function(x) {
                    result <- get_anagrams(x, dict2_env)
                    if (is.null(result)) {
                      return(NA)
                    } else {
                      return(result |> unlist())
                    }
                  })(full_name_letters) |> list()) |>
  filter(all(!is.na(anagram))) |>
  left_join(player_career_years |> select(player_id, career_years, debut, last_year, final_team), by = "player_id") |>
  arrange(desc(career_years)) |>
  mutate(n_letters = nchar(full_name_letters))


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
                       text_col = ifelse(lum < 0.45, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- full_anagrams2 |>
  ungroup() |>
  filter(last_year == 2024) |>
  arrange(debut) |>
  mutate(
  #  full_name = "?",
    ` ` = paste0(row_number(), "."),
    `  ` = paste0(row_number(), ".")) |>
  select(` `, career_years, total_matches, final_team, `  `, anagram, full_name) |>
  rowwise() |>
  mutate(
    anagram = paste(anagram, collapse = " | ") |> toupper()) |>
  rename(
    `Career` = career_years,
    `# Matches` = total_matches,
    `Current Team` = final_team,
    `Anagram` = anagram,
    `Player` = full_name
  ) |>
  formattable(
    list(
      `Current Team` = team_formatter,
      Anagram = bold_formatter,
      Player = bold_formatter,
      ` ` = bold_formatter,
      `  ` = bold_formatter
    ),
    align = c("r", "c", "c", "l", "r", "c", "c"),
    table.attr = 'style="font-size: 16px; font-family: Roboto"'
  ) |>
  as.htmlwidget()
final_table

saveWidget(final_table, "tables/html/anagrams1b.html")
webshot(url = "tables/html/anagrams1b.html", 
        file = "tables/png/anagrams1b.png", 
        selector = "body", zoom = 10,
        vwidth = 1050)
 
