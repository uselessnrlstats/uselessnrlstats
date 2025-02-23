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
  remotes::install_github("solarchemist/periodicdata")
  library(periodicdata)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

elements <- periodicdata::values |> pull(2)
elements_upper <- toupper(elements)

##### Helper Stats #####
# Career years
player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year),
            final_team = team[max(row_number())]) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

# Build checking function
periodic_check <- function(string) {
  # Recursive helper function
  spell_helper <- function(done_string, todo_string) {
    if (todo_string == "") {
      return(done_string)
    }
    
    for (i in 2:1) {
      if (nchar(todo_string) >= i) {
        prefix <- substr(todo_string, 1, i)
        if (prefix %in% elements_upper) {
          done_string_temp <- paste(done_string, elements[which(prefix == elements_upper)])
          result <- spell_helper(done_string_temp, substr(todo_string, i + 1, nchar(todo_string)))
          if (!is.na(result)) {
            done_string <- done_string_temp
            return(result)
          }
        }
      }
    }
    return(NA)
  }
  
  return(spell_helper("", string))
}

##### Analysis #####
periodic_names <- player_data |>
  select(player_id, full_name, first_name, last_name) |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  rowwise() |>
  mutate(first_name_per = gsub("[^a-zA-ZÀ-ž]", "", first_name) |> 
           str_to_upper() |> str_split(pattern = "") |> unlist() |>  paste(collapse = ""),
         last_name_per = gsub("[^a-zA-ZÀ-ž]", "", last_name) |> 
           str_to_upper() |> str_split(pattern = "") |> unlist() |>  paste(collapse = ""),
         full_name_per = gsub("[^a-zA-ZÀ-ž]", "", full_name) |> 
           str_to_upper() |> str_split(pattern = "") |> unlist() |>  paste(collapse = "")) |>
  rowwise() |>
  mutate(first_name_per = periodic_check(first_name_per),
         last_name_per = periodic_check(last_name_per),
         full_name_per = periodic_check(full_name_per)) |>
  filter((!is.na(first_name_per) & !is.na(last_name_per)) | !is.na(full_name_per)) |>
  left_join(player_career_years |> select(player_id, career_years, last_year, final_team), by = "player_id") |>
  arrange(desc(nchar(full_name_per))) |>
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
              font.weight = "bold",
              font.size = "12.5px"
            ))

# Final table formatting
final_table <- periodic_names |>
  filter(last_year == 2024) |>
  select(career_years, full_name_per, final_team) |>
  arrange(final_team) |>
  # filter(row_number() <= 15) |>
  filter(row_number() > 15) |>
  rename(
    `Career` = career_years,
    `Elemental Name` = full_name_per,
    `Team` = final_team
  ) |>
  formattable(
    list(
      `Team` = team_formatter,
      `Elemental Name` = bold_formatter
    ),
    align = c("c", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}"))
  
final_table

saveWidget(final_table, "tables/html/periodic2.html")
webshot(url = "tables/html/periodic2.html", 
        file = "tables/png/periodic2.png", 
        selector = "body", zoom = 4,
        vwidth = 500)
