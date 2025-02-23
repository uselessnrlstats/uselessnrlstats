##### Description #####
# An R script to look at player names with the longest string of alphabetical letters

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
         letter_indices = list(which(LETTERS %in% full_name_letters)),
         highest_letter = LETTERS[min(letter_indices)],
         lowest_letter = LETTERS[max(letter_indices)],
         letter_range = range(letter_indices) |> diff()) |>
  ungroup() |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id") |>
  arrange(letter_range, desc(n_letters), desc(career_years))

alphabetical_strings_rev <- player_data |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  select(player_id, full_name) |>
  mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |>
           str_to_upper()) |>
  rowwise() |>
  mutate(longest_alph_list = list(longest_alph_string(full_name_letters, reverse = TRUE, repeats = TRUE)),
         longest_alph = paste(longest_alph_list, collapse = ""),
         n_letters = nchar(longest_alph)) |>
  ungroup() |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id") |>
  arrange(desc(n_letters), desc(career_years))

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

# Final table formatting 1
final_table <- alphabetical_strings |>
  rowwise() |>
  mutate(
    regex_segment = paste0("(", paste(longest_alph_list, collapse = ".?"), ")"),
    full_name_under = paste0("<b>", gsub(regex_segment, paste0("<u>\\1</u>"), full_name, ignore.case = TRUE), "</b>")) |>
  filter(n_letters >= 7) |>
  ungroup() |>
  select(career_years, full_name_under, longest_alph, n_letters) |>
  rename(
    Career = career_years,
    `Player Name` = full_name_under,
    `Alph. Order` = longest_alph,
    `# Letters` = n_letters
  ) |>
  formattable(
    list(
      `Number of Letters` = bold_formatter
    ),
    align = c("r", "l", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Letters in Alphabetical Order")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL player names with the longest stretch of consecutive letters in alphabetical order"))
final_table

saveWidget(final_table, "tables/html/names_alph_forward.html")
webshot(url = "tables/html/names_alph_forward.html", 
        file = "tables/png/names_alph_forward.png", 
        selector = "body", zoom = 4,
        vwidth = 450)

# Final table formatting 2
final_table <- alphabetical_strings_rev |>
  rowwise() |>
  mutate(
    regex_segment = paste0("(", paste(longest_alph_list, collapse = ".?"), ")"),
    full_name_under = paste0("<b>", gsub(regex_segment, paste0("<u>\\1</u>"), full_name, ignore.case = TRUE), "</b>")) |>
  filter(n_letters >= 7) |>
  ungroup() |>
  select(career_years, full_name_under, longest_alph, n_letters) |>
  rename(
    Career = career_years,
    `Player Name` = full_name_under,
    `Rev. Alph. Order` = longest_alph,
    `# Letters` = n_letters
  ) |>
  formattable(
    list(
      `Number of Letters` = bold_formatter
    ),
    align = c("r", "l", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Letters in Reverse Alphabetical Order")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL player names with the longest stretch of consecutive letters in reverse alphabetical order"))
final_table

saveWidget(final_table, "tables/html/names_alph_rev.html")
webshot(url = "tables/html/names_alph_rev.html", 
        file = "tables/png/names_alph_rev.png", 
        selector = "body", zoom = 4,
        vwidth = 450)
