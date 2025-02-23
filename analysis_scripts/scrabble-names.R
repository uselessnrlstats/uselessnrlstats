##### Description #####
# An R script to look at players whose names give the highest scrabble score

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
ref_data <- read_csv("cleaned_data/nrl/ref_data.csv")
letter_scores <- c(1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10)

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
scrabble <- player_data |>
  select(player_id, full_name, first_name, last_name) |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  mutate(first_name_letters = gsub("[^a-zA-Z]", "", first_name) |> 
           str_to_upper() |>
           str_split(pattern = ""),
         last_name_letters = gsub("[^a-zA-Z]", "", last_name) |> 
           str_to_upper() |>
           str_split(pattern = ""),
         n_letters = gsub("[^a-zA-Z]", "", full_name) |>
           nchar()) |>
  rowwise() |>
  mutate(
    first_name_scrabble = list(letter_scores[match(first_name_letters, LETTERS)]),
    last_name_scrabble = list(letter_scores[match(last_name_letters, LETTERS)])
  ) |>
  mutate(
    first_name_score = sum(first_name_scrabble),
    last_name_score = sum(last_name_scrabble),
    full_name_score = first_name_score + last_name_score,
    score_per_letter = full_name_score / n_letters
  ) |>
  ungroup() |>
  select(player_id, first_name, first_name_score, last_name, last_name_score, full_name, full_name_score, n_letters, score_per_letter) |>
  arrange(desc(full_name_score))
scrabble

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
final_table1 <- scrabble |>
  arrange(desc(full_name_score)) |>
  filter(row_number() %in% 1:19) |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id") |>
  mutate(score_per_letter = digits(score_per_letter, 2),
         ` ` = row_number()) |>
  select(` `, full_name, career_years, n_letters, full_name_score, score_per_letter) |>
  rename(
    Player = full_name,
    Career = career_years,
    Letters = n_letters,
    Score = full_name_score,
    `Score / Letter` = score_per_letter
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      Score = bold_formatter
    ),
    align = c("c", "r", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Highest NRL Name Scrabble Scores"))
final_table1

saveWidget(final_table1, "tables/html/scrabble1.html")
webshot(url = "tables/html/scrabble1.html", 
        file = "tables/png/scrabble1.png", 
        selector = "body", zoom = 4,
        vwidth = 500)

# Final Table 2 Formatting
final_table2 <- scrabble |>
  arrange(desc(score_per_letter)) |>
  filter(row_number() %in% 1:20) |>
  left_join(player_career_years |> select(player_id, career_years),
            by = "player_id") |>
  mutate(score_per_letter = digits(score_per_letter, 2),
         ` ` = row_number()) |>
  select(` `, full_name, career_years, n_letters, full_name_score, score_per_letter) |>
  rename(
    Player = full_name,
    Career = career_years,
    Letters = n_letters,
    Score = full_name_score,
    `Score / Letter` = score_per_letter
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `Score / Letter` = bold_formatter
    ),
    align = c("c", "r", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Highest NRL Name Scrabble Scores per Letter"))
final_table2

saveWidget(final_table2, "tables/html/scrabble2.html")
webshot(url = "tables/html/scrabble2.html", 
        file = "tables/png/scrabble2.png", 
        selector = "body", zoom = 4,
        vwidth = 500)
