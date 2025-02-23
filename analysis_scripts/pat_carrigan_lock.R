##### Description #####
# An R script to look at players who played in all 4 spine positions in a season

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

##### Analysis #####

# spine positions
most_lock_games <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  left_join(team_data |> select(team_name, team_abbr), by = c("team" = "team_name")) |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(match_no = row_number()) |>
  filter(match_no <= 100) |>
  filter(max(match_no) >= 99) |>
  summarise(n_matches = n(),
            n_matches_L = sum(position == "L"),
            teams = paste0(unique(team_abbr), collapse = " "),
            .groups = "drop") |>
  left_join(player_data |> select(player_id, full_name), 
            by = "player_id") |>
  left_join(player_career_years, by = "player_id")


##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `vertical.align` = "center",
              `background-color` = data.frame(team_name = x) |>
                left_join(team_data, by = "team_name") |>
                left_join(team_logos, by = "team_unique") |>
                pull(team_colour),
              color = data.frame(team_name = x) |>
                left_join(team_data, by = "team_name") |>
                left_join(team_logos, by = "team_unique") |>
                mutate(lum = luminance(team_colour),
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

number_formatter <-
  formatter("span",
            style = x ~ style(
              font.size = "12px"
            ))

# Final table formatting
final_table <- most_lock_games |>
  filter(first_year >= 1991, n_matches_L >= 50, n_matches == 100 | player_id == 28411) |>
  mutate(n_matches_L = ifelse(player_id == 28411, 80, n_matches_L)) |>
  arrange(desc(n_matches_L), first_year, last_year) |>
  mutate(n_matches_L = ifelse(player_id == 28411, "*80", n_matches_L)) |>
  select(career_years, teams, full_name, n_matches_L) |>
  rename(
    Career = career_years,
    Teams = teams,
    Player = full_name,
    `# Matches at<br>Lock<br>in first 100` = n_matches_L
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `# Matches at<br>Lock<br>in first 100` = bold_formatter
    ),
    align = c("c", "c", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 14px; font-family: Helvetica; vertical-align: middle"'
  ) |>
  as.htmlwidget() |>
  prependContent(h3(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 5px;",
                    "LOCKing down the 13")) |>
  prependContent(h4(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 5px;",
                    "NSWRL/NRL Players since 1991 to have started at<br>LOCK the most times in their first 100 matches." |> HTML())) |>
  prependContent(tags$style("td { padding: 6px  !important;}")) |>
  prependContent(tags$style("table td:nth-of-type(4n-2) { width: 150px;}")) |>
  prependContent(tags$style("table tr:nth-of-type(19n-16) td {background-color: #FFB90555;}"))

final_table

saveWidget(final_table, "tables/html/pat_carrigan_lock.html")
webshot(url = "tables/html/pat_carrigan_lock.html", 
        file = "tables/png/pat_carrigan_lock.png", 
        selector = "body", zoom = 4, delay = 1,
        vwidth = 600)
