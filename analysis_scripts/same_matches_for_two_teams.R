##### Description #####
# An R script to look at players who have played an equal number of games for 
# two or more different teams

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
even_split <- player_match_data |>
  left_join(team_data |> select(team_name, team_unique), 
            by = c("team" = "team_name")) |>
  left_join(match_data |> select(match_id, date), by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id, team_unique) |>
  summarise(matches = n(),
            first_year = min(year),
            .groups = "drop") |>
  group_by(player_id) |>
  arrange(first_year) |>
  summarise(n_teams = n(),
            teams = list(c(team_unique)),
            team_matches = list(c(matches)),
            unique_count = length(unique(matches)),
            .groups = "drop") |>
  filter(unique_count < n_teams) |>
  rowwise() |>
  mutate(repeated_count = list(unique(team_matches[which(vapply(team_matches, \(x) sum(x == team_matches), integer(1)) > 1)]))) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  relocate(full_name, .before = n_teams) |>
  filter(unique_count == 1) |>
  mutate(repeated_count = unlist(repeated_count) |> as.numeric()) |>
  arrange(desc(repeated_count))


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
final_table <- even_split |>
  mutate(team1 = teams[1], team2 = teams[2], matches1 = team_matches[1], matches2 = team_matches[2]) |>
  left_join(player_career_years, by = "player_id") |>
  select(career_years, full_name, team1, matches1, team2, matches2) |>
  ungroup() |>
  filter(matches1 >= 15) |>
  rename(
    Career = career_years,
    Player = full_name,
    `Team 1` = team1,
    Matches = matches1,
    `Team 2` = team2,
    `Matches ` = matches2
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `Team 1` = team_formatter,
      `Team 2` = team_formatter
    ),
    align = c("c", "r", "r", "l", "r", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 10px;",
                    "Even Split")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 10px;",
                    "NRL/NSWRL players who split their career games evenly between the two clubs they played for (\u226530 games)"))
final_table

saveWidget(final_table, "tables/html/same_career_matches2.html")
webshot(url = "tables/html/same_career_matches2.html", 
        file = "tables/png/same_career_matches2.png", 
        selector = "body", zoom = 4,
        vwidth = 860)
