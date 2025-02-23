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
  library(showtext)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

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
            teams = paste(team_unique, collapse = ", "),
            team_matches = paste(matches, collapse = ", "),
            sd = sd(matches),
            .groups = "drop") |>
  filter(n_teams == 3,
         sd == 0) |>
  select(player_id, teams, team_matches) |>
  mutate(team1 = gsub("^(.+), (.+)$", "\\1", teams),
         matches1 = gsub("^(.+), (.+)$", "\\1", team_matches) |> as.numeric(),
         team2 = gsub("^(.+), (.+)$", "\\2", teams),
         matches2 = gsub("^(.+), (.+)$", "\\2", team_matches) |> as.numeric()) |>
  select(-c(teams, team_matches)) |>
  arrange(desc(matches1)) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(-player_id) |>
  relocate(full_name, .before = team1)


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
  filter(row_number() <= 10) |>
  rename(
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
    align = c("r", "r", "l", "r", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Even Split")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL players who split their career games evenly between the two clubs they played for (min 20 career games)"))
final_table

saveWidget(final_table, "tables/html/same_career_matches.html")
webshot(url = "tables/html/same_career_matches.html", 
        file = "tables/png/same_career_matches.png", 
        selector = "body", zoom = 4,
        vwidth = 730)
