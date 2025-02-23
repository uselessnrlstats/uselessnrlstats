##### Description #####
# An R script to look at teams with players whose names end in -on

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

##### Analysis #####

on_teams <- player_match_data |>
  left_join(player_data |> select(player_id, full_name, last_name),
            by = "player_id") |>
  mutate(on_name = tolower(str_sub(last_name, start = -2))) |>
  filter(on_name == "on") |>
  group_by(match_id, team, opposition_team) |>
  summarise(
    n_on_names = n(),
    on_names = paste(sort(last_name), collapse = ", "),
    .groups = "drop"
  ) |>
  left_join(match_data |> select(match_id, competition_year, round, date),
            by = "match_id") |>
  arrange(desc(n_on_names), date)

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
                       text_col = ifelse(lum < 0.35, "#F2F2F2", "#1A1A1A")) |>
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
final_table <- on_teams |>
  filter(n_on_names == 7) |>
  mutate(year = year(date),
         round = gsub("Round\\s", "", round)) |>
  select(year, round, team, n_on_names, on_names) |>
  rbind(data.frame(year = "",
                   round = "",
                   team = paste0("+",
                                 c(on_teams |> filter(n_on_names == 6) |> nrow(),
                                   on_teams |> filter(n_on_names == 5) |> nrow())),
                   n_on_names = c(6, 5),
                   on_names = "")) |>
  rename(
    Year = year,
    Round = round,
    Team = team,
    `# Players` = n_on_names,
    `'-on' Surnames` = on_names
  ) |>
  formattable(
    list(
      Team = team_formatter,
      `# Players` = bold_formatter
    ),
    align = c("r", "r", "r", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica; vertical-align: middle"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "'-on' Sides")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL teams with the most surnames ending in '-on' in one match"))
final_table

saveWidget(final_table, "tables/html/on_names_matches.html")
webshot(url = "tables/html/on_names_matches.html", 
        file = "tables/png/on_names_matches.png", 
        selector = "body", zoom = 4,
        vwidth = 850)
