##### Description #####
# An R script to create arainbow ladder

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(TSP)
}

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
current_teams <- match_data |>
  filter(year(date) == 2023) |>
  select(home_team) |>
  rename(team_name = home_team) |>
  distinct() |>
  left_join(team_data, by = "team_name") |>
  left_join(team_logos, by = "team_unique") |>
  select(team_name, team_colour)

ordered_colours <- col2rgb(current_teams$team_colour) |>
  t() |>
  dist() |>
  as.TSP() |>
  solve_TSP(control = list(repetitions = 1e3)) |>
  as.numeric()

hue_order <- col2rgb(current_teams$team_colour) |>
  rgb2hsv() |>
  t() |>
  data.frame() |>
  mutate(position = row_number()) |>
  arrange(h) |>
  pull(position)

rainbow_order <- current_teams[rev(c(3,11,4,10,12,8,1,7,9,16,2,17,5,15,13,14,6)), ]

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

# Final table formatting
final_table <- rainbow_order |>
  select(-team_colour) |>
  mutate(position = row_number()) |>
  relocate(position, .before = team_name) |>
  rename(
    `#` = position,
    Team = team_name
  ) |>
  formattable(
    list(
      `#` = bold_formatter,
      Team = team_formatter
    ),
    align = c("c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica; vertical-align: middle"'
  )  |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 10px;",
                    "Rainbow NRL Ladder"))
final_table

saveWidget(final_table, "tables/html/rainbow_ladder.html")
webshot(url = "tables/html/rainbow_ladder.html", 
        file = "tables/png/rainbow_ladder.png", 
        selector = "body", zoom = 4,
        vwidth = 650)
