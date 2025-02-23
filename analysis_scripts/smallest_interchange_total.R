##### Description #####
# An R script to look at the smallest jersey sum on the interchange

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
inter_totals <- player_match_data |>
  left_join(player_data |> select(player_id, full_name),
            by = "player_id") |>
  filter(position == "B") |>
  group_by(match_id, team) |>
  arrange(number) |>
  summarise(
    n_players = n(),
    number_total = sum(number),
    numbers = paste(number, collapse = "<br>"),
    players = paste(full_name, collapse = "<br>"),
    .groups = "drop"
  ) |>
  left_join(match_data |> select(match_id, competition, competition_year, round),
            by = "match_id") |>
  arrange(number_total, competition_year)
  filter(competition == "NRL",
         n_players == 4)
  

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
              `font-size` = "16px"
            ))

# Final table formatting
final_table <- inter_totals |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "\\1", round)
  ) |>
  select(year, round, team, numbers, players, number_total) |>
  filter(!is.na(number_total)) |>
  filter(row_number() %in% c(1:2, max(row_number()) - 1, max(row_number()))) |>
  rename(
    Year = year,
    Round = round,
    Team = team,
    Numbers = numbers,
    `Interchange Players` = players,
    `Number Total` = number_total
  ) |>
  formattable(
    list(
      Team = team_formatter,
      `Number Total` = bold_formatter
    ),
    align = c("c", "c", "r", "r", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(2n-1) td {border-top: 2px solid #7f7f7f;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Smallest/Largest Interchange Totals")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "The two smallest and largest sums of a team's 4 interchange jerseys in the NRL era (1998-)"))
final_table

saveWidget(final_table, "tables/html/interchange_totals.html")
webshot(url = "tables/html/interchange_totals.html", 
        file = "tables/png/interchange_totals.png", 
        selector = "body", zoom = 4,
        vwidth = 750)
