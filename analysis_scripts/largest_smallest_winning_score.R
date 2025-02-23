##### Description #####
# An R script to look at unique names scoring on debut

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
match_data |>
  rowwise() |>
  mutate(winning_score = max(home_team_score, away_team_score)) |>
  group_by(competition_year, round) |> 
  summarise(total_winning_points = sum(winning_score),
            min_winning_score = min(winning_score),
            all_winning_scores = paste0(sort(winning_score), collapse = ","),
            n_rounds = n(),
            .groups = "drop") |>
  arrange(desc(total_winning_points)) |>
  filter(n_rounds == 8)
  
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
final_table <- match_margins |>
  left_join(team_data |> select(team_name, team_unique), by = c("home_team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  mutate(home_team = paste0('<span style="display: block; padding: 0 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', home_team, '</span>')) |>
  select(-c(team_unique, team_colour)) |>
  left_join(team_data |> select(team_name, team_unique), by = c("away_team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  mutate(away_team = paste0('<span style="display: block; padding: 0 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', away_team, '</span>')) |>
  select(-c(team_unique, team_colour)) |>
  group_by(year, round, date) |>
  summarise(home_team = paste0(home_team, collapse = '<p style="line-height:10px; margin:0px;"><br></p>'),
            match_score = paste0(match_score, collapse = '<p style="line-height:10px; margin:0px;"><br></p>'),
            away_team = paste0(away_team, collapse = '<p style="line-height:10px; margin:0px;"><br></p>'),
            result_diff = max(margin) - min(margin),
            margin = paste0(margin, collapse = '<p style="line-height:10px; margin:0px;"><br></p>'),
            .groups = "drop") |>
  relocate(result_diff, .after = margin) |>
  arrange(desc(result_diff), date) |>
  mutate(date = format(date, "%d/%m/%Y")) |>
  rename(
    Year = year,
    Round = round,
    Date = date,
    `Home Team` = home_team,
    vs = match_score,
    `Away Team` = away_team,
    Margin = margin,
    Difference = result_diff
  ) |>
  formattable(
    list(
      Date = bold_formatter,
      Difference = bold_formatter
    ),
    align = c("c", "c", "c", "r", "c", "l", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 14px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 6px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table td:nth-of-type(4), td:nth-of-type(6) { width: 250px;}")) |>
  prependContent(h4(class = "subtitle",
                    style = "font-family: Montserrat; font-weight: bold; margin-left: 5px;",
                    "Thrashings and Thrillers")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Montserrat; margin-left: 5px;",
                    "Biggest difference in final margins on a day with 2 NRL matches" |> HTML()))
final_table

saveWidget(final_table, "tables/html/thrashings_and_thrillers.html")
webshot(url = "tables/html/thrashings_and_thrillers.html", 
        file = "tables/png/thrashings_and_thrillers.png", 
        selector = "body", zoom = 4,
        vwidth = 950)
