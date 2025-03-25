##### Description #####
# An R script to look at which year had the mos forwards as captains

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot)
  library(scales)
  library(slider)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
player_match_data |>
  select(player_id, match_id, team, opposition_team) |>
  left_join(match_data |> select(match_id, date), by = "match_id") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  arrange(date) |>
  group_by(player_id, full_name) |>
  mutate(match_number = row_number(),
         experience = match_number - 1) |>
  ungroup() |>
  group_by(match_id, team, opposition_team, date) |>
  summarise(n_players = n(),
            experience_sorted = list(sort(experience)),
            min_experience = min(experience),
            min_player = paste0(full_name[which(experience == min_experience)], collapse = ", "),
            .groups = "drop") |>
  arrange(desc(min_experience)) |>
  filter(n_players >= 17) |>
  mutate(year = year(date)) |>
  View()
  


##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "2px 4px 2px 4px", 
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
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold",
              display = "block"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat"
            ))

num_bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat",
              font.weight = "bold"
            ))

# Final table formatting
final_table <- player_team_stints |>
  ungroup() |>
  select(time_span, full_name, teams, matches_to_4) |>
  filter(matches_to_4 <= 7) |>
  rename(
    Years = time_span,
    Player = full_name,
    `4 Teams` = teams,
    `# Matches` = matches_to_4
  ) |>
  formattable(
    list(
      `Player` = bold_formatter,
      `# Matches` = num_bold_formatter
    ),
    align = c("c", "l", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(2) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "NRL/NSWRL players to represent 4 different teams across the shortest stretch of matches"))
final_table

saveWidget(final_table, "tables/html/quickest_4_teams.html")
webshot(url = "tables/html/quickest_4_teams.html", 
        file = "tables/png/quickest_4_teams.png", 
        selector = "div", zoom = 4,
        vwidth = 600)
