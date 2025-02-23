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
  library(webshot)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv") |>
  mutate(position = case_when(
    position == "FB" ~ "Fullback",
    position == "W" ~ "Wing",
    position == "C" ~ "Centre",
    position == "FE" ~ "5/8th",
    position == "HB" ~ "Halfback",
    position == "FR" ~ "Prop",
    position == "HK" ~ "Hooker",
    position == "2R" ~ "2nd Row",
    position == "L" ~ "Lock",
    position == "B" ~ "Bench"
  ),
  position = factor(position, levels = c("Fullback", "Wing", "Centre", "5/8th", "Halfback", "Hooker", "Prop", "2nd Row", "Lock", "Bench"))
  )
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
most_by_position <- player_match_data |>
  select(match_id, team, player_id, position) |>
  left_join(match_data |> select(match_id, competition_year, round), by = "match_id") |>
  filter(competition_year == "NRL 2024",  grepl("Round", round)) |>
  group_by(team, player_id, position) |>
  summarise(n_matches = n(),
            .groups = "drop") |>
  left_join(player_data |> select(player_id, full_name, first_name, last_name), by = "player_id") |>
  mutate(player_label = paste(substr(first_name, 1, 1), last_name)) |>
  group_by(team, position) |>
  arrange(desc(n_matches)) |>
  summarise(n_players = n(),
            players = paste0(paste0("<b>", player_label, "</b> (", n_matches, ")"), collapse = ", "),
            max_matches = max(n_matches),
            .groups = "drop") |>
  group_by(position) |>
  filter(n_players == max(n_players)) |>
  filter(max_matches == min(max_matches)) |>
  ungroup() |>
  arrange(position)
  

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "4px 4px 4px 4px",
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
              font.weight = "bold"
            ))

# Final table formatting
final_table <- most_by_position |>
  filter(position != "Bench") |>
  select(position, team, n_players, players) |>
  rename(
    Position = position,
    Team = team,
    `#` = n_players,
    Players = players
  ) |>
  formattable(
    list(
      Position = bold_formatter,
      Team = team_formatter
    ),
    align = c("c", "l", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table td:nth-of-type(1) { width: 60px;}")) |>
  prependContent(tags$style("table td:nth-of-type(2) { width: 190px;}")) |>
  prependContent(tags$style("table td:nth-of-type(3) { width: 60px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 18px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "NRL 2024: Most players played in each position by team (H/A matches)"))
final_table

saveWidget(final_table, "tables/html/most_by_position24.html")
webshot(url = "tables/html/most_by_position24.html", 
        file = "tables/png/most_by_position24.png", 
        selector = "div", zoom = 4,
        vwidth = 910)
