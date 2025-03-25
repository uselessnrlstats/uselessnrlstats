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
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
captains <- player_match_data |>
  select(player_id, match_id, position, captain, team) |>
  left_join(match_data |> select(match_id, round, date), by = "match_id") |>
  mutate(year = year(date)) |>
  filter(captain) |>
  #count(year, team, position, player_id) |>
  group_by(year, round) |>
  summarise(
    n_teams = length(unique(team)),
    n_captains = length(unique(player_id)),
    n_forward_captains = sum(position %in% c("HK", "FR", "2R", "L", "B"))
  )

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
final_table <- last_by_decade |>
  arrange(birth_decade) |>
  select(birth_decade, birthday, full_name, age, team, date) |>
  filter(birth_decade <= 1980) |>
  mutate(full_name = ifelse(birth_decade >= 1990, paste0(full_name, "*"), full_name),
         birthday = format(birthday, "%d/%m/%Y"),
         date = format(date, "%d/%m/%Y"),
         birth_decade = paste0(birth_decade, "s")) |>
  rename(
    Decade = birth_decade,
    Birthday = birthday,
    Player = full_name,
    `Age at Debut` = age,
    Team = team,
    `Last Match` = date
  ) |>
  formattable(
    list(
      Decade = num_bold_formatter,
      Team = team_formatter,
      `Player` = bold_formatter,
      `Age at Debut` = num_formatter
    ),
    align = c("c", "c", "l", "l", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(12) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "Last NRL/NSWRL player born in each decade to still be playing"))
final_table

saveWidget(final_table, "tables/html/last_player_from_decade.html")
webshot(url = "tables/html/last_player_from_decade.html", 
        file = "tables/png/last_player_from_decade.png", 
        selector = "div", zoom = 4,
        vwidth = 800)
