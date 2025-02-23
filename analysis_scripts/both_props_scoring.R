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
both_props_scores <- player_match_data |>
  filter(tries > 0 | penalty_tries > 0, position == "FR") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(match_id, team, opposition_team, player_id, full_name, position, tries) |>
  group_by(match_id, team, opposition_team, position) |>
  summarise(scorers = paste0(paste0("<b>", full_name, "</b>", ifelse(tries > 1, paste0(" (", tries, ")"), "")), collapse = " | "),
            n_scorers = n(),
            .groups = "drop") |>
  left_join(match_data |> select(match_id, competition_year, round, date), by = "match_id") |>
  select(competition_year, round, date, team, opposition_team, position, scorers, n_scorers) |>
  filter(n_scorers == 2) |>
  arrange(date)
  
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
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- both_props_scores |>
  filter(team == "Brisbane Broncos") |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "Rd\\1", round)
  ) |>
  select(year, round, team, opposition_team, scorers) |>
  rbind(
    tibble(year = 2024, round = "Rd23", team = "Brisbane Broncos", opposition_team = "North Queensland Cowboys", scorers = "<b>Corey Jensen</b> | <b>Xavier Willison</b>")
  ) |>
  rename(
    Year = year,
    Round = round,
    Team = team,
    Opposition = opposition_team,
    `Prop Tryscorers` = scorers
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Opposition = team_formatter
    ),
    align = c("c", "c", "l", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 7px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(3) td {background-color: #ffd6f0;}")) |>
  prependContent(h4(class = "title",
                    style = "font-family: Montserrat; font-weight: bold; margin-left: 5px;",
                    "Tryscoring Props")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Montserrat; margin-left: 5px;",
                    "Matches where both Brisbane Broncos starting props have scored"))
final_table

saveWidget(final_table, "tables/html/broncos_props_scoring.html")
webshot(url = "tables/html/broncos_props_scoring.html", 
        file = "tables/png/broncos_props_scoring.png", 
        selector = "body", zoom = 4,
        vwidth = 720)
