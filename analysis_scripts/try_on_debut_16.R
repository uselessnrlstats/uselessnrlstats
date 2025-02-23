##### Description #####
# An R script to look at players who scored on debut in the #16 jersey

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
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
try_on_debut16 <- player_match_data |>
  left_join(match_data |> select(match_id, competition_year, round, date),
            by = "match_id") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  group_by(player_id) |>
  arrange(date) |>
  mutate(game_number = row_number()) |>
  ungroup() |>
  filter(game_number == 1 & (tries > 0 | penalty_tries > 0)) |>
  filter(year(date) >= 1998) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round)) |>
  select(year, round, date, team, opposition_team, full_name, number, position, tries)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block",
              padding = "6px 4px 6px 4px",
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
final_table <- three_bench_debuts |> 
  rbind(tibble(year = "2024", round = "Rd26", team = "Melbourne Storm", opposition_team = "North Queensland Cowboys", debutants = "14. Keagan Russell-Smith<br>16. Ativalu Lisati<br>17. Tristan Powell")) |>
  mutate(
    year_rd = paste0(year, " ", round),
    debutants = paste0("<b>", debutants, "</b>")) |>
  # left_join(team_data |> select(team_name, team_unique, team_mascots), by = c("opposition_team" = "team_name")) |>
  # left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  # select(-c(opposition_team, team_unique)) |> 
  # rename(opposition_mascots = team_mascots, opposition_colour = team_colour) |>
  # left_join(team_data |> select(team_name, team_unique, team_mascots), by = c("team" = "team_name")) |>
  # left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  # select(-c(team, team_unique)) |> 
  # mutate(
  #   team_mascots = paste0('<span style="display: block; padding: 0 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', team_mascots, '</span>'),
  #   opposition_mascots = paste0('<span style="display: block; padding: 0 4px; border-radius: 4px; font-weight: bold; background-color:', opposition_colour, '; color:', ifelse(luminance(opposition_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', opposition_mascots, '</span>'),
  # ) |>
  # select(year_rd, team_mascots, opposition_mascots, debutants) |>
  # rename(
  #   Match = year_rd,
  #   Team = team_mascots,
  #   Opposition = opposition_mascots,
  #   `Bench Debutants` = debutants
  # ) |>
  select(year_rd, team, opposition_team, debutants) |>
  rename(
    Match = year_rd,
    Team = team,
    Opposition = opposition_team,
    `Bench Debutants` = debutants
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Opposition = team_formatter
    ),
    align = c("l", "r", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(7) td {background-color: #ffd6f0;}")) |>
  #prependContent(tags$style("table td:nth-child(4), td:nth-child(5) { width: 180px;}")) |>
  prependContent(h5(class = "title",
                    style = "font-family: Montserrat; font-weight: bold; margin-left: 5px;",
                    "NRL-era: 3+ debutants from the bench"))
final_table

saveWidget(final_table, "tables/html/bench_debuts2.html")
webshot(url = "tables/html/bench_debuts2.html", 
        file = "tables/png/bench_debuts2.png", 
        selector = "div", zoom = 4,
        vwidth = 680)
