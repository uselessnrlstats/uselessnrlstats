##### Description #####
# An R script to look at pi related stats

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
three_hundredths <- player_match_data |>
  left_join(match_data |> select(match_id, competition_year, round, date),
            by = "match_id") |>
  group_by(player_id) |>
  arrange(date) |>
  mutate(game_number = row_number()) |>
  filter(game_number %in% c(1, 300)) |>
  group_by(match_id, competition_year, round) |>
  filter(all(c(1, 300) %in% game_number)) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  ungroup() |>
  select(match_id, competition_year, round, team, full_name, game_number)

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
final_table <- three_hundredths |> 
  filter(game_number == 1) |>
  left_join(three_hundredths |> filter(game_number == 300) |> select(match_id, team, full_name) |> rename(team300 = team, full_name300 = full_name),
            by = "match_id") |>
  select(-c(match_id, game_number)) |>
  # rbind(tibble(competition_year = "NRL 2024", round = "Round 24", team = "St George Illawarra Dragons", full_name = "Lyhkan King-Togia", team300 = "Gold Coast Titans", full_name300 = "Kieran Foran")) |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("team300" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  select(-c(team300, team_unique)) |> 
  rename(team_abbr300 = team_abbr, team_colour300 = team_colour) |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  select(-c(team, team_unique)) |> 
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "Rd\\1", round),
    team_abbr = paste0('<span style="display: block; padding: 0 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', team_abbr, '</span>'),
    team_abbr300 = paste0('<span style="display: block; padding: 0 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour300, '; color:', ifelse(luminance(team_colour300) < 0.4, "#F2F2F2", "#1A1A1A"), '">', team_abbr300, '</span>'),
  ) |>
  select(year, round, team_abbr, full_name, team_abbr300, full_name300) |>
  rename(
    Year = year,
    Rd = round,
    Team = team_abbr,
    Debutant = full_name,
    `Team ` = team_abbr300,
    `300-Gamer` = full_name300
  ) |>
  formattable(
    list(
      Debutant = bold_formatter,
      `300-Gamer` = bold_formatter
    ),
    align = c("c", "l", "c", "l", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 5px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(20) td {background-color: #ffd6f0;}")) |>
  #prependContent(tags$style("table td:nth-child(4), td:nth-child(5) { width: 180px;}")) |>
  prependContent(h5(class = "title",
                    style = "font-family: Montserrat; font-weight: bold; margin-left: 5px;",
                    "NRL Players to debut in another player's 300th match"))
final_table

saveWidget(final_table, "tables/html/debut_in_300th.html")
webshot(url = "tables/html/debut_in_300th.html", 
        file = "tables/png/debut_in_300th.png", 
        selector = "body", zoom = 4,
        vwidth = 535)
