##### Description #####
# An R script to look at players scoring hat-tricks and four-try hauls before a double

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
tries_before_double <- player_match_data |>
  mutate(tries = tries + penalty_tries) |>
  left_join(match_data |> select(match_id, competition_year, round, date), by = "match_id") |>
  select(match_id, competition_year, round, date, team, opposition_team, player_id, tries) |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(match_no = row_number()) |>
  filter(all(3:4 %in% tries)) |>
  mutate(first_double = ifelse(2 %in% tries, min(which(tries == 2)), 500),
         first_hattrick = min(which(tries == 3)),
         first_four = min(which(tries == 4))) |>
  ungroup() |>
  rowwise() |>
  filter(any(match_no == c(first_double, first_hattrick, first_four))) |>
  filter(first_double > first_hattrick & first_hattrick > first_four) |> 
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "R\\1", round)
  )

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
final_table <- tries_before_double |>
  ungroup() |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("opposition_team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  select(-c(opposition_team, team_unique)) |> 
  rename(opp_abbr = team_abbr, opposition_colour = team_colour) |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  select(-c(team, team_unique)) |>
  mutate(
    team_abbr = paste0('<span style="display: inline; padding: 0 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', team_abbr, '</span>'),
    opp_abbr = paste0('<span style="display: inline; padding: 0 4px; border-radius: 4px; font-weight: bold; background-color:', opposition_colour, '; color:', ifelse(luminance(opposition_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', opp_abbr, '</span>'),
    match = paste0(year, " ", round, " ", team_abbr, " v ", opp_abbr)
  ) |>
  select(player_id, full_name, match, tries) |>
  pivot_wider(id_cols = c(player_id, full_name), names_from = tries, values_from = match, values_fill = "-") |>
  select(-player_id) |>
  rename(
    Player = full_name,
    `First 4-try Haul` = `4`,
    `First Hat-trick` = `3`,
    `First Double` = `2`
  ) |>
  formattable(
    list(
      Player = bold_formatter
    ),
    align = c("l", "l", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(6) td {background-color: #ffd6f0;}")) |>
  # prependContent(tags$style("table td:nth-child(4) { width: 320px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-family: Montserrat; padding: 3px 0 0 0;",
                    "<b>NSWRL/NRL players who scored:</b><br>a) their first 4-try haul before their first hat-trick, and<br>b) their first hat-trick before their first double" |> HTML()))
final_table

saveWidget(final_table, "tables/html/tries_before_double.html")
webshot(url = "tables/html/tries_before_double.html", 
        file = "tables/png/tries_before_double.png", 
        selector = "div", zoom = 4,
        vwidth = 680)
