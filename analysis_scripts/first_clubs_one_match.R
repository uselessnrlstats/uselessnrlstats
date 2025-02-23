##### Description #####
# An R script to look at players who have played an equal number of games for 
# two or more different teams

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
player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year)) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

##### Analysis #####
one_match <- player_match_data |>
  left_join(team_data |> select(team_name, team_unique), 
            by = c("team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  mutate(team_label = paste0('<span style="display: block; padding: 2px 6px 2px 6px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', team_unique, '</span>')) |>
  left_join(match_data |> select(match_id, date), by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id, team_unique, team_label) |>
  summarise(matches = n(),
            first_date = min(date),
            .groups = "drop") |>
  group_by(player_id) |>
  arrange(first_date) |>
  summarise(n_teams = n(),
            teams = list(c(team_label)),
            team_matches = list(c(matches)),
            n_matches = sum(matches),
            .groups = "drop") |>
  rowwise() |>
  filter(n_teams > 2, team_matches[1] == 1, team_matches[2] == 1) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  relocate(full_name, .before = n_teams) |>
  arrange(desc(n_matches)) |>
  ungroup()


##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("div", 
            style = x ~ style(
              display = "block",
              vertical.align = "middle",
              padding = "2px 4px 3px 4px", 
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
  formatter("div", 
            style = x ~ style(
              font.weight = "bold",
              font.size = "14px"
            ))

# Final table formatting
final_table <- one_match |>
  rowwise() |>
  mutate(teams = paste0(teams, collapse = '<p style="line-height:5px; margin:0px;"><br></p>'),
         team_matches = paste0(team_matches, collapse = '<p style="line-height:5px; margin:0px;"><br></p>'),
         team_matches = paste0("<b style='font-size: 14px; font-family: Montserrat;'>", team_matches, "</b>")) |>
  left_join(player_career_years, by = "player_id") |>
  select(career_years, full_name, teams, team_matches) |>
  arrange(career_years) |>
  ungroup() |>
  filter(row_number() <= 15) |>
  rename(
    Career = career_years,
    Player = full_name,
    `Teams` = teams,
    `# Matches` = team_matches
  ) |>
  formattable(
    list(
      Player = bold_formatter
    ),
    align = c("c", "l", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(4) td {background-color: #ffd6f0;}")) |>
  # prependContent(tags$style("table td:nth-of-type(5) { width: 155px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "NSWRL/NRL Players to have represented 3 different clubs in their first 3 matches"))
final_table

saveWidget(final_table, "tables/html/first_three_matches.html")
webshot(url = "tables/html/first_three_matches.html", 
        file = "tables/png/first_three_matches.png", 
        selector = "div", zoom = 4,
        vwidth = 600)
 
