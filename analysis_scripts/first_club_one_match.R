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
  library(webshot2)
  library(showtext)
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
  left_join(match_data |> select(match_id, date), by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id, team_unique) |>
  summarise(matches = n(),
            first_date = min(date),
            .groups = "drop") |>
  group_by(player_id) |>
  arrange(first_date) |>
  summarise(n_teams = n(),
            teams = list(c(team_unique)),
            team_matches = list(c(matches)),
            n_matches = sum(matches),
            .groups = "drop") |>
  rowwise() |>
  filter(n_teams > 1, team_matches[1] == 1) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  relocate(full_name, .before = n_teams) |>
  arrange(desc(n_matches)) |>
  ungroup()


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
  formatter("div", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- one_match |>
  rowwise() |>
  mutate(first_team = teams[1], 
         other_teams = paste0(teams[2:length(teams)], collapse = "<br>"),
         other_teams = paste0("<b>", other_teams, "</b>"),
         first_counts = paste0("(", team_matches[1], ")"),
         other_counts = paste0(paste0("(", team_matches[2:length(teams)], ")"), collapse = "<br>")) |>
  left_join(player_career_years, by = "player_id") |>
  select(career_years, full_name, first_team, first_counts, other_teams, other_counts, n_matches) |>
  ungroup() |>
  filter(row_number() <= 15) |>
  rename(
    Career = career_years,
    Player = full_name,
    `1st Team` = first_team,
    `(n)` = first_counts,
    `Other Teams` = other_teams,
    `(n) ` = other_counts,
    `# Matches` = n_matches
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `1st Team` = team_formatter,
      `# Matches` = bold_formatter
    ),
    align = c("c", "c", "r", "l", "r", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table td:nth-of-type(7n-6) { width: 70px;}")) |>
  prependContent(tags$style("table td:nth-of-type(7n-5) { width: 130px;}")) |>
  prependContent(tags$style("table td:nth-of-type(7n-4) { width: 200px;}")) |>
  prependContent(tags$style("table td:nth-of-type(7n-3) { width: 20px;}")) |>
  prependContent(tags$style("table td:nth-of-type(7n-2) { width: 200px;}")) |>
  prependContent(tags$style("table td:nth-of-type(7n-1) { width: 20px;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 10px;",
                    "The ones that got away")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 10px;",
                    "Longest NRL/NSWRL careers by players who only played 1 match for their first club"))
final_table

saveWidget(final_table, "tables/html/first_club_one_match.html")
webshot(url = "tables/html/first_club_one_match.html", 
        file = "tables/png/first_club_one_match.png", 
        selector = "body", zoom = 4,
        vwidth = 850)
