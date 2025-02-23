##### Description #####
# An R script to look at players with the most games without a sin bin / send off

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
# Player Career year Span
player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year),
            .groups = "drop") |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

##### Analysis #####
player_match_data |>
  left_join(match_data |> select(match_id, date), by = "match_id") |>
  group_by(player_id) |>
  summarise(total_matches = n(),
            teams = list(unique(team)),
            years = list(sort(unique(year(date)))),
            .groups = "drop") |>
  left_join(player_career_years, by = "player_id") |>
  rowwise() |>
  mutate(career_span = list(first_year:last_year),
         n_years_span = length(career_span)) |>
  filter(length(career_span) == length(years)) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(player_id, full_name, teams, total_matches, n_years_span, career_years, years) |>
  arrange(desc(n_years_span), total_matches) |>
  group_by(n_years_span) |>
  filter(row_number() <= 1) |>
  ungroup() |>
  View()

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
final_table <- most_no_cards |>
  mutate(
    `Total SB/SOs` = total_cards
  ) |>
  select(career_years, full_name, total_matches, `Total SB/SOs`) |>
  filter(row_number() <= 20) |>
  rename(
    Career = career_years,
    Player = full_name,
    Matches = total_matches
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `Matches per SB/SO` = bold_formatter
    ),
    align = c("c", "r", "r", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "<b>Goody Two-Shoes:</b> NRL/NSWRL players with the most matches without a career sin bin (SB) or send off (SO)." |> HTML()))
final_table

saveWidget(final_table, "tables/html/matches_no_bin.html")
webshot(url = "tables/html/matches_no_bin.html", 
        file = "tables/png/matches_no_bin.png", 
        selector = "body", zoom = 4,
        vwidth = 450)
