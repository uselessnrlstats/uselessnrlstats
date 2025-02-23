##### Description #####
# An R script to look at players with the most games before their first sin bin / send off

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
# Player Career Summaries
player_career_summaries <- player_match_data |>
  left_join(match_data |> select(match_id, date), by = "match_id") |>
  # filter(year(date) >= 1998) |>
  group_by(player_id) |>
  summarise(total_matches = n(),
            teams = list(unique(team)),
            numbers = list(unique(number, na.rm = TRUE)),
            positions = list(unique(position, na.rm = TRUE)),
            captaincy = sum(captain),
            total_tries = sum(tries + penalty_tries),
            total_goals = sum(goals),
            total_goal_attempts = sum(goal_attempts, na.rm = TRUE),
            total_field_goals = sum(field_goals),
            total_field_goals2 = sum(field_goals2),
            total_sin_bins = sum(sin_bins5 + sin_bins),
            total_send_offs = sum(send_offs),
            total_points = sum(points),
            .groups = "drop")

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
players_with_so <- player_career_summaries |>
  left_join(player_career_years, by = "player_id") |>
  mutate(total_cards = total_sin_bins + total_send_offs) |>
  filter(total_cards > 0) |>
  pull(player_id)

first_so_match <- player_match_data |>
  filter(player_id %in% players_with_so) |>
  left_join(match_data |> select(match_id, date), by = "match_id") |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(match_no = row_number()) |>
  filter(sin_bins5 > 0 | sin_bins > 0 | send_offs > 0) |>
  summarise(first_so = min(match_no), .groups = "drop")

games_before_so <- player_career_summaries |>
  left_join(player_career_years, by = "player_id") |>
  mutate(total_cards = total_sin_bins + total_send_offs) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(player_id, full_name, career_years, total_matches, total_sin_bins, total_send_offs, total_cards) |>
  filter(total_cards > 0) |>
  left_join(first_so_match, by = "player_id") |>
  arrange(desc(first_so))

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
final_table <- games_before_so |>
  mutate(
    `Sin Bins/Send Offs` = paste(total_sin_bins, "/", total_send_offs)
  ) |>
  select(career_years, full_name, total_matches, `Sin Bins/Send Offs`, first_so) |>
  filter(row_number() <= 20) |>
  rename(
    Career = career_years,
    Player = full_name,
    Matches = total_matches,
    `Match of 1st SB/SO` = first_so
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `Match of 1st SB/SO` = bold_formatter
    ),
    align = c("c", "r", "r", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "<b>Close but no cigar:</b> NRL/NSWRL players with the most matches before their first career sin bin (SB) or send off (SO)." |> HTML()))
final_table

saveWidget(final_table, "tables/html/matches_first_bin.html")
webshot(url = "tables/html/matches_first_bin.html", 
        file = "tables/png/matches_first_bin.png", 
        selector = "body", zoom = 4,
        vwidth = 600)
