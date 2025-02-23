##### Description #####
# An R script to look at players with the most sin-bins in a season

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
player_year_summaries <- player_match_data |>
  left_join(match_data |> select(match_id, competition_year, date), by = "match_id") |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
#  filter(year >= 1998) |>
  group_by(player_id, year) |>
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
mult_card_years <- player_year_summaries |>
  mutate(total_cards = total_sin_bins + total_send_offs) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(player_id, full_name, year, total_matches, total_sin_bins, total_send_offs, total_cards) |>
  arrange(year) |>
  filter(total_cards >= 2) |>
 # mutate(year = gsub("\\d{2}(\\d{2})", "'\\1", year)) |>
  rowwise() |>
  mutate(year_cards = paste0(year, " (", total_cards, ")")) |>
  ungroup() |>
  group_by(player_id, full_name) |>
  summarise(mult_card_years = n(),
            year_summaries = paste0(unique(year_cards), collapse = ", "),
            .groups = "drop") |>
  arrange(desc(mult_card_years)) |>
  filter(mult_card_years >= 4) |>
  left_join(player_career_years |> select(player_id, career_years), by = "player_id") |>
  relocate(career_years, .before = full_name)
         

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
final_table <- mult_card_years |>
  mutate(
    sb_summary = paste0("<b>", mult_card_years, ":</b> ", year_summaries)
  ) |>
  select(career_years, full_name, sb_summary) |>
  rename(
    Career = career_years,
    Player = full_name,
    "# Seasons with \u22652 Sin Bins/Send Offs" = sb_summary
  ) |>
  formattable(
    list(
      Player = bold_formatter
    ),
    align = c("c", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
 # prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 5px;",
                    "<b>Repeat Offenders:</b> All-time NSWRL/NRL (1908-) players to have 4 or more seasons with \u22652 Sin Bins/Send Offs" |> HTML()))
final_table

saveWidget(final_table, "tables/html/mult_card_years_all.html")
webshot(url = "tables/html/mult_card_years_all.html", 
        file = "tables/png/mult_card_years_all.png", 
        selector = "body", zoom = 4,
        vwidth = 700)
 
