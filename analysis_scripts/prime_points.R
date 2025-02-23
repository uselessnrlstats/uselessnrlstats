##### Description #####
# An R script to look at players who scored a prime number of points in a season

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(primes)
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

player_career_summaries <- player_match_data |>
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
            total_points = sum(points)) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  relocate(full_name, .after = player_id)

##### Analysis #####
player_career_summaries |>
  select(player_id, full_name, total_matches, total_tries, total_goals, total_field_goals) |>
  mutate(match_prime = is_prime(total_matches),
         try_prime = is_prime(total_tries),
         goals_prime = is_prime(total_goals)) |>
  filter(match_prime, try_prime, goals_prime) |>
  arrange(desc(total_matches)) |>
  select(-c(match_prime, try_prime, goals_prime))
  
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
final_table <- average_jersey |>
  filter(row_number() <= 10) |>
  left_join(player_number_counts, by = "player_id") |>
  select(full_name, career_years, total_matches, number_summary, number_total, av_number, av_num_1pi) |>
  mutate(
    av_num_1pi = ifelse(av_number > pi, av_num_1pi, -av_num_1pi),
    av_number = digits(av_number, 6),
    av_num_1pi = digits(av_num_1pi, 6)) |>
  rename(
    `Player` = full_name,
    Career = career_years,
    `# Matches` = total_matches,
    `Jersey Number (Matches)` = number_summary,
    `Jersey Total` = number_total,
    `Jersey Average` = av_number,
    `Diff from Pi` = av_num_1pi
  ) |>
  formattable(
    list(
      `Player Name` = bold_formatter,
      `Diff from Pi` = bold_formatter
    ),
    align = c("r", "c", "r", "l", "r", "r", "r"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "World \u03C0 Day")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL players with the closest average career jersey number to \u03C0 = 3.14159... "))
final_table

saveWidget(final_table, "tables/html/average_jersey_number.html")
webshot(url = "tables/html/average_jersey_number.html", 
        file = "tables/png/average_jersey_number.png", 
        selector = "body", zoom = 4,
        vwidth = 850)
