##### Description #####
# An R script to look at rounds where players in jersey x scored for the team in position x on the ladder

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
ladder_data <- read_csv("cleaned_data/nrl/ladder_round_data.csv") |>
  group_by(competition_year, year, round) |>
  mutate(ladder_position = row_number()) |>
  ungroup()

##### Helper Stats #####

##### Analysis #####
data_setup <- player_match_data |>
  select(player_id, match_id, team, position, number, tries) |>
  left_join(match_data |> select(match_id, competition_year, round), by= "match_id") |>
  filter(grepl("Round", round)) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "\\1", round) |> as.numeric()) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  group_by(competition_year, year, round, team, match_id) |>
  arrange(number) |>
  summarise(n_tries = sum(tries),
            try_scorers = list(full_name[which(tries > 0)]),
            try_scorer_numbers = list(number[which(tries > 0)]),
            .groups = "drop") |>
  arrange(year) |>
  left_join(ladder_data, by = c("competition_year", "year", "round", "team")) |>
  relocate(ladder_position, .before = points) |>
  rowwise() |>
  mutate(try_position = (ladder_position %in% try_scorer_numbers))

data_setup |>
  group_by(competition_year, year, round) |>
  summarise(n_teams = max(ladder_position),
            successes = sum(try_position),
            .groups = "drop") |>
  mutate(diff = n_teams - successes) |>
  View()

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
