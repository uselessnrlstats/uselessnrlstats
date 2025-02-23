##### Description #####
# An R script to look at rounds where a player led the try-scorer tally from 14th or lower

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(scales)
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
# Summarise each team and round 
ladder_data_2 <- ladder_data |>
  mutate(pts_record = paste0(points, " (", paste(wins, draws, losses, sep = "-"), ")")) |>
  select(competition_year, year, round, ladder_position, team, played, pts_record)

season_summary <- ladder_data |>
  group_by(competition_year, year) |>
  summarise(round = max(round), 
            teams = length(unique(team)),
            .groups = "drop") |>
  arrange(desc(year))

summarise_consecutive <- function(vec) {
  # Sort the vector to ensure it is in order
  vec <- sort(vec)
  # Initialize variables
  result <- c()
  start <- vec[1]
  end <- start
  
  if (length(vec) == 1) {
    result <- start
  } else {
    for (i in 2:length(vec)) {
      if (vec[i] == end + 1) {
        # Continue the sequence
        end <- vec[i]
      } else {
        # End the current sequence and start a new one
        if (start == end) {
          result <- c(result, as.character(start))
        } else {
          result <- c(result, paste(start, end, sep = "-"))
        }
        start <- vec[i]
        end <- start
      }
    }
    
    # Add the last sequence
    if (start == end) {
      result <- c(result, as.character(start))
    } else {
      result <- c(result, paste(start, end, sep = "-"))
    }
  }
  # Return the result as a comma-separated string
  return(paste(result, collapse = ","))
}

##### Analysis #####
data_setup <- player_match_data |>
  mutate(tries = tries + penalty_tries) |>
  select(player_id, match_id, team, position, tries) |>
  left_join(match_data |> select(match_id, competition_year, round, date), by = "match_id") |>
  filter(grepl("Round", round)) |>
  mutate(round = gsub("Round (\\d+)", "\\1", round) |> as.numeric()) |>
  arrange(date) |>
  group_by(competition_year, round, team, player_id) |>
  summarise(n_tries = sum(tries),
            .groups = "drop") |>
  group_by(competition_year, team, player_id) |>
  mutate(n_tries = cumsum(n_tries)) |>
  ungroup() |>
  left_join(ladder_data_2, by = c("competition_year", "round", "team")) |>
  group_by(competition_year, round) |>
  mutate(top_try_scorer = (n_tries == max(n_tries))) |>
  ungroup() |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  filter(top_try_scorer) |>
  select(competition_year, year, round, team, full_name, n_tries, ladder_position) |>
  arrange(year, round)

##### #####
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
                       text_col = ifelse(lum < 0.45, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- data_setup |>
  filter(round >= 18, ladder_position >= 14) |>
  mutate(ladder_position = ordinal(ladder_position)) |>
  group_by(competition_year, team, full_name) |>
  summarise(rounds = summarise_consecutive(unique(round)),
            n_tries = paste0(n_tries, collapse = ","),
            ladder_pos = paste0(unique(ladder_position), collapse = ","),
            .groups = "drop") |>
  select(competition_year, rounds, team, ladder_pos, full_name, n_tries) |>
  arrange(competition_year, rounds) |>
  rename(
    Year = competition_year,
    Rounds = rounds,
    Team = team,
    `Ladder Pos.` = ladder_pos,
    Player = full_name,
    `# Tries (by round)` = n_tries
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Player = bold_formatter,
      `# Tries (by round)` = bold_formatter
    ),
    align = c("l", "c", "l", "l", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 10px;",
                    "Mixed Fortunes")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 10px;",
                    "NRL/NSWRL players that <b>topped the try-scoring tally</b> from <b>Rd18 onwards</b><br>for a <b>team 14th or lower</b> on the ladder at the time" |> HTML()))
final_table

saveWidget(final_table, "tables/html/top_try_scorer_14th.html")
webshot(url = "tables/html/top_try_scorer_14th.html", 
        file = "tables/png/top_try_scorer_14th.png", 
        selector = "body", zoom = 4,
        vwidth = 750)
