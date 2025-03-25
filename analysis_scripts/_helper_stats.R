##### Description #####
# An R script to produce helpful dataframes

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
ref_data <- read_csv("cleaned_data/nrl/ref_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
ladder_data <- read_csv("cleaned_data/nrl/ladder_round_data.csv")
venue_data <- read_csv("cleaned_data/nrl/venue_data.csv")

##### Useful filtering Lines #####
year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()
round = gsub("Round (\\d+)", "\\1", round) |> as.numeric()
round = case_when(
  round %in% c("Prelim Final", "Prelim Playoff", "Major Prelim Semi", "Minor Prelim Semi", "Minor Prelim", "Major Prelim") ~ "PF",
  round %in% c("Semi Final", "Minor Semi", "Major Semi", "Minor Semi Rep.") ~ "SF",
  round %in% c("Qualif Final", "Qualifier", "Minor Qualif", "Major Qualif") ~ "QF",
  round %in% c("Elim Qualif", "Elim Prelim Final") ~ "EF",
  round %in% c("Grand Final", "Final", "Grand Final Rep.") ~ "GF",
  round %in% c("Grand Final Chall.") ~ "GFC",
  round %in% c("Playoff") ~ "PO",
  .default = round
)

##### Helper Stats #####
###### Player Career Summaries ######
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
  
###### Player Career year Span ######
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

###### Player Initials ######
player_initials <- player_data |>
  filter(first_name != "?") |>
  mutate(first_name_initial = substr(first_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = rev(LETTERS), ordered = TRUE),
         last_name_initial = substr(last_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE),
         initials = paste0(first_name_initial, last_name_initial)) |>
  select(player_id, full_name, initials)

###### Produce Team Lineups #####
produce_team_lineups <- function(id) {
  correct_positions <- c("FB", "W", "C", "C", "W", "FE", "HB", "FR", "HK", "FR", "2R", "2R", "L")
  
  player_match_data |>
    filter(match_id == id) |>
    select(team, position, player_id) |>
    arrange(team) |>
    group_by(team, position) |>
    mutate(position_unique = case_when(
            row_number() > 1 ~ paste0(position, row_number()),
            .default = position
    )) |>
    ungroup() |>
    pivot_wider(id_cols = c(position, position_unique), 
                names_from = team, values_from = player_id) |>
    select(position, 3, 4)
}

###### Anagram Solver ######
anagram_finder <- function(word, player) {
  word_split <- sort(strsplit(word |> tolower(), "")[[1]]) |> table()
  player_split <- sort(strsplit(player |> tolower(), "")[[1]]) |> table()
  
  has_word <- ifelse(
    all(names(word_split) %in% names(player_split)),
    ifelse(all(player_split[names(word_split)] >= word_split), TRUE, FALSE),
    FALSE)
  
  return(has_word)
}

player_data |>
  rowwise() |>
  mutate(has_word = anagram_finder("magician", full_name)) |>
  filter(has_word)

###### Match Results for each team ######
matchups_by_team <- player_match_data |>
  select(match_id, team, opposition_team) |>
  distinct()

match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, date, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  left_join(matchups_by_team, by = c("match_id", "team")) |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, date, round, home_away, team, opposition_team, score_for, score_against, result)

##### Premiership Database #####
###### Minor Premierships ######
minor_premiership_winners <- ladder_data |>
  group_by(competition_year, year) |>
  filter(round == max(round)) |>
  filter(row_number() == 1) |>
  filter(year != 2026) |>
  ungroup() |>
  select(competition_year, year, team)

###### Wooden Spoons ######
wooden_spoon_winners <- ladder_data |>
  group_by(competition_year, year) |>
  filter(round == max(round)) |>
  filter(row_number() == max(row_number())) |>
  filter(year != 2026) |>
  ungroup() |>
  select(competition_year, year, team)

###### Grand Finals ######
grand_final_winners <- match_results |>
  select(-c(match_id, date, opposition_team)) |>
  filter(round %in% c("Final", "Grand Final", "Grand Final Rep.", "Grand Final Chall.")) |>
  # Special Cases
  rbind(data.frame(competition_year = "NSWRFL 1909",
                   year = 1909,
                   round = "Final",
                   home_away = c("H", "A"),
                   team = c("South Sydney Rabbitohs", "Balmain Tigers"),
                   score_for = NA,
                   score_against = NA,
                   result = c("W", "L"))) |>
  filter(result == "W") |>
  group_by(competition_year) |>
  filter(row_number() == max(row_number())) |>
  ungroup() |>
  arrange(year) |>
  select(-c(home_away, result))

###### Premiership Dataframe ######
premierships <- minor_premiership_winners |>
  rename(`Minor Premiership` = team) |>
  left_join(grand_final_winners |> select(competition_year, team), 
            by = "competition_year") |>
  mutate(team = ifelse(year == 2025, "-", team),
         team = ifelse(is.na(team) , `Minor Premiership`, team)) |>
  rename(Premiership = team) |>
  left_join(wooden_spoon_winners |> select(competition_year, team),
            by = "competition_year") |>
  rename(`Wooden Spoon` = team)

write_csv(premierships, "helper_stats/premiership_data.csv")


##### Time in years/days #####
# timespan in years and days
time_years_days <- function(start_date, end_date) {
  year_span <- interval(start_date, end_date) |>
    time_length("years") |>
    floor()
  day_span <- interval(start_date %m+% lubridate::years(year_span), end_date) |>
    time_length("days") |>
    as.numeric()
  time_span <- paste0(year_span, "yrs ", day_span, " days")
  return(time_span)
}