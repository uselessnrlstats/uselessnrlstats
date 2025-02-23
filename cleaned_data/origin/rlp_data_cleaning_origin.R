##### Description #####
# Script to join and clean data scraped from rugby league project.

##### Libraries #####
library(chron)
library(lubridate)
library(tidyverse)
library(tools)

##### match_data #####
match_data_cleaned <- read_csv("data/origin/match_data_origin.csv", show_col_types = FALSE) |>
  select(-...1) |>
  unique() |>
  mutate(home_team = toTitleCase(tolower(home_team)),
         away_team = toTitleCase(tolower(away_team))) |>
  mutate(date = as_date(gsub("\\w+, (\\d+)\\w+ (\\w+), (\\d+)", "\\1 \\2 \\3", date),
                        format = "%d %B %Y"),
         time = gsub("(\\d+:\\d+ \\w+) \\(local time\\)", "\\1", time),
         time_24hr = times(format(strptime(time, "%I:%M %p"), "%H:%M:%S"))) |>
  relocate(time_24hr, .after = time) |>
  arrange(date) |>
  group_by(year) |>
  mutate(match_no = row_number()) |>
  ungroup() |>
  relocate(match_no, .after = year)

write_csv(match_data_cleaned, 
          "origin/cleaned_data/match_data.csv")

##### team_data #####
###### Import data ######
team_data <- read_csv("data/origin/team_data_origin.csv", show_col_types = FALSE) |>
  select(-...1)

team_data_cleaned <- team_data |>
  unique() |>
  mutate(team_name = toTitleCase(tolower(team_name)),
         team_colour = c("#B03060", "#87CEFF"))

write_csv(team_data_cleaned, 
          "origin/cleaned_data/team_data.csv")

##### player_summary_data #####
player_summary_data <- read_csv("data/origin/player_summary_data.csv", show_col_types = FALSE) |>
  select(-...1) |>
  distinct()
##### player_data #####
player_names_data_cleaned <- read_csv("data/origin/player_names_data_origin.csv", show_col_types = FALSE) |>
  select(-...1) |>
  mutate(player_id = as.numeric(player_id)) |>
  distinct() |>
  mutate(
    full_name = gsub("(.+)\\s'.+'\\s(.+)", "\\1 \\2", player_name2),
    nickname = if_else(
      grepl(".+\\s'(.+)'\\s.+", player_name2),
      gsub(".+\\s'(.+)'\\s.+", "\\1", player_name2),
      NA)
  ) |>
  mutate(
    birthday = if_else(
      grepl("\\w+, \\d+\\w+ \\w+, \\d+", birthday),
      birthday,
      NA),
    birthday = as_date(gsub("\\w+, (\\d+)\\w+ (\\w+), (\\d+)", "\\1 \\2 \\3", birthday),
                       format = "%d %B %Y"),
    birthdate = if_else(
      !is.na(birthday),
      format(birthday, format = "%d-%m"),
      NA)
  ) |>
  left_join(player_summary_data |> select(-player_birthday),
            by = "player_id") |>
  select(player_id, full_name, player_name1, player_name_comma, nickname, birthday, birthdate, birthplace, total_matches, total_points) |>
  # Fix specific issues
  mutate(full_name = ifelse(player_id == 9712 & full_name == "H Wright", "Harold Wright", full_name),
         player_name1 = ifelse(player_id == 9712 & full_name == "H Wright", "Harold WRIGHT", player_name1)) |>
  mutate(first_name = gsub("^(.+), (.+)$","\\2", player_name_comma),
         last_name = ifelse(
           first_name == "?",
           gsub("^\\? (.+)$", "\\1", full_name),
           Vectorize(gsub)(paste0("^", first_name, " (.+)$"), "\\1", full_name))) |>
  relocate(first_name, last_name, .after = full_name) |>
  select(-player_name1, -player_name_comma, -total_points, -total_matches)
 
write_csv(player_names_data_cleaned, 
          "origin/cleaned_data/player_data.csv")

##### player_scoring_data #####
player_scoring_data_cleaned <- read_csv("data/origin/player_scoring_data_origin.csv", show_col_types = FALSE) |>
  select(-...1) |>
  mutate(n_scores = as.character(n_scores)) |>
  distinct() |>
  pivot_wider(id_cols = c(match_id, player_id),
              names_from = score, values_from = n_scores, values_fill = "0") |>
  rename(tries = `T`, 
         goals = G, 
         field_goals = FG, 
         #field_goals2 = `FG-2`, 
         penalty_tries = PT, 
         send_offs= OFF, 
         sin_bins = BIN, 
         sin_bins5 = BIN5) |>
  mutate(field_goals2 = 0) |> relocate(field_goals2, .after = field_goals) |>
  mutate(goal_attempts = ifelse(
           grepl("(\\d+)/(\\d+)", goals),
           gsub("(\\d+)/(\\d+)", "\\2", goals), NA),
         goals = gsub("(\\d+)/(\\d+)", "\\1", goals)) |>
  mutate(across(everything(), as.numeric)) |>
  select(match_id, player_id, tries, penalty_tries, goals, goal_attempts, field_goals, field_goals2, sin_bins5, sin_bins, send_offs)
##### player_match_data #####
player_match_data_cleaned <- read_csv("data/origin/player_match_data_origin.csv", show_col_types = FALSE) |>
  select(-...1) |>
  mutate(number = as.character(number)) |>
  mutate(team = toTitleCase(tolower(team))) |>
  left_join(player_scoring_data_cleaned, by = c("match_id", "player_id")) |>
  mutate_at(vars(tries, penalty_tries, goals, field_goals, field_goals2, sin_bins5, sin_bins, send_offs), ~replace_na(., 0)) |>
  left_join(match_data_cleaned |> select(match_id, date), by = "match_id") |>
  mutate(year = year(date),
         points = case_when(
           year < 1971 ~ 3 * (tries + penalty_tries) + 2 * (goals + field_goals),
           year >= 1971 & year < 1983 ~ 3 * (tries + penalty_tries) + 2 * goals + 1 * field_goals,
           year >= 1983 ~ 4 * (tries + penalty_tries) + 2 * (goals + field_goals2) + 1 * field_goals
         )) |>
  mutate(number = as.numeric(ifelse(number == "-", NA, number))) |>
  select(-date, -year)

write_csv(player_match_data_cleaned, 
          "origin/cleaned_data/player_match_data.csv")

##### coach_summary_data #####
# coach_summary_data <- read_csv("data/origin/coach_summary_data.csv", show_col_types = FALSE) |>
#   select(-...1) |>
#   distinct()
# coach_data_cleaned <- coach_summary_data |>
#   distinct() |>
#   mutate(
#     full_name = gsub("(.+)\\s'.+'\\s(.+)", "\\1 \\2", coach_name),
#     nickname = if_else(
#       grepl(".+\\s'(.+)'\\s.+", coach_name),
#       gsub(".+\\s'(.+)'\\s.+", "\\1", coach_name),
#       NA)
#   ) |>
#   mutate(
#     birthday = as_date(coach_birthday,
#                        format = "%Y-%m-%d"),
#     birthdate = if_else(
#       !is.na(birthday),
#       format(birthday, format = "%d-%m"),
#       NA)
#   ) |>
#   mutate(first_name = gsub("^(.+), (.+)$","\\2", coach_name_comma),
#          last_name = ifelse(
#            first_name == "?",
#            gsub("^\\? (.+)$", "\\1", full_name),
#            Vectorize(gsub)(paste0("^", first_name, " (.+)$"), "\\1", full_name))) |>
#   select(coach_id, full_name, first_name, last_name, nickname, birthday, birthdate)
# 
# write_csv(coach_data_cleaned, 
#           "origin/cleaned_data/coach_data.csv")
##### coach_match_data #####
coach_match_data_cleaned <- read_csv("data/origin/coach_match_data_origin.csv", show_col_types = FALSE) |>
  select(-...1) |>
  distinct() |>
  mutate(team = toTitleCase(tolower(team)))

write_csv(coach_match_data_cleaned, 
          "origin/cleaned_data/coach_match_data.csv")
##### ref_summary_data #####
###### Import data ######
ref_summary_data <- read_csv("data/origin/ref_summary_data.csv", show_col_types = FALSE) |>
  select(-...1) |>
  distinct()
###### Clean data ######
ref_data_cleaned <- ref_summary_data |>
  distinct() |>
  mutate(
    full_name = gsub("(.+)\\s'.+'\\s(.+)", "\\1 \\2", ref_name),
    nickname = if_else(
      grepl(".+\\s'(.+)'\\s.+", ref_name),
      gsub(".+\\s'(.+)'\\s.+", "\\1", ref_name),
      NA)
  ) |>
  mutate(
    birthday = as_date(ref_birthday,
                       format = "%d %B %Y"),
    birthdate = if_else(
      !is.na(birthday),
      format(birthday, format = "%d-%m"),
      NA)
  ) |>
  mutate(first_name = gsub("^(.+), (.+)$","\\2", ref_name_comma),
         last_name = ifelse(
           first_name == "?",
           gsub("^\\? (.+)$", "\\1", full_name),
           Vectorize(gsub)(paste0("^", first_name, " (.+)$"), "\\1", full_name))) |>
  select(ref_id, full_name, first_name, last_name, nickname, birthday, birthdate)

write_csv(ref_data_cleaned, 
          "origin/cleaned_data/ref_data.csv")
##### ref_match_data #####
ref_match_data_cleaned <- read_csv("data/origin/ref_match_data_origin.csv", show_col_types = FALSE) |>
  select(-...1)

write_csv(ref_match_data_cleaned, 
          "origin/cleaned_data/ref_match_data.csv")
##### venue_data #####
###### Import data ######
venue_data <- read_csv("data/origin/venue_data_origin.csv", show_col_types = FALSE) |>
  select(-...1) |>
  mutate(venue_id = as.numeric(venue_id))

###### Clean data ######
cities <- c("Adelaide", "Albury", "Auckland", "Bathurst", "Brisbane", "Bundaberg", "Cairns", "Campbelltown", "Canberra", "Christchurch", "Coffs Harbour", "Cootamundra", "Darwin", "Dubbo", "Dunedin", "Gladstone", "Gold Coast", "Gosford", "Goulburn", "Hamilton", "Las Vegas", "Long Beach", "Mackay", "Melbourne", "Mudgee", "Napier", "New Plymouth", "Newcastle", "Palmerston North", "Parkes", "Perth", "Queanbeyan", "Rockhampton", "Sydney", "Tamworth", "Taupo", "Toowoomba", "Townsville", "Tweed Heads", "Wagga Wagga", "Wellington", "Whyalla", "Wollongong")
states <- c("Auckland", "Australian Capital Territory", "California", "Canterbury", "Hawke's Bay", "Manawatu-Wanganui", "Nevada", "New South Wales", "Northern Territory", "Otago", "Queensland", "South Australia", "Taranaki", "Victoria", "Waikato", "Wellington", "Western Australia")
countries <- c("Australia", "New Zealand", "United States of America")

venue_data_cleaned <- venue_data |>
  mutate(location = gsub("\n", ", ", location)) |>
  rowwise() |>
  mutate(country = countries[which(countries %in% str_split(location, ", ")[[1]])],
         states = states[which(states %in% str_split(location, ", ")[[1]])],
         cities = cities[which(cities %in% str_split(location, ", ")[[1]])])

write_csv(venue_data_cleaned, 
          "origin/cleaned_data/venue_data.csv")
