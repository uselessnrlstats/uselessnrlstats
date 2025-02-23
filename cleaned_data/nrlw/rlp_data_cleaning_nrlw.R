##### Description #####
# Script to join and clean data scraped from rugby league project.

##### Libraries #####
library(chron)
library(lubridate)
library(tidyverse)
library(tools)

##### match_data #####
###### Import data ######
match_data <- map(
  .x = list.files(
    "data/nrlw/match_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1)
  }) |>
  list_rbind()
###### Clean data ######
match_data_cleaned <- match_data |>
  unique() |>
  mutate(competition_year = competition,
         competition = gsub("(\\D+) \\d+", "\\1", competition)) |>
  relocate(competition_year, .after = competition) |>
  mutate(home_team = toTitleCase(tolower(home_team)),
         home_team = gsub("\n", " ", home_team),
         home_team = gsub(" \\(w\\)", "", home_team),
         home_team = paste0(home_team, " (W)"),
         away_team = toTitleCase(tolower(away_team)),
         away_team = gsub("\n", " ", away_team),
         away_team = gsub(" \\(w\\)", "", away_team),
         away_team = paste0(away_team, " (W)")) |>
  mutate(date = as_date(gsub("\\w+, (\\d+)\\w+ (\\w+), (\\d+)", "\\1 \\2 \\3", date),
                        format = "%d %B %Y"),
         time = gsub("(\\d+:\\d+ \\w+) \\(local time\\)", "\\1", time),
         time_24hr = times(format(strptime(time, "%I:%M %p"), "%H:%M:%S"))) |>
  relocate(time_24hr, .after = time)

write_csv(match_data_cleaned, 
          "cleaned_data/nrlw/match_data.csv")

##### team_data #####
###### Import data ######
team_data <- map(
  .x = list.files(
    "data/nrlw/team_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1)
  }) |> 
  list_rbind()
###### Clean data ######
team_mascots <- c("Broncos",
                  "Raiders",
                  "Sharks",
                  "Titans",
                  "Warriors",
                  "Knights",
                  "Cowboys",
                  "Eels",
                  "Dragons",
                  "Roosters",
                  "Tigers")

team_abbr <- c("BRI",
               "CAN",
               "CRO",
               "GCT",
               "NZW",
               "KNI",
               "NQC",
               "PAR",
               "SGI",
               "SYD",
               "WES")

team_data_cleaned <- team_data |>
  unique() |>
  mutate(team_short = ifelse(team_short == "Warriors", "New Zealand", team_short)) |>
  mutate(team_short = toTitleCase(tolower(team_short)),
         team_short = gsub("\n", " ", team_short),
         team_name = toTitleCase(tolower(team_name)),
         team_name = gsub("\n", " ", team_name),
         team_name = gsub(" \\(w\\)", "", team_name),
         team_name = paste0(team_name, " (W)")) |>
  arrange(team_short) |>
  mutate(team_mascots = team_mascots,
         team_abbr = team_abbr)

write_csv(team_data_cleaned, 
          "cleaned_data/nrlw/team_data.csv")

##### player_summary_data #####
###### Import data ######
player_summary_data <- read_csv("data/nrlw/player_summary_data.csv", show_col_types = FALSE) |>
  select(-...1) |>
  distinct()
##### player_data #####
###### Import data ######
player_names_data <- map(
  .x = list.files(
    "data/nrlw/player_names_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1) |>
      mutate(player_id = as.numeric(player_id))
  }) |> 
  list_rbind()
###### Clean data ######
player_names_data_cleaned <- player_names_data |>
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
  select(-player_name1, -player_name_comma)
 
write_csv(player_names_data_cleaned, 
          "cleaned_data/nrlw/player_data.csv")

##### player_scoring_data #####
###### Import data ######
player_scoring_data <- map(
  .x = list.files(
    "data/nrlw/player_scoring_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1) |>
      mutate(n_scores = as.character(n_scores))
  }) |>
  list_rbind()
###### Clean data ######
player_scoring_data_cleaned <- player_scoring_data |>
  distinct() |>
  pivot_wider(id_cols = c(match_id, player_id),
              names_from = score, values_from = n_scores, values_fill = "0") |>
  rename(tries = `T`, 
         goals = G, 
         field_goals = FG, 
         #field_goals2 = `FG-2`, 
         #penalty_tries = PT, 
         send_offs= OFF, 
         sin_bins = BIN) |>
  mutate(field_goals2 = 0) |> relocate(field_goals2, .after = field_goals) |>
  mutate(penalty_tries = 0) |> relocate(penalty_tries, .after = field_goals2) |>
  mutate(goal_attempts = ifelse(
           grepl("(\\d+)/(\\d+)", goals),
           gsub("(\\d+)/(\\d+)", "\\2", goals), NA),
         goals = gsub("(\\d+)/(\\d+)", "\\1", goals)) |>
  mutate(across(everything(), as.numeric)) |>
select(match_id, player_id, tries, penalty_tries, goals, goal_attempts, field_goals, field_goals2, sin_bins, send_offs)
##### player_match_data #####
###### Import data ######
player_match_data <- map(
  .x = list.files(
    "data/nrlw/player_match_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1) |>
      mutate(number = as.character(number))
  }) |>
  list_rbind()

###### Clean data ######
player_match_data_cleaned <- player_match_data |>
  mutate(team = toTitleCase(tolower(team)),
         team = gsub("\n", " ", team),
         team = gsub(" \\(w\\)", "", team),
         team = paste0(team, " (W)"),
         opposition_team = toTitleCase(tolower(opposition_team)),
         opposition_team = gsub("\n", " ", opposition_team),
         opposition_team = gsub(" \\(w\\)", "", opposition_team),
         opposition_team = paste0(opposition_team, " (W)")) |>
  left_join(player_scoring_data_cleaned, by = c("match_id", "player_id")) %>%
  mutate_at(vars(tries, penalty_tries, goals, field_goals, field_goals2, sin_bins, send_offs), ~replace_na(., 0)) |>
  mutate(points = 4 * (tries + penalty_tries) + 2 * (goals + field_goals2) + 1 * field_goals) |>
  mutate(number = as.numeric(ifelse(number == "-", NA, number)))

write_csv(player_match_data_cleaned, 
          "cleaned_data/nrlw/player_match_data.csv")

##### coach_summary_data #####
###### Import data ######
coach_summary_data <- read_csv("data/nrlw/coach_summary_data.csv", show_col_types = FALSE) |>
  select(-...1) |>
  distinct()
###### Clean data ######
coach_data_cleaned <- coach_summary_data |>
  distinct() |>
  mutate(
    full_name = gsub("(.+)\\s'.+'\\s(.+)", "\\1 \\2", coach_name),
    nickname = if_else(
      grepl(".+\\s'(.+)'\\s.+", coach_name),
      gsub(".+\\s'(.+)'\\s.+", "\\1", coach_name),
      NA)
  ) |>
  mutate(
    birthday = as_date(coach_birthday,
                       format = "%Y-%m-%d"),
    birthdate = if_else(
      !is.na(birthday),
      format(birthday, format = "%d-%m"),
      NA)
  ) |>
  mutate(first_name = gsub("^(.+), (.+)$","\\2", coach_name_comma),
         last_name = ifelse(
           first_name == "?",
           gsub("^\\? (.+)$", "\\1", full_name),
           Vectorize(gsub)(paste0("^", first_name, " (.+)$"), "\\1", full_name))) |>
  select(coach_id, full_name, first_name, last_name, nickname, birthday, birthdate)

write_csv(coach_data_cleaned, 
          "cleaned_data/nrlw/coach_data.csv")
##### coach_match_data #####
###### Import data ######
coach_match_data <- map(
  .x = list.files(
    "data/nrlw/coach_match_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1)
  }) |>
  list_rbind()
###### Clean data ######
coach_match_data_cleaned <- coach_match_data |>
  distinct() |>
  mutate(team = toTitleCase(tolower(team)),
         team = gsub("\n", " ", team),
         team = gsub(" \\(w\\)", "", team),
         team = paste0(team, " (W)"))

write_csv(coach_match_data_cleaned, 
          "cleaned_data/nrlw/coach_match_data.csv")
##### ref_summary_data #####
###### Import data ######
ref_summary_data <- read_csv("data/nrlw/ref_summary_data.csv", show_col_types = FALSE) |>
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
          "cleaned_data/nrlw/ref_data.csv")
##### ref_match_data #####
###### Import data ######
ref_match_data <- map(
  .x = list.files(
    "data/nrlw/ref_match_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1)
  }) |>
  list_rbind()
###### Clean data ######
ref_match_data_cleaned <- ref_match_data

write_csv(ref_match_data_cleaned, 
          "cleaned_data/nrlw/ref_match_data.csv")
##### venue_data #####
###### Import data ######
venue_data <- map(
  .x = list.files(
    "data/nrlw/venue_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1) |>
      mutate(venue_id = as.numeric(venue_id))
  }) |>
  list_rbind()
###### Clean data ######
cities <- c("Adelaide", "Albury", "Auckland", "Bathurst", "Brisbane", "Bundaberg", "Cairns", "Campbelltown", "Canberra", "Christchurch", "Coffs Harbour", "Cootamundra", "Darwin", "Dubbo", "Dunedin", "Gladstone", "Gold Coast", "Gosford", "Goulburn", "Hamilton", "Las Vegas", "Mackay", "Melbourne", "Mudgee", "Napier", "New Plymouth", "Newcastle", "Palmerston North", "Parkes", "Perth", "Queanbeyan", "Rockhampton", "Sydney", "Tamworth", "Taupo", "Toowoomba", "Townsville", "Tweed Heads", "Wagga Wagga", "Wellington", "Whyalla", "Wollongong")
states <- c("Auckland", "Australian Capital Territory", "Canterbury", "Hawke's Bay", "Manawatu-Wanganui", "Nevada", "New South Wales", "Northern Territory", "Otago", "Queensland", "South Australia", "Taranaki", "Victoria", "Waikato", "Wellington", "Western Australia")
countries <- c("Australia", "New Zealand", "United States of America")

venue_data_cleaned <- venue_data |>
  mutate(location = gsub("\n", ", ", location)) |>
  rowwise() |>
  mutate(country = countries[which(countries %in% str_split(location, ", ")[[1]])],
         states = states[which(states %in% str_split(location, ", ")[[1]])],
         cities = cities[which(cities %in% str_split(location, ", ")[[1]])])

write_csv(venue_data_cleaned, 
          "cleaned_data/nrlw/venue_data.csv")
##### team_logos #####
team_unique_names <- c("Brisbane Broncos (W)",
                       "Canberra Raiders (W)",
                       "Cronulla Sharks (W)",
                       "Gold Coast Titans (W)",
                       "New Zealand Warriors (W)",
                       "Newcastle Knights (W)",
                       "North Queensland Cowboys (W)",
                       "Parramatta Eels (W)",
                       "St George Illawarra Dragons (W)",
                       "Sydney Roosters (W)",
                       "Wests Tigers (W)")

team_colour <- c("#fbe051", # Brisbane Broncos
                 "#95c94a", # Canberra Raiders
                 "#6ee2e0", # Cronulla Sutherland Sharks
                 "#1e90ff", # Gold Coast Titans
                 "#05a655", # New Zealand Warriors
                 "#256cb3", # Newcastle Knights
                 "#002b5c", # North Queensland Cowboys
                 "#e6b123", # Parramatta Eels
                 "#f36c6c", # St George Illawarra Dragons
                 "#cb2022", # Sydney Roosters
                 "#ed771e" # Wests Tigers
)

team_logo_paths <- c("images/logos/Brisbane Broncos.png",
                     "images/logos/Canberra Raiders.png",
                     "images/logos/Cronulla Sutherland Sharks.png",
                     "images/logos/Gold Coast Titans.png",
                     "images/logos/New Zealand Warriors.png",
                     "images/logos/Newcastle Knights.png",
                     "images/logos/North Queensland Cowboys.png",
                     "images/logos/Parramatta Eels.png",
                     "images/logos/St George Illawarra Dragons.png",
                     "images/logos/Sydney Roosters.png",
                     "images/logos/Wests Tigers.png"
)

team_logos <- tibble(
  team_name = team_unique_names,
  team_colour = team_colour,
  logo_path = team_logo_paths
)

write_csv(team_logos, 
          "cleaned_data/nrlw/team_logos.csv")
##### Potentially common mutations #####
# year = year(date)
# year_date = format(date, format = "%d-%m")
# month = month(date)
# day = day(date)
# day_of_week = wday(date, label = TRUE)

