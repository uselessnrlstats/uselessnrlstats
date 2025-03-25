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
    "data/nrl/match_data/",
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
         home_team = gsub("\r", "", home_team),
         away_team = toTitleCase(tolower(away_team)),
         away_team = gsub("\n", " ", away_team),
         away_team = gsub("\r", "", away_team)) |>
  mutate(date = as_date(gsub("\\w+, (\\d+)\\w+ (\\w+), (\\d+)", "\\1 \\2 \\3", date),
                        format = "%d %B %Y"),
         time = gsub("(\\d+:\\d+ \\w+) \\(local time\\)", "\\1", time),
         time_24hr = times(format(strptime(time, "%I:%M %p"), "%H:%M:%S"))) |>
  relocate(time_24hr, .after = time) |>
  # Fix Sharks naming issue in 2019
  mutate(home_team = ifelse(competition_year == "NRL 1999" & home_team == "Sharks", "Cronulla Sutherland Sharks", home_team),
         away_team = ifelse(competition_year == "NRL 1999" & away_team == "Sharks", "Cronulla Sutherland Sharks", away_team)) |>
  # Call Dargons back to Dragons
  mutate(home_team = ifelse(home_team == "St George Illawarra Dargons", "St George Illawarra Dragons", home_team),
         away_team = ifelse(away_team == "St George Illawarra Dargons", "St George Illawarra Dragons", away_team))

write_csv(match_data_cleaned, 
          "cleaned_data/nrl/match_data.csv")

##### team_data #####
###### Import data ######
team_data <- map(
  .x = list.files(
    "data/nrl/team_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1)
  }) |> 
  list_rbind()
###### Clean data ######
team_ids <- c("Adelaide Rams", 
              "Annandale", 
              "New Zealand Warriors", 
              "Balmain Tigers",
              "Brisbane Broncos",
              "Canterbury Bankstown Bulldogs",
              "Canberra Raiders",
              "Canterbury Bankstown Bulldogs",
              "Canterbury Bankstown Bulldogs",
              "Cronulla Sutherland Sharks",
              "Cumberland",
              "Dolphins",
              "Sydney Roosters",
              "Sydney Roosters",
              "Glebe",
              "Gold Coast Titans",
              "Gold Coast/Tweed Heads Giants",
              "Gold Coast Seagulls",
              "Gold Coast Chargers",
              "Hunter Mariners",
              "Illawarra Steelers",
              "Manly Warringah Sea Eagles",
              "Melbourne Storm",
              "New Zealand Warriors",
              "Newcastle",
              "Newcastle Knights",
              "Newtown Jets",
              "Newtown Jets",
              "North Queensland Cowboys",
              "North Sydney Bears",
              "North Sydney Bears",
              "Northern Eagles",
              "Parramatta Eels",
              "Penrith Panthers",
              "Cronulla Sutherland Sharks",
              "South Queensland Crushers",
              "South Sydney Rabbitohs",
              "St George Illawarra Dragons",
              "St George Dragons",
              "Sydney Roosters",
              "Canterbury Bankstown Bulldogs",
              "Sydney Roosters",
              "Balmain Tigers",
              "University",
              "New Zealand Warriors",
              "Western Reds",
              "Western Suburbs Magpies",
              "Wests Tigers")

team_mascots <- c("Rams", 
                  "Annandale", 
                  "Warriors", 
                  "Balmain Tigers",
                  "Broncos",
                  "Bulldogs",
                  "Raiders",
                  "Bulldogs",
                  "Bulldogs",
                  "Sharks",
                  "Cumberland",
                  "Dolphins",
                  "Roosters",
                  "Roosters",
                  "Glebe",
                  "Titans",
                  "Giants",
                  "Seagulls",
                  "Chargers",
                  "Mariners",
                  "Steelers",
                  "Sea Eagles",
                  "Storm",
                  "Warriors",
                  "Newcastle",
                  "Knights",
                  "Jets",
                  "Jets",
                  "Cowboys",
                  "Bears",
                  "Bears",
                  "Eagles",
                  "Eels",
                  "Panthers",
                  "Sharks",
                  "Crushers",
                  "Rabbitohs",
                  "Dragons",
                  "St George Dragons",
                  "Roosters",
                  "Bulldogs",
                  "Roosters",
                  "Balmain Tigers",
                  "University",
                  "Warriors",
                  "Reds",
                  "Magpies",
                  "Tigers")

team_abbr <- c("ADE", 
               "ANN", 
               "NZW", 
               "BAL",
               "BRI",
               "CBY",
               "CAN",
               "CBY",
               "CBY",
               "CRO",
               "CBL",
               "DOL",
               "SYD",
               "SYD",
               "GLE",
               "GCT",
               "GCG",
               "GCS",
               "GCC",
               "HUN",
               "ILL",
               "MAN",
               "MEL",
               "NZW",
               "NEW",
               "KNI",
               "NTJ",
               "NTJ",
               "NQC",
               "NSB",
               "NSB",
               "NTE",
               "PAR",
               "PEN",
               "CRO",
               "SQC",
               "SOU",
               "SGI",
               "STG",
               "SYD",
               "CBY",
               "SYD",
               "BAL",
               "UNI",
               "NZW",
               "WTR",
               "WSM",
               "WES")

team_data_cleaned <- team_data |>
  unique() |>
  filter(team_name != "St George Illawarra\r\nDARGONS") |>
  filter(team_short != "St George Illawarra") |>
  mutate(team_short = toTitleCase(tolower(team_short)),
         team_short = gsub("\n", " ", team_short),
         team_short = gsub("\r", "", team_short),
         team_name = toTitleCase(tolower(team_name)),
         team_name = gsub("\n", " ", team_name),
         team_name = gsub("\r", "", team_name)) |>
  arrange(team_short) |>
  distinct() |>
  mutate(team_unique = team_ids,
         team_mascots = team_mascots,
         team_abbr = team_abbr)

write_csv(team_data_cleaned, 
          "cleaned_data/nrl/team_data.csv")

##### player_summary_data #####
###### Import data ######
player_summary_data <- read_csv("data/nrl/player_summary_data.csv", show_col_types = FALSE) |>
  select(-...1) |>
  distinct()
##### player_data #####
###### Import data ######
player_names_data <- map(
  .x = list.files(
    "data/nrl/player_names_data/",
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
  ##%%##%%##%%##%% TEMPORARY LAST NAME FIX ##%%##%%##%%##%%
  mutate(
    player_name_comma = ifelse(is.na(player_name_comma), gsub("^(.+) (.+)$", "\\2, \\1", player_name1), player_name_comma)) |>
  # Fix specific issues
  mutate(full_name = ifelse(player_id == 48731, "Casey McLean", full_name)) |>
  mutate(first_name = gsub("^(.+), (.+)$","\\2", player_name_comma),
         last_name = ifelse(
           first_name == "?",
           gsub("^\\? (.+)$", "\\1", full_name),
           Vectorize(gsub)(paste0("^", first_name, " (.+)$"), "\\1", full_name))) |>
  relocate(first_name, last_name, .after = full_name) |>
  select(-player_name1, -player_name_comma)
 
write_csv(player_names_data_cleaned, 
          "cleaned_data/nrl/player_data.csv")

##### player_scoring_data #####
###### Import data ######
player_scoring_data <- map(
  .x = list.files(
    "data/nrl/player_scoring_data/",
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
  rename(tries = `T`, goals = G, field_goals = FG, field_goals2 = `FG-2`, penalty_tries = PT, send_offs= OFF, sin_bins = BIN, sin_bins5 = BIN5) |>
  mutate(goal_attempts = ifelse(
           grepl("(\\d+)/(\\d+)", goals),
           gsub("(\\d+)/(\\d+)", "\\2", goals), NA),
         goals = gsub("(\\d+)/(\\d+)", "\\1", goals)) |>
  mutate(across(everything(), as.numeric)) |>
select(match_id, player_id, tries, penalty_tries, goals, goal_attempts, field_goals, field_goals2, sin_bins5, sin_bins, send_offs)
##### player_match_data #####
###### Import data ######
player_match_data <- map(
  .x = list.files(
    "data/nrl/player_match_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1) |>
      mutate(number = as.character(number))
  }) |>
  list_rbind()
###### Clean data ######
sharks1999_match_ids <- match_data_cleaned |> 
  filter(home_team == "Cronulla Sutherland Sharks" | away_team == "Cronulla Sutherland Sharks") |>
  pull(match_id)

player_match_data_cleaned <- player_match_data |>
  mutate(team = toTitleCase(tolower(team)),
         team = gsub("\n", " ", team),
         team = gsub("\r", "", team),
         opposition_team = toTitleCase(tolower(opposition_team)),
         opposition_team = gsub("\n", " ", opposition_team),
         opposition_team = gsub("\r", "", opposition_team)) |>
  left_join(player_scoring_data_cleaned, by = c("match_id", "player_id")) %>%
  mutate_at(vars(tries, penalty_tries, goals, field_goals, field_goals2, sin_bins5, sin_bins, send_offs), ~replace_na(., 0)) |>
  left_join(match_data_cleaned |> select(match_id, date), by = "match_id") |>
  mutate(year = year(date),
         points = case_when(
           year < 1971 ~ 3 * (tries + penalty_tries) + 2 * (goals + field_goals),
           year >= 1971 & year < 1983 ~ 3 * (tries + penalty_tries) + 2 * goals + 1 * field_goals,
           year >= 1983 ~ 4 * (tries + penalty_tries) + 2 * (goals + field_goals2) + 1 * field_goals
         )) |>
  mutate(number = as.numeric(ifelse(number == "-", NA, number))) |>
  select(-date, -year) |>
  # Clean Sharks 1999
  mutate(team = ifelse(match_id %in% sharks1999_match_ids & team == "Sharks", "Cronulla Sutherland Sharks", team),
         opposition_team = ifelse(match_id %in% sharks1999_match_ids & opposition_team == "Sharks", "Cronulla Sutherland Sharks", opposition_team)) |>
  # Clean Dargons
  mutate(team = ifelse(team == "St George Illawarra Dargons", "St George Illawarra Dragons", team),
         opposition_team = ifelse(opposition_team == "St George Illawarra Dargons", "St George Illawarra Dragons", opposition_team))

write_csv(player_match_data_cleaned, 
          "cleaned_data/nrl/player_match_data.csv")

##### coach_summary_data #####
###### Import data ######
coach_summary_data <- read_csv("data/nrl/coach_summary_data.csv", show_col_types = FALSE) |>
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
          "cleaned_data/nrl/coach_data.csv")
##### coach_match_data #####
###### Import data ######
coach_match_data <- map(
  .x = list.files(
    "data/nrl/coach_match_data/",
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
         team = gsub("\r", "", team)) |>
  # Clean Sharks 1999
  mutate(team = ifelse(match_id %in% sharks1999_match_ids & team == "Sharks", "Cronulla Sutherland Sharks", team)) |>
  # Clean Dargons
  mutate(team = ifelse(team == "St George Illawarra Dargons", "St George Illawarra Dragons", team))

write_csv(coach_match_data_cleaned, 
          "cleaned_data/nrl/coach_match_data.csv")
##### ref_summary_data #####
###### Import data ######
ref_summary_data <- read_csv("data/nrl/ref_summary_data.csv", show_col_types = FALSE) |>
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
          "cleaned_data/nrl/ref_data.csv")
##### ref_match_data #####
###### Import data ######
ref_match_data <- map(
  .x = list.files(
    "data/nrl/ref_match_data/",
    full.names = TRUE),
  .f = function(.x) {
    read_csv(file = .x, show_col_types = FALSE) |>
      select(-...1)
  }) |>
  list_rbind()
###### Clean data ######
ref_match_data_cleaned <- ref_match_data

write_csv(ref_match_data_cleaned, 
          "cleaned_data/nrl/ref_match_data.csv")
##### venue_data #####
###### Import data ######
venue_data <- map(
  .x = list.files(
    "data/nrl/venue_data/",
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
  mutate(location = gsub("\r\n", ", ", location)) |>
  rowwise() |>
  mutate(country = countries[which(countries %in% str_split(location, ", ")[[1]])],
         states = states[which(states %in% str_split(location, ", ")[[1]])],
         cities = cities[which(cities %in% str_split(location, ", ")[[1]])])

write_csv(venue_data_cleaned, 
          "cleaned_data/nrl/venue_data.csv")
##### team_logos #####
team_unique_names <- c("Adelaide Rams",
                       "Annandale",
                       "Balmain Tigers",
                       "Brisbane Broncos",
                       "Canberra Raiders",
                       "Canterbury Bankstown Bulldogs",
                       "Cronulla Sutherland Sharks",
                       "Cumberland",
                       "Dolphins",
                       "Glebe",
                       "Gold Coast Chargers",
                       "Gold Coast Seagulls",
                       "Gold Coast Titans",
                       "Gold Coast/Tweed Heads Giants",
                       "Hunter Mariners",
                       "Illawarra Steelers",
                       "Manly Warringah Sea Eagles",
                       "Melbourne Storm",
                       "New Zealand Warriors",
                       "Newcastle",
                       "Newcastle Knights",
                       "Newtown Jets",
                       "North Queensland Cowboys",
                       "North Sydney Bears",
                       "Northern Eagles",
                       "Parramatta Eels",
                       "Penrith Panthers",
                       "South Queensland Crushers",
                       "South Sydney Rabbitohs",
                       "St George Dragons",
                       "St George Illawarra Dragons",
                       "Sydney Roosters",
                       "University",
                       "Western Reds",
                       "Western Suburbs Magpies",
                       "Wests Tigers"
)

team_colour <- c("#0c1469", # Adelaide Rams
                 "#faa21a", # Annandale
                 "#ed771e", # Balmain Tigers
                 "#fbe051", # Brisbane Broncos
                 "#95c94a", # Canberra Raiders
                 "#02488c", # Canterbury Bankstown Bulldogs
                 "#6ee2e0", # Cronulla Sutherland Sharks
                 "#fffc01", # Cumberland
                 "#f0d0a2", # Dolphins
                 "#8f1836", # Glebe
                 "#039690", # Gold Coast Chargers
                 "#cc0629", # Gold Coast Seagulls
                 "#1e90ff", # Gold Coast Titans
                 "#848484", # Gold Coast/Tweed Heads Giants
                 "#161b6c", # Hunter Mariners
                 "#c30006", # Illawarra Steelers
                 "#6f163d", # Manly Warringah Sea Eagles
                 "#632390", # Melbourne Storm
                 "#05a655", # New Zealand Warriors
                 "#256cb3", # Newcastle
                 "#256cb3", # Newcastle Knights
                 "#014fa2", # Newtown Jets
                 "#002b5c", # North Queensland Cowboys
                 "#fa0001", # North Sydney Bears
                 "#6f163d", # Northern Eagles
                 "#e6b123", # Parramatta Eels
                 "#1c1d1f", # Penrith Panthers
                 "#dd9e4e", # South Queensland Crushers
                 "#074e16", # South Sydney Rabbitohs
                 "#f36c6c", # St George Dragons
                 "#f36c6c", # St George Illawarra Dragons
                 "#cb2022", # Sydney Roosters
                 "#ffdd43", # University
                 "#e51f2e", # Western Reds
                 "#FFFFFF", # Western Suburbs Magpies
                 "#ed771e" # Wests Tigers
)

team_logo_paths <- c("images/logos/Adelaide Rams.png",
                     "images/logos/Annandale.png",
                     "images/logos/Balmain Tigers.png",
                     "images/logos/Brisbane Broncos.png",
                     "images/logos/Canberra Raiders.png",
                     "images/logos/Canterbury Bankstown Bulldogs.png",
                     "images/logos/Cronulla Sutherland Sharks.png",
                     "images/logos/Cumberland.png",
                     "images/logos/Dolphins.png",
                     "images/logos/Glebe.png",
                     "images/logos/Gold Coast Chargers.png",
                     "images/logos/Gold Coast Seagulls.png",
                     "images/logos/Gold Coast Titans.png",
                     "images/logos/Gold Coast Giants.png",
                     "images/logos/Hunter Mariners.png",
                     "images/logos/Illawarra Steelers.png",
                     "images/logos/Manly Warringah Sea Eagles.png",
                     "images/logos/Melbourne Storm.png",
                     "images/logos/New Zealand Warriors.png",
                     "images/logos/Newcastle.png",
                     "images/logos/Newcastle Knights.png",
                     "images/logos/Newtown Jets.png",
                     "images/logos/North Queensland Cowboys.png",
                     "images/logos/North Sydney Bears.png",
                     "images/logos/Northern Eagles.png",
                     "images/logos/Parramatta Eels.png",
                     "images/logos/Penrith Panthers.png",
                     "images/logos/South Queensland Crushers.png",
                     "images/logos/South Sydney Rabbitohs.png",
                     "images/logos/St George Dragons.png",
                     "images/logos/St George Illawarra Dragons.png",
                     "images/logos/Sydney Roosters.png",
                     "images/logos/University.png",
                     "images/logos/Western Reds.png",
                     "images/logos/Western Suburbs Magpies.png",
                     "images/logos/Wests Tigers.png"
)

team_logos <- tibble(
  team_unique = team_unique_names,
  team_colour = team_colour,
  logo_path = team_logo_paths
)

write_csv(team_logos, 
          "cleaned_data/nrl/team_logos.csv")
##### Potentially common mutations #####
# year = year(date)
# year_date = format(date, format = "%d-%m")
# month = month(date)
# day = day(date)
# day_of_week = wday(date, label = TRUE)

