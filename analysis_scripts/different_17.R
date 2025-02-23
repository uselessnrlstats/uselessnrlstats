##### Description #####
# An R script to look at players who debuted in the same game

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
match_team_players <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  left_join(team_data |> select(team_name, team_unique),
            by = c("opposition_team" = "team_name")) |>
  rename(team_unique_opp = team_unique) |>
  left_join(team_data |> select(team_name, team_unique),
            by = c("team" = "team_name")) |>
  select(match_id, player_id, team_unique, team_unique_opp) |>
  group_by(match_id, team_unique, team_unique_opp) |>
  summarise(
    n_players = length(unique(player_id)),
    player_ids = list(unique(player_id)),
    .groups = "drop"
  )

##### Analysis #####
match_summaries <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, round, date, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  left_join(team_data |> select(team_name, team_unique),
            by = c("team" = "team_name")) |>
  mutate(home_away = toupper(substr(home_away, 1, 1))) |>
  select(match_id, competition_year, year, round, date, home_away, team_unique) |>
  arrange(date) |>
  left_join(match_team_players, by = c("match_id", "team_unique")) |>
  group_by(team_unique) |>
  mutate(match_no = row_number()) |>
  ungroup() |>
  filter(!is.na(n_players))
  
match_change <- match_summaries |>
  split(f = match_summaries$team_unique) |>
  map(.f = function(df) {
    print(unique(df$team_unique))
    map(.x = 1:nrow(df),
        .f = function(x) {
          match_players <- df$player_ids[[x]]
          match_no_x <- df$match_no[[x]]
          different_teams <- (df |> 
            filter(match_no > match_no_x) |>
            rowwise() |>
            mutate(check = any(match_players %in% player_ids)) |>
            filter(!check))[1,]
          return(
            df[x,] |> 
              mutate(match_id2 = different_teams$match_id,
                     date2 = different_teams$date,
                     n_players2 = different_teams$n_players,
                     player_ids2 = different_teams$player_ids,
                     match_no2 = different_teams$match_no))
        },
        .progress = TRUE) |>
      list_rbind()
  },
  .progress = TRUE) |>
  list_rbind()

match_change |>
  filter(!is.na(match_no2)) |>
  mutate(gap_games = match_no2 - match_no,
         gap_days = date2 - date) |>
  arrange(gap_games)

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
final_table <- complete_data |>
  filter(n_matches >= 100) |>
  mutate(round = gsub("Round (\\d+)", "\\1", round) |> as.numeric(),
         year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         n_matches = paste0(n_matches, " (", n_diff_team, ")"),
         debut_match = paste0(year, " Rd", round, " (", teams, ")")) |>
  select(debut_match, full_name1, full_name2, n_matches) |>
  rename(
    `Debut Match` = debut_match,
    `Player 1` = full_name1,
    `Player 2` = full_name2,
    `# Matches <br> (Diff. Teams)` = n_matches
  ) |>
  formattable(
    list(
      `Player 1` = bold_formatter,
      `Player 2` = bold_formatter
    ),
    align = c("l", "c", "c", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table tr:nth-of-type(10n) td {background-color: #FFB90555;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Together from the Start")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NSWRL/NRL players with the most matches played together after debuting in the same match"))
final_table

saveWidget(final_table, "tables/html/debut_games_together.html")
webshot(url = "tables/html/debut_games_together.html", 
        file = "tables/png/debut_games_together.png", 
        selector = "body", zoom = 4,
        vwidth = 525)
