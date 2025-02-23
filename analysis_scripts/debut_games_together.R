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
  library(webshot)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
player_debut_matches <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  group_by(player_id) |>
  arrange(date) |>
  filter(row_number() == 1) |>
  ungroup() |>
  group_by(match_id, date) |>
  summarise(debut_players = list(unique(player_id)),
            .groups = "drop") |>
  rowwise() |>
  filter(length(debut_players) > 1) |>
  arrange(date)

player_career_matches <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  group_by(player_id) |>
  arrange(date) |>
  summarise(match_ids = list(match_id),
            teams = list(team),
            .groups = "drop")

match_summary <- match_data |>
  select(match_id, competition_year, round, date, home_team, away_team) |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("home_team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  select(-c(home_team, team_unique)) |> 
  rename(home_abbr = team_abbr, home_colour = team_colour) |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("away_team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  select(-c(away_team, team_unique)) |>
  rename(away_abbr = team_abbr, away_colour = team_colour) |>
  mutate(
    home_team_abbr = paste0('<span style="display: inline; padding: 2px 4px 2px 4px; border-radius: 4px; font-weight: bold; background-color:', home_colour, '; color:', ifelse(luminance(home_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', home_abbr, '</span>'),
    away_team_abbr = paste0('<span style="display: inline; padding: 2px 4px 2px 4px; border-radius: 4px; font-weight: bold; background-color:', away_colour, '; color:', ifelse(luminance(away_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', away_abbr, '</span>'),
  ) |>
  mutate(teams = paste(home_team_abbr, "v", away_team_abbr)) |>
  select(match_id, competition_year, round, date, teams)

##### Analysis #####
pw_player_debuts <- player_debut_matches |>
  split(f = player_debut_matches$match_id) |>
  map(.f = function(df) {
    players <- df$debut_players[[1]]
    pw_players <- combn(players, 2) |> 
      t() |>
      as.data.frame() |>
      setNames(c("player_id1", "player_id2"))
    return(pw_players)
  }) |>
  list_rbind(names_to = "match_id") |>
  mutate(match_id = as.numeric(match_id)) |>
  tibble()

pw_shared_matches <- pw_player_debuts |>
  split(f = 1:nrow(pw_player_debuts)) |>
  map(.f = function(df) {
    all_matches <- player_career_matches |> 
      filter(player_id %in% c(df$player_id1, df$player_id2)) |>
      pull(match_ids) |>
      unlist() |>
      table()
    
    shared_matches <- all_matches[which(all_matches == 2)] |> names()
    
    teams1 <- player_career_matches |> 
      filter(player_id == df$player_id1) |>
      rowwise() |>
      mutate(match_indices = which(match_ids %in% shared_matches) |> list(),
             match_teams = teams[match_indices] |> list()) |>
      pull(match_teams) |>
      unlist()
    
    teams2 <- player_career_matches |> 
      filter(player_id == df$player_id2) |>
      rowwise() |>
      mutate(match_indices = which(match_ids %in% shared_matches) |> list(),
             match_teams = teams[match_indices] |> list()) |>
      pull(match_teams) |>
      unlist()
    
    final_df <- df |>
      mutate(n_matches = sum(all_matches == 2),
             n_same_team = sum(teams1 == teams2),
             n_diff_team = n_matches - n_same_team,
             match_ids = list(shared_matches))
    return(final_df)
  }) |>
  list_rbind()

complete_data <- pw_shared_matches |>
  left_join(player_data |> select(player_id, full_name), by = c("player_id1" = "player_id")) |>
  rename(full_name1 = full_name) |>
  left_join(player_data |> select(player_id, full_name), by = c("player_id2" = "player_id")) |>
  rename(full_name2 = full_name) |>
  left_join(match_summary, by = "match_id") |>
  select(competition_year, round, date, teams, match_id, player_id1, full_name1, player_id2, full_name2, n_matches, n_same_team, n_diff_team, match_ids) |>
  arrange(desc(n_matches)) 

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
  filter(row_number() <= 10) |>
  mutate(round = gsub("Round (\\d+)", "\\1", round),
         year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         n_matches = paste0("<b>", n_matches, "</b> (", n_diff_team, ")"),
         debut_match = paste0(year, " Rd", round, " ", teams)) |>
  select(debut_match, full_name1, full_name2, n_matches) |>
  rename(
    `Debut Match` = debut_match,
    `Player 1` = full_name1,
    `Player 2` = full_name2,
    `# Matches<br>(Diff. Teams)` = n_matches
  ) |>
  formattable(
    list(
      `Player 1` = bold_formatter,
      `Player 2` = bold_formatter
    ),
    align = c("l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(8) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 18px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "NSWRL/NRL duos with the most matches played together after debuting in the same match"))
final_table

saveWidget(final_table, "tables/html/debut_games_together2.html")
webshot(url = "tables/html/debut_games_together2.html", 
        file = "tables/png/debut_games_together2.png", 
        selector = "div", zoom = 4,
        vwidth = 560)
