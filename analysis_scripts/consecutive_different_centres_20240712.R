##### Description #####
# An R script to look at pi related stats

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
match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D"),
         result = paste0(result, " (", score_for, "-", score_against, ")")) |>
  left_join(team_data |> select(team_name, team_abbr), by = c("team" = "team_name")) |>
  select(-team) |> rename(team = team_abbr) |> relocate(team, .after = home_away) |>
  select(match_id, competition_year, year, round, home_away, team, score_for, score_against, result)

##### Analysis #####
ctr_data <- player_match_data |>
  left_join(match_data |> select(match_id, competition_year, round, date), by = "match_id") |>
  arrange(date) |>
  filter(position %in% c("C")) |>
  group_by(match_id, team) |>
  mutate(position = ifelse(player_id == player_id[1], "C1", "C2")) |>
  ungroup() |>
  select(match_id, competition_year, round, date, team, opposition_team, player_id, position) |>
  pivot_wider(id_cols = c(match_id, competition_year, round, date, team, opposition_team), 
              names_from = "position", values_from = "player_id") |>
  left_join(player_data |> select(player_id, full_name), by = c("C1" = "player_id")) |>
  rename(C1_player = full_name) |>
  left_join(player_data |> select(player_id, full_name), by = c("C2" = "player_id")) |>
  rename(C2_player = full_name) |>
  left_join(team_data |> select(team_name, team_abbr), by = c("team" = "team_name")) |>
  select(-team) |> rename(team = team_abbr) |> relocate(team, .after = date) |>
  left_join(team_data |> select(team_name, team_abbr), by = c("opposition_team" = "team_name")) |>
  select(-opposition_team) |> rename(opposition_team = team_abbr) |> relocate(opposition_team, .after = team) |>
  rowwise() |>
  mutate(centre_ids = paste0(min(C1, C2), "-", max(C1, C2))) |>
  group_by(team, opposition_team) |>
  mutate(new_centres = (centre_ids != lag(centre_ids)) |> replace_na(TRUE)) |>
  mutate(seq_no = cumsum(!new_centres)) |>
  ungroup() |>
  group_by(team, opposition_team, seq_no) |>
  mutate(seq_length = n(),
         all_unique = (length(unique(centre_ids)) == seq_length)) |>
  ungroup() |>
  #arrange(desc(seq_length), seq_no, date) |>
  #filter(seq_length == max(seq_length)) |>
  #filter(all_unique) |>
  filter(team == "WES" & opposition_team == "CRO")
  
rbind(
  halves_by_game |> rename(player_id = FE, partner_id = HB),
  halves_by_game |> rename(player_id = HB, partner_id = FE)
) |>
  arrange(match_id, team) |>
  group_by(player_id) |>
  summarise(
    n_games = n(),
    n_partners = length(unique(partner_id)),
    .groups = "drop"
  ) |>
  mutate(games_per_partner = n_games / n_partners) |>
  left_join(player_data |> select(player_id, full_name),
            by = "player_id") |>
  filter(n_games > 50) |>
  select(full_name, n_games, n_partners, games_per_partner) |>
  arrange(games_per_partner) |>
  View()

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
final_table <- rbind(
  ctr_data |>
    ungroup() |>
    left_join(match_results |> select(match_id, team, result),
              by = c("match_id", "team")) |>
    mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
           round = gsub("Round (\\d+)", "\\1", round) |> as.numeric(),
           match_summary = paste0(team, " v ", opposition_team)) |>
    select(year, round, match_summary, C1_player, C2_player, result),
  tibble(year = 2024, round = 19, match_summary = "WES v CRO", C1_player = "Adam Doueihi", C2_player = "Solomona Faataape", result = "?")) |>
  mutate(prev = paste0(lag(C1_player), ",", lag(C2_player)) |> str_split(",")) |>
  rowwise() |>
  mutate(C1_player_label = paste0('<span style="display: block; padding: 0 4px; border-radius: 4px; font-weight: bold; color: #1A1A1A; background-color:', ifelse(C1_player %in% prev | "NA" %in% prev, "#FD9B9B", "#9AFF9A"), '">', C1_player, '</span>'),
         C2_player_label = paste0('<span style="display: block; padding: 0 4px; border-radius: 4px; font-weight: bold; color: #1A1A1A; background-color:', ifelse(C2_player %in% prev | "NA" %in% prev, "#FD9B9B", "#9AFF9A"), '">', C2_player, '</span>')) |>
  ungroup() |>
  select(year, round, match_summary, C1_player_label, C2_player_label, result) |>
  #filter(year >= 2015) |>
  rename(
    `Year` = year,
    `Rd` = round,
    `Match` = match_summary,
    `Centre 1` = C1_player_label,
    `Centre 2` = C2_player_label,
    Result = result
  ) |>
  formattable(
    list(
      Result = bold_formatter
    ),
    align = c("r", "l", "c", "c", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 10px;",
                    "<b>Centre of attention:</b> Wests Tigers have not played the same Centre pairing in<br>consecutive matches vs Cronulla since 2016" |> HTML()))
final_table

saveWidget(final_table, "tables/html/wests_centres.html")
webshot(url = "tables/html/wests_centres.html", 
        file = "tables/png/wests_centres.png", 
        selector = "body", zoom = 4,
        vwidth = 750)
