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

##### Analysis #####
player_match_data |>
  left_join(match_data |> select(match_id, competition_year, round, date), by = "match_id") |>
  arrange(date) |>
  filter(position %in% c("HB", "FE")) |>
  select(match_id, competition_year, round, date, team, opposition_team, player_id, position) |>
  pivot_wider(id_cols = c(match_id, competition_year, round, date, team, opposition_team), 
              names_from = "position", values_from = "player_id") |>
  left_join(player_data |> select(player_id, full_name), by = c("FE" = "player_id")) |>
  rename(FE_player = full_name) |>
  left_join(player_data |> select(player_id, full_name), by = c("HB" = "player_id")) |>
  rename(HB_player = full_name) |>
  left_join(team_data |> select(team_name, team_unique), by = c("team" = "team_name")) |>
  select(-team) |> rename(team = team_unique) |> relocate(team, .after = date) |>
  left_join(team_data |> select(team_name, team_unique), by = c("opposition_team" = "team_name")) |>
  select(-opposition_team) |> rename(opposition_team = team_unique) |> relocate(opposition_team, .after = team) |>
  rowwise() |>
  mutate(halves_ids = paste0(min(FE, HB), "-", max(FE, HB))) |>
  group_by(team) |>
  mutate(new_halves = (halves_ids != lag(halves_ids)) |> replace_na(TRUE)) |>
  mutate(seq_no = cumsum(!new_halves)) |>
  ungroup() |>
  group_by(team, seq_no) |>
  mutate(seq_length = n(),
         all_unique = (length(unique(halves_ids)) == seq_length)) |>
  ungroup() |>
  arrange(desc(seq_length), seq_no, date) |>
  #filter(seq_length == max(seq_length)) |>
  filter(all_unique) |>
  #filter(team == "Newcastle Knights" & opposition_team == "Cronulla Sutherland Sharks") |>
  View()
  
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
