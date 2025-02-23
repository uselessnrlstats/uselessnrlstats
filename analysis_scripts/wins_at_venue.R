##### Description #####
# An R script to look at players with only wins/losses at a single venue

##### Libraries # ####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(extrafont)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
venue_data <- read_csv("cleaned_data/nrl/venue_data.csv")

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
           .default = "D")) |>
  select(match_id, competition_year, year, round, home_away, team, score_for, score_against, result)

player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(debut = min(date),
            first_year = min(year),
            last_year = max(year),
            final_team = team[max(row_number())]) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

##### Analysis #####
undefeated <- player_match_data |>
  select(player_id, match_id, team, captain) |>
  left_join(match_data |> select(match_id, competition, venue_id), by = "match_id") |>
  left_join(match_results |> select(match_id, team, result), 
            by = c("match_id", "team")) |>
  #filter(competition == "NRL") |>
  group_by(player_id, venue_id) |>
  summarise(n_wins = sum(result == "W"),
            n_draws = sum(result == "D"),
            n_losses = sum(result == "L"),
            n_matches = n(),
            .groups = "drop") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  left_join(venue_data |> select(venue_id, venue_name, `non-commercial_name`),
            by = "venue_id") |>
  left_join(player_career_years, by = "player_id") |>
  arrange(desc(n_matches), first_year, last_year) |>
  select(career_years, first_year, last_year, full_name, venue_name, `non-commercial_name`, n_matches, n_wins, n_draws, n_losses) |>
  filter(n_losses == 0)

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
final_table <- undefeated |>
  filter(first_year > 1997) |>
  mutate(`non-commercial_name` = ifelse(`non-commercial_name` == "Brisbane Stadium (Lang Park)", "Lang Park", `non-commercial_name`),
         venue_name = ifelse(venue_name == `non-commercial_name`,
                             venue_name,
                             paste0(venue_name, " (", `non-commercial_name`, ")"))) |>
  rowwise() |>
  mutate(record = paste0(c(n_wins, n_draws, n_losses), collapse = "-"),
         full_name = ifelse(grepl("2024", career_years), paste0(full_name, "\u2217"), full_name)) |>
  ungroup() |>
  select(n_matches, career_years, full_name, venue_name, record) |>
  filter(n_matches > 7) |>
  group_by(n_matches) |>
  mutate(n_matches = ifelse(row_number() %in% c(mean(row_number()), mean(row_number()) - 0.5),
                            n_matches, "")) |>
  ungroup() |>
  rbind(tibble(
    n_matches = c("7", "6"),
    career_years = "",
    full_name = paste0("+", c(undefeated |> filter(first_year > 1997, n_matches == 7) |> nrow(), undefeated |> filter(first_year > 1997, n_matches == 6) |> nrow())),
    venue_name = "",
    record = ""
  )) |>
  rename(
    `# Matches` = n_matches,
    Career = career_years,
    Player = full_name,
    Venue = venue_name,
    `W-D-L` = record
  ) |>
  formattable(
    list(
      `# Matches` = bold_formatter,
      Player = bold_formatter,
      Venue = bold_formatter
    ),
    align = c("c", "c", "l", "l", "c"),
    table.attr = 'class=\"table table-hoverable\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-child(4), tr:nth-child(9), tr:nth-child(18), tr:nth-child(19), td {border-top: 2px solid #120b2f;}")) |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Montserrat; margin-left: 5px;",
                    "<b>NRL era (1998-2024): Most matches at a venue without a loss</b> (\u2217active)" |> HTML()))
final_table

saveWidget(final_table, "tables/html/undefeated_venues.html")
webshot(url = "tables/html/undefeated_venues.html", 
        file = "tables/png/undefeated_venues.png", 
        selector = "div", zoom = 4,
        vwidth = 675)

# Final table 2 formatting
final_table2 <- undefeated |>
  mutate(`non-commercial_name` = ifelse(`non-commercial_name` == "Brisbane Stadium (Lang Park)", "Lang Park", `non-commercial_name`),
         venue_name = ifelse(venue_name == `non-commercial_name`,
                             venue_name,
                             paste0(venue_name, " (", `non-commercial_name`, ")"))) |>
  rowwise() |>
  mutate(record = paste0(c(n_wins, n_draws, n_losses), collapse = "-")) |>
  ungroup() |>
  select(n_matches, career_years, full_name, venue_name, record) |>
  filter(n_matches > 10) |>
  group_by(n_matches) |>
  mutate(n_matches = ifelse(row_number() %in% c(mean(row_number()), mean(row_number()) - 0.5),
                            as.character(n_matches), "")) |>
  ungroup() |>
  rbind(tibble(
    n_matches = c("10", "9"),
    career_years = "",
    full_name = paste0("+", c(undefeated |> filter(n_matches == 10) |> nrow(), undefeated |> filter(n_matches == 9) |> nrow())),
    venue_name = "",
    record = ""
  )) |>
  rename(
    `# Matches` = n_matches,
    Career = career_years,
    Player = full_name,
    Venue = venue_name,
    `W-D-L` = record
  ) |>
  formattable(
    list(
      `# Matches` = bold_formatter,
      Player = bold_formatter,
      Venue = bold_formatter
    ),
    align = c("c", "c", "l", "l", "c"),
    table.attr = 'class=\"table table-hoverable\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #ffffff;}")) |>
  prependContent(tags$style(paste0("table ",
                                   paste0("tr:nth-child(", undefeated |> filter(n_matches > 10) |> mutate(x = (n_matches != lag(n_matches))) |> pull(x) |> which(), ")") |> paste(collapse = ", "),
                                   ", tr:nth-child(15), tr:nth-child(16), td {border-top: 2px solid #120b2f;}"))) |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Montserrat; margin-left: 5px;",
                    "<b>All time (1908-): Most matches at a venue without a loss</b> (\u2217active)" |> HTML()))
final_table2

saveWidget(final_table2, "tables/html/undefeated_venues2.html")
webshot(url = "tables/html/undefeated_venues2.html", 
        file = "tables/png/undefeated_venues2.png", 
        selector = "div", zoom = 4,
        vwidth = 675)
