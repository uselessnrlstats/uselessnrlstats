##### Description #####
# An R script to look at players who have played an equal number of games for 
# two or more different teams

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
player_ex_clubs <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  left_join(team_data |> select(team_name, team_unique), by = c("team" = "team_name")) |>
  arrange(date) |>
  select(match_id, player_id, team_unique, date) |>
  group_by(player_id) |>
  mutate(ex_teams = team_unique |> accumulate(c) |> map(unique)) |>
  ungroup() |>
  rowwise() |>
  mutate(ex_teams = ex_teams[ex_teams != team_unique] |> list()) |>
  ungroup() |>
  select(-date)

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

##### Analysis #####
player_match_data |>
  left_join(match_data, by = "match_id") |>
  select(match_id, competition_year, round, date, team, player_id) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  filter(round %in% c("Grand Final", "Final", "Grand Final Rep.")) |>
  mutate(round = case_when(
    round == "Grand Final" ~ "GF",
    round == "Final" ~ "GF",
    round == "Grand Final Rep." ~ "GF Rep.",
    .default = NA
  )) |>
  select(match_id, competition_year, year, round, team, player_id) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  left_join(player_ex_clubs |> select(-team_unique), by = c("match_id", "player_id")) |>
  unnest(ex_teams) |>
  rename(ex_team = ex_teams) |>
  group_by(year, ex_team) |>
  summarise(n_players = n(),
            players = paste0(full_name, collapse = ", "),
            .groups = "drop") |>
  arrange(desc(n_players))


ex_tigers <- player_match_data |>
  left_join(match_data, by = "match_id") |>
  select(match_id, competition_year, round, date, team, player_id) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  filter(round %in% c("Grand Final", "Final", "Grand Final Rep.")) |>
  mutate(round = case_when(
    round == "Grand Final" ~ "GF",
    round == "Final" ~ "GF",
    round == "Grand Final Rep." ~ "GF Rep.",
    .default = NA
  )) |>
  select(match_id, competition_year, year, round, team, player_id) |>
  left_join(player_data |> select(player_id, full_name, last_name), by = "player_id") |>
  left_join(player_ex_clubs |> select(-team_unique), by = c("match_id", "player_id")) |>
  unnest(ex_teams) |>
  rename(ex_team = ex_teams) |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  mutate(team_label = paste0('<span style="display: inline; padding: 1px 4px 1px 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', team_abbr, '</span>'),
         player_label = paste0(full_name, " ", team_label)) |>
  group_by(competition_year, year, round, ex_team) |>
  arrange(team, last_name) |>
  summarise(n_players = n(),
            players = paste0(player_label, collapse = "<br>"),
            .groups = "drop") |>
  arrange(year) |>
  filter(ex_team == "Wests Tigers", year >= 2011)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("div", 
            style = x ~ style(
              display = "block",
              vertical.align = "middle",
              padding = "2px 4px 2px 4px", 
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
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("div", 
            style = x ~ style(
              font.weight = "bold"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "14px",
              font.family = "Montserrat"
            ))

# Final table formatting
final_table <- ex_tigers |>
  mutate(match_summary = paste0(year, " ", round)) |>
  select(match_summary, ex_team, n_players, players) |>
  rbind(tibble(match_summary = "2024 GF", ex_team = "Wests Tigers", n_players = 4, players = "Shawn Blore <span style=\"display: inline; padding: 1px 4px 1px 4px; border-radius: 4px; font-weight: bold; background-color:#632390; color:#F2F2F2\">MEL</span><br>Harry Grant <span style=\"display: inline; padding: 1px 4px 1px 4px; border-radius: 4px; font-weight: bold; background-color:#632390; color:#F2F2F2\">MEL</span><br>Matt Eisenhuth <span style=\"display: inline; padding: 1px 4px 1px 4px; border-radius: 4px; font-weight: bold; background-color:#1c1d1f; color:#F2F2F2\">PEN</span><br>Luke Garner <span style=\"display: inline; padding: 1px 4px 1px 4px; border-radius: 4px; font-weight: bold; background-color:#1c1d1f; color:#F2F2F2\">PEN</span>")) |>
  mutate(players = paste0("<b>", players, "</b>")) |>
  rename(
    `Match` = match_summary,
    `Ex Club` = ex_team,
    `#` = n_players,
    `Ex Players` = players
  ) |>
  formattable(
    list(
      `Ex Club` = team_formatter,
      `#` = num_formatter
    ),
    align = c("c", "c", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "Wests Tigers are the only club with an ex-player in every NRL GF since 2012"))
final_table

saveWidget(final_table, "tables/html/ex_tigers_gf.html")
webshot(url = "tables/html/ex_tigers_gf.html", 
        file = "tables/png/ex_tigers_gf.png", 
        selector = "div", zoom = 4,
        vwidth = 410)
 
