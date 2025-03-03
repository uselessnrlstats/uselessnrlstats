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
  library(showtext)
}

# TEAMS TO WATCH
# F - Titans
# K - Bulldogs
# M - Rabbitohs
# S - Raiders
# W - Roosters ?

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
player_initials <- player_data |>
  filter(first_name != "?") |>
  mutate(last_name_initial = substr(last_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE)) |>
  select(player_id, full_name, last_name_initial)


# Produce Team Lineups
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
  
##### Analysis #####
player_match_data |>
  left_join(player_initials, by = "player_id") |>
  filter(last_name_initial == "S") |>
  group_by(match_id, team) |>
  summarise(n_names = n(),
            players = list(paste0(full_name, collapse = ", ")),
            .groups = "drop") |>
  left_join(match_data |> select(match_id, competition_year, round)) |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "\\1", round)
  ) |>
  arrange(desc(n_names), desc(year)) |>
  View()

match_ids <- c(11822, 12144, 17785)

match_id_used <- match_ids[2]

home_team <- match_data |> filter(match_id == match_id_used) |> pull(home_team)
away_team <- match_data |> filter(match_id == match_id_used) |> pull(away_team)

match_list <- match_id_used |>
  produce_team_lineups() |>
  left_join(player_data |> select(player_id, full_name), 
            by = c("player_id") |> setNames(home_team)) |>
  select(-{{home_team}}) |> rename(!!home_team := full_name) |> 
  relocate(!!home_team, .before = position) |>
  left_join(player_data |> select(player_id, full_name),
            by = c("player_id") |> setNames(away_team)) |>
  select(-{{away_team}}) |> rename(!!away_team := full_name)
match_list

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

fixed_width_formatter <- 
  formatter(.tag = "html",
            style = style(
              width = "150px"
            ))

# Final table formatting
final_table <- match_list |>
  mutate(
    {{home_team}} := gsub("((b))", "<b>\\1</b>", !!sym(home_team), ignore.case = TRUE),
    {{away_team}} := gsub("((b))", "<b>\\1</b>", !!sym(away_team), ignore.case = TRUE)
  ) |>
  rename(
    vs = position
  ) |>
  formattable(
    list(
      vs = bold_formatter
    ),
    col.names = team_formatter(c(home_team, "vs", away_team)),
    align = c("r", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table td:nth-of-type(2n-1) { width: 160px;}")) |>
  prependContent(tags$style("table tr:nth-of-type(13) td {border-bottom: 2px solid #7f7f7f;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 10px; font-size: 16px",
                    "NSWRFL 1964 BF: Balmain vs Parramatta"))

final_table

saveWidget(final_table, "tables/html/world_bee_day1.html")
webshot(url = "tables/html/world_bee_day1.html", 
        file = "tables/png/world_bee_day1.png", 
        selector = "body", zoom = 4,
        vwidth = 450)
