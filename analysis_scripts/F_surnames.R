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
           factor(levels = LETTERS, ordered = TRUE),
         last_name = str_to_upper(last_name),
         full_name = paste(first_name, last_name, sep = " ")) |>
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
most_Fs <- player_match_data |>
  left_join(player_initials, by = "player_id") |>
  filter(last_name_initial == "F") |>
  group_by(match_id, team) |>
  summarise(n_names = n(),
            players = paste0(full_name, collapse = "<br>"),
            .groups = "drop") |>
  left_join(match_data |> select(match_id, competition_year, round)) |>
  mutate(
    year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
    round = gsub("Round (\\d+)", "\\1", round)
  ) |>
  arrange(desc(n_names), desc(year))
most_Fs

match_id_used <- 113599

home_team <- match_data |> filter(match_id == match_id_used) |> pull(home_team)
away_team <- match_data |> filter(match_id == match_id_used) |> pull(away_team)

match_list <- match_id_used |>
  produce_team_lineups() |>
  left_join(player_initials |> select(player_id, full_name, last_name_initial), 
            by = c("player_id") |> setNames(home_team)) |>
  select(-{{home_team}}, -{{away_team}})
match_list

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "4px 4px", 
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
  formatter("span", 
            style = x ~ style(
              font.weight = "bold",
              display = "block"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat"
            ))

num_bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat",
              font.weight = "bold"
            ))

##### Final table 1 formatting #####
final_table <- match_list |>
  mutate(
    full_name = ifelse(last_name_initial == "F", 
                       paste0('<span style="display: block; padding: 2px 4px 2px 4px; border-radius: 4px; font-weight: bold; background-color:', "#1e90ff", '; color:', "#F2F2F2", '">', full_name, '</span>'),
                       full_name)
  ) |>
  select(-last_name_initial) |>
  rename(
    Pos = position,
    Player = full_name
  ) |>
  formattable(
    list(
      Pos = bold_formatter
    ),
    align = c("c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}"))
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "<u>F-WORDS</u><br>Rd3 2025: The Gold Coast Titans set an all-time record for the most surnames beginning with F (7) in a single team" |> HTML()))
final_table

saveWidget(final_table, "tables/html/F_surnames.html")
webshot(url = "tables/html/F_surnames.html", 
        file = "tables/png/F_surnames.png", 
        selector = "div", zoom = 4,
        vwidth = 320)

