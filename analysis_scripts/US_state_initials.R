##### Description #####
# An R script to look at plyers with US state initials

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
  library(rlang)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

US_states <- data.frame(state = state.name, abbr = state.abb)

##### Helper Stats #####
player_initials <- player_data |>
  filter(first_name != "?") |>
  mutate(first_name_initial = substr(first_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = rev(LETTERS), ordered = TRUE),
         last_name_initial = substr(last_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE),
         initials = paste0(first_name_initial, last_name_initial)) |>
  select(player_id, full_name, initials)

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
top_state_matches <- player_match_data |>
  left_join(player_initials, by = "player_id") |>
  mutate(US_state = initials %in% state.abb) |>
  group_by(match_id) |>
  summarise(US_state_count = sum(US_state)) |>
  arrange(desc(US_state_count)) |>
  filter(US_state_count == max(US_state_count)) |>
  pull(match_id)

match_id_used <- top_state_matches[2]

home_team <- match_data |> filter(match_id == match_id_used) |> pull(home_team)
away_team <- match_data |> filter(match_id == match_id_used) |> pull(away_team)
  
match_list <- match_id_used |>
  produce_team_lineups() |>
  left_join(player_initials, by = c("player_id") |> setNames(home_team)) |>
  select(-{{home_team}}) |> rename(!!home_team := full_name) |> 
  relocate(!!home_team, .before = position) |>
  left_join(player_initials, by = c("player_id") |> setNames(away_team)) |>
  select(-{{away_team}}) |> rename(!!away_team := full_name) |>
  relocate(initials.x, .before = !!home_team) |>
  left_join(US_states, by = c("initials.x" = "abbr")) |>
  left_join(US_states, by = c("initials.y" = "abbr")) |>
  relocate(state.x, .before = initials.x)

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
match_list_table <- match_list |>
  mutate(state.x = ifelse(is.na(state.x), initials.x, paste0(state.x, " - <b>", initials.x, "</b>")),
         state.y = ifelse(is.na(state.y), initials.y, paste0("<b>", initials.y, "</b> - ", state.y))) |>
  select(-c(initials.x, initials.y)) |>
  rename(
    `State - Initials` = state.x,
    vs = position,
    `Initials - State` = state.y
  ) 

final_table <- match_list_table |>
  formattable(
    list(
      vs = bold_formatter
    ),
    col.names = team_formatter(colnames(match_list_table)),
    align = c("r", "r", "c", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "American State Initials")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL 2022 Rd6: NRL/NSWRL match with the most players (10) whose initials are also US state abbreviations"))
final_table

saveWidget(final_table, "tables/html/US_state_initials2.html")
webshot(url = "tables/html/US_state_initials2.html", 
        file = "tables/png/US_state_initials2.png", 
        selector = "body", zoom = 4,
        vwidth = 600)
