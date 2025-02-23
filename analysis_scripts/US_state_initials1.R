##### Description #####
# An R script to look at players with US state initials

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

match_list <- player_match_data |>
  filter(match_id == match_id_used) |>
  left_join(player_initials, by = "player_id") |>
  left_join(US_states, by = c("initials" = "abbr")) |>
  left_join(match_data, by = "match_id") |>
  mutate(year = year(date),
         round = gsub("Round ", "Rd", round)) |>
  select(year, round, team, position, full_name, initials, state) |>
  filter(!is.na(state)) |>
  arrange(year, team)

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
final_table <- match_list |>
  mutate(match = paste(year, round)) |>
  relocate(match, .before = team) |>
  select(-c(year, round)) |>
  rename(
    Match = match,
    Team = team,
    Position = position,
    Player = full_name,
    Initials = initials,
    `US State` = state
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Player = bold_formatter,
      `US State` = bold_formatter
    ),
    align = c("r", "r", "c", "l", "c", "l"),
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

saveWidget(final_table, "tables/html/US_state_initials_small2.html")
webshot(url = "tables/html/US_state_initials_small2.html", 
        file = "tables/png/US_state_initials_small2.png", 
        selector = "body", zoom = 4,
        vwidth = 600)
