##### Description #####
# An R script to look at team lists with the most 'V' names

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

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

player_data |>
  filter(full_name != paste(first_name, last_name, sep = " ")) |>
  View()

##### Analysis #####
V_names <- player_data |>
  filter(first_name != "?",
         !is.na(first_name)) |>
  select(player_id, full_name, first_name, last_name) |>
  mutate(first_name_initial = substr(first_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE),
         last_name_initial = substr(last_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE)) |>
  mutate(any_Vs = (first_name_initial == "V") + (last_name_initial == "V")) |>
  select(player_id, full_name, any_Vs)

top_v_matches <- player_match_data |>
  #filter(position != "B") |>
  left_join(V_names, by = "player_id") |>
  filter(any_Vs > 0) |>
  group_by(match_id, team) |> 
  summarise(total_Vs = sum(any_Vs),
            players = paste(sort(full_name), collapse = ", "),
            .groups = "drop") |>
  left_join(match_data |> select(match_id, competition, competition_year, round, date), by = "match_id") |>
  arrange(date) |>
  mutate(round = gsub("Round (\\d+)", "\\1", round),
         year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  group_by(team, total_Vs, players, competition) |>
  summarise(times = n(), 
            rounds = paste(round, collapse = ","),
            years = paste(sort(unique(year)), collapse = ","),
            .groups = "drop") |>
  mutate(competition_year = paste(competition, years)) |>
  arrange(desc(total_Vs), years) |>
  select(competition_year, times, team, players, total_Vs)

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
final_table <- top_v_matches |>
  filter(total_Vs >= 3, players != "V Beaumont, Vic Hill") |>
  mutate(players = gsub("V", "<b><u>V</u></b>", players)) |>
  rename(
    Year = competition_year,
    Times = times,
    Team = team,
    Players = players,
    `#V Names` = `total_Vs`
  ) |>
  formattable(
    list(
      Team = team_formatter,
      `#V Names` = bold_formatter,
      Times = bold_formatter
    ),
    align = c("r", "c", "c", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h5(class = "subtitle",
                    style = "font-weight: bold; font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL teams with the most 'V' names in the lineup"))
final_table

saveWidget(final_table, "tables/html/v_names.html")
webshot(url = "tables/html/v_names.html", 
        file = "tables/png/v_names.png", 
        selector = "body", zoom = 4,
        vwidth = 750)
