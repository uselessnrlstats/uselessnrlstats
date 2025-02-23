##### Description #####
# An R script to look at rounds where teams had a points diff of 0

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(scales)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
days_between <- player_match_data |>
  select(player_id, match_id, team, opposition_team, tries) |>
  filter(tries > 0) |>
  left_join(match_data |> select(match_id, competition_year, round, date), 
            by = "match_id") |>
  group_by(player_id) |>
  arrange(player_id, date) |>
  mutate(try_number = cumsum(tries)) |>
  mutate(day_gap = ifelse(row_number() == 1,
                          NA,
                          difftime(date, lag(date), units = "days"))) |>
  mutate(prev_team = lag(team),
         prev_cy = lag(competition_year), 
         prev_ot = lag(opposition_team),
         prev_rd = lag(round),
         prev_date = lag(date)) |>
  ungroup() |>
  filter(!is.na(day_gap)) |>
  rowwise() |>
  mutate(across(c(team, prev_team, opposition_team, prev_ot),
                .f = \(x) team_data$team_short[which(x == team_data$team_name)])) |>
  filter(year(date) == 2024) |>
  left_join(player_data |> select("player_id", "full_name"), by = "player_id") |>
  select(full_name, try_number, day_gap, prev_team, prev_rd, prev_date, prev_ot, team, round, date, opposition_team) |>
  arrange(desc(day_gap))
  
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
final_table <- days_between |>
  mutate(across(c(round, prev_rd), \(x) gsub("Round (\\d+)", "\\1", x))) |>
  rowwise() |>
  mutate(across(c(round, prev_rd),
                \(x) ifelse(grepl("[A-z]", x), 
                            paste(str_extract_all(pattern = "[A-Z]", x) |> unlist(), collapse = ""),
                            paste("Rd", x, sep = "")))) |>
  ungroup() |>
  mutate(
    prev_game = paste(prev_rd, prev_team, "v", prev_ot),
    try_game = paste(round, team, "v", opposition_team),
    across(c(prev_date, date), \(x) format(x, "%d/%m/%Y"))) |>
  select(full_name, try_number, prev_date, prev_game, date, try_game, day_gap) |>
  filter(day_gap > 500) |>
  rename(
    Player = full_name,
    `Try #` = try_number,
    `Prev. Date` = prev_date,
    `Previous Match` = prev_game,
    `Date` = date,
    `Match` = try_game,
    `Day Gap` = day_gap
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `Day Gap` = bold_formatter
    ),
    align = c("r", "c", "c", "l", "c", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h5(class = "subtitle",
                    style = "font-weight: bold; font-family: Roboto Regular; margin-left: 25px;",
                    "Longest gaps in days between NRL/NSWRL tries by players that have already reached 100 tries"))
final_table

saveWidget(final_table, "tables/html/days_between_tries100.html")
webshot(url = "tables/html/days_between_tries100.html", 
        file = "tables/png/days_between_tries100.png", 
        selector = "body", zoom = 4,
        vwidth = 800)
