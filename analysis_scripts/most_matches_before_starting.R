##### Description #####
# An R script to look at players who played in all 4 spine positions in a season

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
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv") |>
  mutate(position = case_when(
    position == "FB" ~ "Fullback",
    position == "W" ~ "Wing",
    position == "C" ~ "Centre",
    position == "FE" ~ "5/8th",
    position == "HB" ~ "Halfback",
    position == "FR" ~ "Prop",
    position == "HK" ~ "Hooker",
    position == "2R" ~ "2nd Row",
    position == "L" ~ "Lock",
    position == "B" ~ "Bench"
  ))
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year)) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

##### Analysis #####
most_before_starting <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  arrange(date) |>
  group_by(player_id) |>
  filter(position[1] == "Bench") |>
  summarise(first_start = ifelse(any(position != "Bench"),
                                 min(which(position != "Bench")),
                                 n()),
            match_id = ifelse(any(position != "Bench"),
                              match_id[min(which(position != "Bench"))],
                              NA),
            n_positions = length(unique(position)),
            first_position = ifelse(any(position != "Bench"),
                                      position[first_start],
                                      NA),
            n_matches = n(),
            team = team[first_start],
            .groups = "drop") |>
  left_join(player_data |> select(player_id, full_name), 
            by = "player_id") |>
  #filter(n_positions > 1) |>
  left_join(match_data |> select(match_id, competition_year, round, date), by = "match_id") |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round),
         match = ifelse(is.na(first_position), "DNS", paste0(year, " ", round)),
         first_position = replace_na(first_position, "-")) |>
  select(player_id, first_start, full_name, match, team, first_position, date) |>
  mutate(match = ifelse(player_id == "31629", "2024 Rd26", match),
         first_start = ifelse(player_id == "31629", first_start + 1, first_start),
         first_position = ifelse(player_id == "31629", "Lock", first_position),
         date = ifelse(player_id == "31629", as_date("2024-08-29"), date) |> as_date()) |>
  arrange(desc(first_start), date) |>
  select(-player_id, -date) |>
  mutate(first_start = ifelse(match == "DNS", paste0(first_start, "*"), first_start)) |>
  filter(match != "DNS")
  
##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `vertical.align` = "center",
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
              font.weight = "bold"
            ))

number_formatter <-
  formatter("span",
            style = x ~ style(
              font.size = "12px"
            ))

# Final table formatting
final_table <- most_before_starting |>
  filter(row_number() <= 9) |>
  rename(
    Player = full_name,
    `First Start` = match,
    `Team` = team,
    `First starting<br>position` = first_position,
    `Match # of<br>First Start` = first_start
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Player = bold_formatter,
      `Match # of<br>First Start` = bold_formatter
    ),
    align = c("c", "c", "l", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 14px; font-family: Helvetica; vertical-align: middle"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {font-size: 14px; background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(1) td {background-color: #ffd6f0;}")) |>
  #prependContent(tags$style("table td:nth-child(1), td:nth-child(3) { width: 220px;}")) |>
  prependContent(h5(class = "title",
                    style = "font-family: Montserrat; font-weight: bold; margin-left: 5px;",
                    "Most NSWRL/NRL matches until first match in the starting 13"))
final_table

saveWidget(final_table, "tables/html/most_before_starting.html")
webshot(url = "tables/html/most_before_starting.html", 
        file = "tables/png/most_before_starting.png", 
        selector = "div", zoom = 4,
        vwidth = 750)
