##### Description #####
# An R script to look at players who kicked at 100% on debut

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot)
  library(scales)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
most_goalkickers <- player_match_data |>
  select(player_id, match_id, team, opposition_team, goals, goal_attempts) |>
  left_join(player_data |> select(player_id, full_name, last_name), by = "player_id") |>
  filter(goal_attempts > 0 | goals > 0) |>
  mutate(goal_label = ifelse(is.na(goal_attempts), goals, paste0(goals, "/", goal_attempts))) |>
  mutate(player_summary = paste0("<b>", full_name, "</b> (", goal_label, ")")) |>
  group_by(match_id, team, opposition_team) |>
  arrange(desc(goals), last_name) |>
  summarise(
    n_goals = sum(goals),
    n_goalkickers = n(),
    goalkickers = paste0(player_summary, collapse = "<br>"),
    .groups = "drop"
  ) |>
  left_join(match_data |> select(match_id, competition_year, round, date), by = "match_id") |>
  filter(!(grepl("Round", round))) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = case_when(
           round %in% c("Prelim Final", "Prelim Playoff", "Major Prelim Semi", "Minor Prelim Semi", "Minor Prelim", "Major Prelim") ~ "PF",
           round %in% c("Semi Final", "Minor Semi", "Major Semi", "Minor Semi Rep.") ~ "SF",
           round %in% c("Qualif Final", "Qualifier", "Minor Qualif", "Major Qualif") ~ "QF",
           round %in% c("Elim Qualif", "Elim Final", "Elim Prelim Final") ~ "EF",
           round %in% c("Grand Final", "Final", "Grand Final Rep.") ~ "GF",
           round %in% c("Grand Final Chall.") ~ "GFC",
           round %in% c("Playoff") ~ "PO",
           .default = round
         )) |>
  arrange(desc(n_goalkickers), date) |>
  mutate(match = paste(year, round)) |>
  select(year, round, team, opposition_team, goalkickers, n_goalkickers)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("div", 
            style = x ~ style(
              display = "block",
              vertical.align = "middle",
              padding = "3px 4px 3px 4px", 
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
              display = "block", 
              padding = "2px 4px 2px 4px"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "14px",
              font.family = "Montserrat"
            ))

# Final table formatting
final_table <- most_goalkickers |>
  filter(n_goalkickers >= 3) |>
  # rbind(tibble(
  #   year = "",
  #   round = "",
  #   team = paste0("\u00D7", most_goalkickers |> filter(n_goalkickers == 2) |> nrow()),
  #   opposition_team = "",
  #   goalkickers = "",
  #   n_goalkickers = 2
  # )) |>
  # select(-opposition_team) |>
  rename(
    Yr = year,
    Final = round,
    Team = team,
    Opposition = opposition_team,
    `Goalkickers (# Goals)` = goalkickers,
    `#` = n_goalkickers
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Opposition = team_formatter,
      `#` = num_formatter
    ),
    align = c("c", "c", "r", "l", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(4) td {background-color: #ffd6f0;}")) |>
  prependContent(tags$style("table td:nth-of-type(5) { width: 155px;}")) |>
  prependContent(tags$style("table td:nth-of-type(6) { width: 50px;}")) |>
  # prependContent(tags$style("table {height: 1px;}")) |>
  # prependContent(tags$style("table tr:nth-of-type(n) td div {height: 100%;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "Most (successful) goalkickers in a finals match (by team)"))
final_table

saveWidget(final_table, "tables/html/most_goalkickers_finals.html")
webshot(url = "tables/html/most_goalkickers_finals.html", 
        file = "tables/png/most_goalkickers_finals.png", 
        selector = "div", zoom = 4,
        vwidth = 600)
