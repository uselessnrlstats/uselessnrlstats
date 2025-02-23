##### Description #####
# An R script to look at players who kicked increasing numbers of goals

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
longest_tryless_streaks <- player_match_data |>
  mutate(tries = tries + penalty_tries) |>
  left_join(match_data, by = "match_id") |>
  select(match_id, competition_year, round, date, team, opposition_team, player_id, tries) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round)) |>
  rowwise() |>
  mutate(match_summary = paste0(year, " ", round)) |>
  ungroup() |>
  arrange(player_id, date) |>
  group_by(player_id) |>
  mutate(match_no = row_number(),
         try_scored = (tries > 0),
         prev_try = ifelse(lag(try_scored), lag(match_summary), "-") |> as.character() |> replace_na("Debut")) |>
  mutate(seq_no = cumsum(try_scored),
         seq_no = ifelse(seq_no != lag(seq_no), seq_no - 1, seq_no) |> replace_na(0)) |> 
  group_by(player_id, seq_no) |>
  summarise(tryless_matches = ifelse(try_scored[which.max(match_no)], n() - 1, n()),
            prev_try = prev_try[1],
            next_try = ifelse(try_scored[which.max(match_no)], match_summary[which.max(match_no)], "-"),
            .groups = "drop") |>
  arrange(desc(tryless_matches), prev_try) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  left_join(player_career_years |> select(player_id, career_years, last_year), by = "player_id") |>
  select(player_id, career_years, full_name, tryless_matches, prev_try, next_try)

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
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold",
              `font-size` = "15px"
            ))

# Final table formatting
final_table <- longest_tryless_streaks |>
  filter(tryless_matches >= 100) |>
  mutate(next_try = ifelse(player_id == 23475, "2024 Rd23", next_try),
         num = ifelse(tryless_matches == lag(tryless_matches), "=", row_number() |> as.character()) |> replace_na("1")) |>
  select(num, career_years, full_name, prev_try, next_try, tryless_matches) |>
  rename(
    `#` = num,
    Career = career_years,
    Player = full_name,
    `Last Try` = prev_try,
    `Next Try` = next_try,
    `# Tryless<br>Matches` = tryless_matches
  ) |>
  formattable(
    list(
      `#` = bold_formatter,
      Player = bold_formatter,
      `# Tryless<br>Matches` = num_formatter
    ),
    align = c("c", "c", "l", "l", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 14px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(6n) td {background-color: #ffd6f0;}")) |>
  prependContent(h4(class = "subtitle",
                    style = "font-family: Montserrat; font-weight: bold; margin-left: 5px;",
                    "Longest NRL/NSWRL Tryless Streaks"))
final_table

saveWidget(final_table, "tables/html/biggest_try_droughts.html")
webshot(url = "tables/html/biggest_try_droughts.html", 
        file = "tables/png/biggest_try_droughts.png", 
        selector = "body", zoom = 4,
        vwidth = 730)
