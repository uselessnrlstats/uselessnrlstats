##### Description #####
# An R script to look at players who wore sequential jersey in their career

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

##### Analysis #####
decreasing_jerseys <- player_match_data |>
  select(player_id, match_id, team, opposition_team, number) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  left_join(match_data |> select(match_id, competition_year, round, date), by = "match_id") |>
  arrange(player_id, date) |>
  group_by(player_id) |>
  mutate(career_matches = n(),
         career_match = row_number(),
         lesser_number = (number <= (lag(number))) |> replace_na(FALSE)) |>
  filter(lesser_number | lead(lesser_number)) |>
  mutate(seq_no = cumsum(!lesser_number)) |>
  ungroup() |>
  group_by(player_id, seq_no) |>
  filter(n() == max(career_matches)) |>
  ungroup() |>
  group_by(player_id, full_name) |>
  summarise(numbers = paste(unique(number), collapse = " "),
            numbers = list(unique(number)),
            n_numbers = length(unique(number)),
            matches = n(),
            debut = min(date),
            retired = max(date),
            .groups = "drop") |>
  arrange(desc(matches), debut, full_name)

  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round),
         round = case_when(
           round %in% c("Minor Prelim Semi") ~ "MPSF",
           round %in% c("Prelim Final") ~ "PF",
           round %in% c("Semi Final") ~ "SF",
           round %in% c("Qualif Final", "Qualifier") ~ "QF",
           round %in% c("Elim Qualif") ~ "EF",
           round %in% c("Grand Final") ~ "GF",
           .default = round)) |>
  select(competition_year, round, team, opposition_team, full_name, goals, career_match)

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

# Final table formatting
final_table <- most_decreasing_goals |>
  rename(
    Year = competition_year,
    Round = round,
    `Team` = team,
    `Opposition` = opposition_team,
    `Player` = full_name,
    `Goals` = goals,
    `Match #` = career_match
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      Goals = bold_formatter,
      `Team` = team_formatter,
      `Opposition` = team_formatter
    ),
    align = c("r", "c", "r", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 10px;",
                    "Sequentially Decreasing Goals")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 10px;",
                    "<b>David Brooks</b> once kicked 6,5,4,3,2,1 and 0 goals in 7 consecutive matches that he played in. This 7-match decreasing sequence is an NRL/NSWRFL record." |> HTML()))
final_table

saveWidget(final_table, "tables/html/decreasing_goals.html")
webshot(url = "tables/html/decreasing_goals.html", 
        file = "tables/png/decreasing_goals.png", 
        selector = "body", zoom = 4,
        vwidth = 700)
