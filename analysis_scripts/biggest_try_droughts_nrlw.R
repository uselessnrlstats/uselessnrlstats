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
  library(webshot)
  library(scales)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/nrlw/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/nrlw/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/nrlw/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/nrlw/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/nrlw/team_logos.csv")

##### Helper Stats #####
player_teams <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  arrange(date) |>
  group_by(player_id) |>
  summarise(final_team = team[max(row_number())],
            .groups = "drop")

##### Analysis #####
longest_tryless_streaks <- player_match_data |>
  mutate(tries = tries + penalty_tries) |>
  left_join(match_data, by = "match_id") |>
  select(match_id, competition_year, round, date, team, opposition_team, player_id, tries) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round),
         round = case_when(
           round %in% c("Prelim Final", "Prelim Playoff", "Major Prelim Semi", "Minor Prelim Semi", "Minor Prelim", "Major Prelim") ~ "PF",
           round %in% c("Semi Final", "Minor Semi", "Major Semi", "Minor Semi Rep.") ~ "SF",
           round %in% c("Qualif Final", "Qualifier", "Minor Qualif", "Major Qualif") ~ "QF",
           round %in% c("Elim Qualif", "Elim Prelim Final") ~ "EF",
           round %in% c("Grand Final", "Final", "Grand Final Rep.") ~ "GF",
           round %in% c("Grand Final Chall.") ~ "GFC",
           round %in% c("Playoff") ~ "PO",
           .default = round
         )) |>
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
  left_join(player_teams, by = "player_id") |>
  select(player_id, final_team, full_name, tryless_matches, prev_try, next_try)

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
                left_join(team_logos, by = "team_name") |>
                pull(team_colour),
              color = data.frame(team_name = x) |>
                left_join(team_logos, by = "team_name") |>
                mutate(lum = luminance(team_colour),
                       text_col = ifelse(lum < 0.45, "#F2F2F2", "#1A1A1A")) |>
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
  #filter(next_try == "-") |>
  filter(row_number() <= 10) |>
  mutate(num = ifelse(tryless_matches == lag(tryless_matches), "=", row_number() |> as.character()) |> replace_na("1")) |>
  select(num, full_name, final_team, prev_try, next_try, tryless_matches) |>
  rename(
    `#` = num,
    Player = full_name,
    `Current Team` = final_team,
    `Last Try` = prev_try,
    `Next Try` = next_try,
    `# Tryless<br>Matches` = tryless_matches
  ) |>
  formattable(
    list(
      `#` = bold_formatter,
      Player = bold_formatter,
      `Current Team` = team_formatter,
      `# Tryless<br>Matches` = num_formatter
    ),
    align = c("c", "c", "l", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 14px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(3) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 18px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "Longest NRLW tryless streaks"))
final_table

saveWidget(final_table, "tables/html/biggest_try_droughts_nrlw.html")
webshot(url = "tables/html/biggest_try_droughts_nrlw.html", 
        file = "tables/png/biggest_try_droughts_nrlw.png", 
        selector = "div", zoom = 4,
        vwidth = 730)
