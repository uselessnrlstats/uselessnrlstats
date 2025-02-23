##### Description #####
# An R script to look at pi related stats

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
jersey_sum <- player_match_data |>
  left_join(player_data, by = "player_id") |>
  left_join(match_data, by = "match_id") |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "\\1", round),
         round = case_when(
           round %in% c("Prelim Final", "Prelim Playoff", "Major Prelim Semi", "Minor Prelim Semi", "Minor Prelim", "Major Prelim") ~ "PF",
           round %in% c("Semi Final", "Minor Semi", "Major Semi", "Minor Semi Rep.") ~ "SF",
           round %in% c("Qualif Final", "Qualifier", "Minor Qualif", "Major Qualif") ~ "QF",
           round %in% c("Elim Qualif", "Elim Prelim Final") ~ "EF",
           round %in% c("Grand Final", "Final", "Grand Final Rep.") ~ "GF",
           round %in% c("Grand Final Chall.") ~ "GFC",
           round %in% c("Playoff") ~ "PO",
           .default = round
         ),
         sin_bins = sin_bins + sin_bins5) |>
  select(match_id, year, round, team, player_id, full_name, number, position, sin_bins, send_offs) |>
  filter(sin_bins > 0 | send_offs > 0) |>
  mutate(player_label = case_when(
          sin_bins > 0 & send_offs == 0 ~ paste0("<span display:inline; style='color:#FFEA00;'>\u25AE</span>", ifelse(sin_bins > 1, "<span display:inline; style='color:#FFEA00;'>\u25AE</span>", "")),
          sin_bins == 0 & send_offs > 0 ~ "<span display:inline; style='color:#FF0000;'>\u25AE</span>",
          sin_bins > 0 & send_offs > 0 ~ paste0("<span display:inline; style='color:#FFEA00;'>\u25AE</span>", ifelse(sin_bins > 1, paste0("\u00D7", sin_bins), ""), "<span display:inline; style='color:#FF0000;'>\u25AE</span>"),
          .default = NA),
         player_label = paste0(full_name, player_label)
         ) |>
  group_by(match_id, year, round, team) |>
  summarise(n_players = n(),
            numbers = paste(number, collapse = "<br>"),
            players = paste(player_label, collapse = "<br>"),
            jersey_sum = sum(sin_bins * number + send_offs * number),
            .groups = "drop") |>
  mutate(numbers = paste0("<b>", numbers, "</b>")) |>
  filter(year >= 1998) |>
  arrange(desc(jersey_sum), year) |>
  select(year, round, team, numbers, players, jersey_sum)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `background-color` = data.frame(team_unique = x) |>
                left_join(team_logos, by = "team_unique") |>
                pull(team_colour),
              color = data.frame(team_unique = x) |>
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
final_table <- jersey_sum |> 
  left_join(team_data |> select(team_name, team_unique), by = c("team" = "team_name")) |>
  relocate(team_unique, .after = team) |>
  select(-team) |>
  filter(jersey_sum > 35) |>
  rename(
    Year = year,
    Rd = round,
    Team = team_unique,
    `#` = numbers,
    Players = players,
    `# Sum` = jersey_sum
  ) |>
  formattable(
    list(
      Team = team_formatter,
      `# Sum` = bold_formatter
    ),
    align = c("c", "c", "l", "r", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(1) td {background-color: #ffd6f0;}")) |>
  #prependContent(tags$style("table td:nth-child(4), td:nth-child(5) { width: 180px;}")) |>
  prependContent(h5(class = "title",
                    style = "font-family: Montserrat; font-weight: bold; margin-left: 5px;",
                    "NRL era: Largest jersey sum of players sin-binned or sent-off in a team in one match"))
final_table

saveWidget(final_table, "tables/html/jersey_sum_bins.html")
webshot(url = "tables/html/jersey_sum_bins.html", 
        file = "tables/png/jersey_sum_bins.png", 
        selector = "body", zoom = 4,
        vwidth = 570)
