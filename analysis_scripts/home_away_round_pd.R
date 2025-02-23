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
  library(webshot)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
away_pd <- match_data |>
  group_by(competition_year, round) |>
  summarise(n_matches = n(),
            home_total = sum(home_team_score, na.rm = TRUE), 
            away_total = sum(away_team_score, na.rm = TRUE),
            away_pd = away_total - home_total,
            .groups = "drop") |>
  arrange(desc(away_pd))

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block",
              padding = "2px 4px 2px 4px",
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
final_table <- away_pd |> 
  filter(row_number() <= 7) |>
  mutate(num = ifelse(away_pd == lag(away_pd), "=", paste0(row_number(), ".")) |> replace_na("1."),
         year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round),
         year_rd = paste0("<b>", year, " ", round, "</b> (", n_matches, ")"),
         home_away_score = paste0(home_total, " \u2013 <b>", away_total, "</b>"),
         away_pd = ifelse(away_pd > 0, paste0("+", away_pd), away_pd)) |>
  select(num, year_rd, home_away_score, away_pd) |>
  rename(
    ` ` = num,
    `Year Rd (# Games)` = year_rd,
    `Home v Away` = home_away_score,
    `Away PD` = away_pd
  ) |>
  formattable(
    list(
      ` ` = bold_formatter,
      `Away PD` = bold_formatter
    ),
    align = c("c", "l", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(2) td {background-color: #ffd6f0;}")) |>
  #prependContent(tags$style("table td:nth-child(4), td:nth-child(5) { width: 180px;}")) |>
  prependContent(h5(class = "title",
                    style = "font-size: 15px; text-align: center; font-family: Montserrat; font-weight: bold; padding: 3px 0 0 0;",
                    "Largest cumulative PD in favour of Away teams in a single NSWRL/NRL round" |> HTML()))
final_table

saveWidget(final_table, "tables/html/home_away_pd.html")
webshot::webshot(url = "tables/html/home_away_pd.html", 
        file = "tables/png/home_away_pd.png", 
        selector = "div", zoom = 4,
        vwidth = 410)
