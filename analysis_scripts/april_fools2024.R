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
matches_14 <- match_data |>
  filter(month(date) == 4, day(date) == 1) |>
  select(match_id, competition_year, round, date)

##### Analysis #####
send_offs_14 <- player_match_data |>
  filter(match_id %in% matches_14$match_id) |>
  filter(sin_bins > 0 | sin_bins5 > 0 | send_offs > 0) |>
  left_join(player_data |> select(player_id, full_name),
            by = "player_id") |>
  left_join(matches_14, by = "match_id") |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "\\1", round) |> as.numeric(),
         date = format(date, "%d/%m"),
         full_name = ifelse(captain, paste(full_name, "\u00A9"), full_name)) |>
  select(year, date, round, full_name, team, opposition_team, sin_bins5, sin_bins, send_offs) |>
  rename(`Sin Bin (5)` = sin_bins5, `Sin Bin` = sin_bins, `Send Off` = send_offs) |>
  pivot_longer(cols = c(`Sin Bin (5)`, `Sin Bin`, `Send Off`),
               names_to = "Punishment",
               values_to = "number") |>
  filter(number > 0) |> select(-number) |>
  arrange(year)
  
 
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
final_table <- send_offs_14 |>
  rename(
    Player = full_name,
    Date = date,
    Year = year,
    Rd = round,
    Team = team,
    Opposition = opposition_team
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Opposition = team_formatter,
      Player = bold_formatter,
      Punishment = bold_formatter
    ),
    align = c("c", "c", "c", "r", "r", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "April Fools")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL players sin-binned or sent-off on April 1st"))
final_table

saveWidget(final_table, "tables/html/april_fools2024.html")
webshot(url = "tables/html/april_fools2024.html", 
        file = "tables/png/april_fools2024.png", 
        selector = "body", zoom = 4,
        vwidth = 800)

# Final table 2 formatting
final_table2 <- send_offs_14 |>
  mutate(
    full_name = c(
      "Marcus Quair",
      "Con Virshen",
      "Moe Mentum",
      "Sal Aricap",
      "Sir Render",
      "Lou Scarry",
      "Don Targew",
      "Mull E Gruber",
      "Doug Deep",
      "Chip Kik",
      "Lynne Spede",
      "Squire L Grip",
      "Earl Eborl",
      "Sue Pircoatch",
      "Juan Pessenter",
      "Finn Sup"
    )
  ) |>
  rename(
    Player = full_name,
    Date = date,
    Year = year,
    Rd = round,
    Team = team,
    Opposition = opposition_team
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Opposition = team_formatter,
      Player = bold_formatter,
      Punishment = bold_formatter
    ),
    align = c("c", "c", "c", "r", "r", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "April Fools")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL players sin-binned or sent-off on April 1st"))
final_table2

saveWidget(final_table2, "tables/html/april_fools2024_2.html")
webshot(url = "tables/html/april_fools2024_2.html", 
        file = "tables/png/april_fools2024_2.png", 
        selector = "body", zoom = 4,
        vwidth = 800)
