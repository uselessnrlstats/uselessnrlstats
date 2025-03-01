##### Description #####
# An R script to look at players who reached 4 different teams quickest

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

##### Helper Stats #####
# timespan in years and days
time_years_days <- function(start_date, end_date) {
  year_span <- interval(start_date, end_date) |>
    time_length("years") |>
    floor()
  day_span <- interval(start_date %m+% lubridate::years(year_span), end_date) |>
    time_length("days") |>
    as.numeric()
  time_span <- paste0(year_span, "yrs ", day_span, " days")
  return(time_span)
}

##### Analysis #####
last_by_decade <- player_match_data |>
  select(player_id, match_id, team) |>
  left_join(player_data |> select(player_id, full_name, birthday), by = "player_id") |>
  left_join(match_data |> select(match_id, date, time), by = "match_id") |>
  filter(!is.na(birthday)) |>
  group_by(player_id) |>
  arrange(date) |>
  filter(row_number() == 1) |>
  ungroup() |>
  mutate(birth_year = year(birthday),
         birth_decade = 10 * floor(birth_year / 10)) |>
  mutate(age = time_years_days(birthday, date)) |>
  group_by(birth_decade) |>
  arrange(desc(date), desc(time)) |>
  filter(row_number() == 1)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "2px 4px 2px 4px", 
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
              display = "block"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat"
            ))

num_bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat",
              font.weight = "bold"
            ))

# Final table formatting
final_table <- last_by_decade |>
  arrange(birth_decade) |>
  select(birth_decade, birthday, full_name, age, team, date) |>
  mutate(full_name = ifelse(birth_decade >= 1990, paste0(full_name, "*"), full_name),
         birthday = format(birthday, "%d/%m/%Y"),
         date = format(date, "%d/%m/%Y"),
         birth_decade = paste0(birth_decade, "s")) |>
  rename(
    Decade = birth_decade,
    Birthday = birthday,
    Player = full_name,
    `Age at Debut` = age,
    Team = team,
    `Debut Date` = date
  ) |>
  formattable(
    list(
      Decade = num_bold_formatter,
      Team = team_formatter,
      `Player` = bold_formatter,
      `Age at Debut` = num_formatter
    ),
    align = c("c", "c", "l", "l", "l", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(12) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "Last NRL/NSWRL player born in each decade to debut"))
final_table

saveWidget(final_table, "tables/html/last_born_decade.html")
webshot(url = "tables/html/last_born_decade.html", 
        file = "tables/png/last_born_decade.png", 
        selector = "div", zoom = 4,
        vwidth = 800)
