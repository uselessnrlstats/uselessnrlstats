##### Description #####
# An R script to look at longest time betweena  first name appearing in the NRL

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
time_between_first_name <- player_match_data |>
  select(player_id, match_id, team) |>
  left_join(player_data |> select(player_id, first_name, full_name), by = "player_id") |>
  left_join(match_data |> select(match_id, competition_year, round, date), 
            by = "match_id") |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  group_by(first_name) |>
  arrange(date) |>
  mutate(day_gap = ifelse(row_number() == 1,
                          NA,
                          difftime(date, lag(date), units = "days")),
         time_gap = ifelse(is.na(day_gap),
                           NA,
                           time_years_days(lag(date), date))) |>
  mutate(prev_player = lag(full_name),
         prev_player_id = lag(player_id),
         prev_team = lag(team),
         prev_date = lag(date)) |>
  ungroup() |>
  filter(!is.na(day_gap)) |>
  filter(player_id != prev_player_id) |>
  select(first_name, prev_date, prev_team, prev_player, date, team, full_name, time_gap, day_gap) |>
  arrange(desc(day_gap))
time_between_first_name


##### Open gaps #####
player_match_data |>
  select(player_id, match_id, team) |>
  left_join(player_data |> select(player_id, first_name, full_name), by = "player_id") |>
  left_join(match_data |> select(match_id, competition_year, round, date), 
            by = "match_id") |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  arrange(desc(date)) |>
  group_by(first_name) |>
  filter(row_number() == 1) |>
  ungroup() |>
  mutate(time_since = time_years_days(date, Sys.Date()),
         days_since = difftime(Sys.Date(), date, units = "days")) |>
  select(first_name, date, team, full_name, time_since, days_since) |>
  arrange(desc(days_since))

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
final_table <- time_between_first_name |>
  mutate(` ` = "-",
         first_name = toupper(first_name)) |>
  select(first_name, prev_player, prev_date, ` `, date, full_name, time_gap) |>
  filter(row_number() <= 6) |>
  mutate(prev_date = format(prev_date, "%d/%m/%Y"),
         date = format(date, "%d/%m/%Y")) |>
  rename(
    Name = first_name,
    `Time Gap` = time_gap,
    `Last Player` = prev_player,
    `Last Date` = prev_date,
    `Next Player` = full_name,
    `Next Date` = date
  ) |>
  formattable(
    list(
      Name = bold_formatter,
      `Time Gap` = num_bold_formatter,
      `Last Date` = num_formatter,
      `Next Date` = num_formatter
    ),
    align = c("l", "r", "r", "c", "l", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(5) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "Longest gap (in days) between the same first name playing in the NSWRL/NRL"))
final_table

saveWidget(final_table, "tables/html/longest_name_gap.html")
webshot(url = "tables/html/longest_name_gap.html", 
        file = "tables/png/longest_name_gap.png", 
        selector = "div", zoom = 4,
        vwidth = 700)
