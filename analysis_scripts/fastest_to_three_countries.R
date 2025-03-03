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
venue_data <- read_csv("cleaned_data/nrl/venue_data.csv") |>
  mutate(country_abbr = case_when(
    country == "Australia" ~ "AUS",
    country == "New Zealand" ~ "NZ",
    country == "United States of America" ~ "USA",
    .default = NA
  ))
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
fewest_to_3_countries <- player_match_data |>
  left_join(team_data, by = c("team" = "team_name")) |>
  left_join(match_data, by = "match_id") |>
  left_join(venue_data, by = "venue_id") |>
  select(player_id, match_id, date, team, team_unique, team_abbr, country, country_abbr) |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(match_number = row_number(),
         countries_so_far = map(1:n(), ~ list(unique(country[1:.x]))),
         countries_so_far_abbr = map_chr(1:n(), ~ paste0(unique(country_abbr[1:.x]), collapse = ", ")),
         country_count = map_int(1:n(), ~ length(unique(country[1:.x])))
         ) |>
  filter(country_count == 3) |>
  filter(match_number == min(match_number)) |>
  ungroup() |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(player_id, team_unique, full_name, match_number, countries_so_far_abbr, country_count) |> 
  arrange(match_number) |>
  filter(!(player_id %in% c(21711, 33403, 47242, 37919, 35673, 39655, 22622, 39738, 22795, 30864, 30911, 35568, 37230, 35654, 47245, 30866, 26515, 28832, 35472, 35545)))

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
final_table <- fewest_to_3_countries |>
  select(full_name, team_unique, match_number, countries_so_far_abbr) |>
  ungroup() |>
  filter(row_number() <= 9) |>
  rename(
    Player = full_name,
    Team = team_unique,
    `Match #` = match_number,
    Countries = countries_so_far_abbr
  ) |>
  formattable(
    list(
      Player = bold_formatter,
      `Team` = team_formatter,
      `Match #` = num_bold_formatter
    ),
    align = c("l", "l", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(1) td {background-color: #ffd6f0;}")) |>
  prependContent(h4(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0",
                    "Fastest to 3 countries")) |>
  prependContent(h5(class = "subtitle",
                    style = "text-align: center; font-size: 12px; font-family: Montserrat;",
                    "NRL players to have played matches in 3 different countries in the fewest career matches"))
final_table

saveWidget(final_table, "tables/html/fastest_to_3_countries.html")
webshot(url = "tables/html/fastest_to_3_countries.html", 
        file = "tables/png/fastest_to_3_countries.png", 
        selector = "div", zoom = 4,
        vwidth = 650)
