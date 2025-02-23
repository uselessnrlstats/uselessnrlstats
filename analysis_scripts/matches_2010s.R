##### Description #####
# An R script to look at the number of matches in the 2010s

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(showtext)
}

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
year_breakdown <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         decade = floor(year / 10) * 10) |>
  filter(decade == 2010) |>
  group_by(year)|>
  summarise(n_matches = n(),
            .groups = "drop") |>
  rbind(tibble(year = "TOTAL", n_matches = "<b>2010</b>"))
  

top_2010_players <- player_match_data |>
  left_join(match_data |> select(match_id, competition_year), by = "match_id") |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         decade = floor(year / 10) * 10) |>
  filter(decade == 2010) |>
  group_by(player_id) |>
  summarise(n_matches = n(),
            .groups = "drop") |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(n_matches, full_name) |>
  arrange(desc(n_matches)) |>
  mutate(perc_2010 = n_matches / 2010)

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

# Table 1 formatting
final_table1 <- year_breakdown |>
  rename(
    Year = year,
    `Matches Played` = n_matches
  ) |>
  formattable(
    list(
      `Year` = bold_formatter
    ),
    align = c("c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "NRL Matches in the 2010s"))
final_table1

saveWidget(final_table1, "tables/html/matches_2010s1.html")
webshot(url = "tables/html/matches_2010s1.html", 
        file = "tables/png/matches_2010s1.png", 
        selector = "body", zoom = 4,
        vwidth = 330)

##### table 2 #####
final_table2 <- top_2010_players |>
  filter(row_number() <= 15) |>
  select(full_name, n_matches, perc_2010) |>
  mutate(perc_2010 = percent(perc_2010)) |>
  rename(
    `Matches Played` = n_matches,
    Player = full_name,
    `Perc of Total` = perc_2010
  ) |>
  formattable(
    list(
      `Player` = bold_formatter
    ),
    align = c("r", "r", "r"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Most NRL matches in 2010s"))
final_table2

saveWidget(final_table2, "tables/html/matches_2010s2.html")
webshot(url = "tables/html/matches_2010s2.html", 
        file = "tables/png/matches_2010s2.png", 
        selector = "body", zoom = 4,
        vwidth = 400)
