##### Description #####
# An R script to look at US states appearing in player names

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
player_data <- rbind(
  read_csv("cleaned_data/nrl/player_data.csv"),
  read_csv("cleaned_data/nrlw/player_data.csv")) |>
  select(1:4)
player_match_data <- rbind(
  read_csv("cleaned_data/nrl/player_match_data.csv") |> select(1:7),
  read_csv("cleaned_data/nrlw/player_match_data.csv") |> select(1:7))
match_data <- rbind(
  read_csv("cleaned_data/nrl/match_data.csv"),
  read_csv("cleaned_data/nrlw/match_data.csv"))
team_data <- rbind(
  read_csv("cleaned_data/nrl/team_data.csv"),
  read_csv("cleaned_data/nrlw/team_data.csv") |> mutate(team_unique = team_name)
)
team_logos <- rbind(
  read_csv("cleaned_data/nrl/team_logos.csv"),
  read_csv("cleaned_data/nrlw/team_logos.csv") |> rename(team_unique = team_name)
)

##### Helper Stats #####

##### Analysis #####
number_Os <- player_data |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  select(player_id, first_name, full_name) |>
  mutate(first_name_letters = gsub("[^a-zA-Z]", "", first_name) |>
           str_to_upper() |>
           str_split(pattern = "")) |>
  rowwise() |>
  mutate(n_Os = sum(first_name_letters %in% c("O", "o"))) |>
  select(-first_name_letters)

no_Os <- player_match_data |>
  left_join(number_Os, by = "player_id") |>
  group_by(match_id, team) |>
  summarise(total_Os = sum(n_Os),
            n_players = n(),
            .groups = "drop") |>
  filter(total_Os == 0) |>
  left_join(match_data |> select(match_id, competition_year, round), by = "match_id") |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "\\1", round)) |>
  arrange(year, round)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "4px 4px 4px 4px",
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
              display = "block", 
              padding = "2px 4px 2px 4px"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "14px",
              font.family = "Montserrat"
            ))

# Final table formatting
final_table <- same_name_debut |>
  select(competition_year, round, first_name, team, players) |>
  rbind(tibble(competition_year = "NRLW 2024", round = "Round 8", team = "Wests Tigers (W)", first_name = "Claudia", players = "Claudia Nielsen<br>Claudia Brown")) |>
  mutate(round = gsub("Round (\\d+)", "Rd\\1", round),
         players = paste0("<b>", players, "</b>")) |>
  rename(
    Year = competition_year,
    Rd = round,
    Team = team,
    Name = first_name,
    Players = players
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Name = bold_formatter
    ),
    align = c("r", "l", "c", "l", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 14px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(5) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 18px; font-weight: bold; font-family: Montserrat; padding: 3px 0 0 0;",
                    "Players with the same first name debuting for the same team in the same match (NRL/NRLW era)"))
final_table

saveWidget(final_table, "tables/html/same_name_debut.html")
webshot(url = "tables/html/same_name_debut.html",
        file = "tables/png/same_name_debut.png",
        selector = "div", zoom = 4,
        vwidth = 630)
