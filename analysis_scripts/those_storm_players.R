##### Description #####
# An R script to look at those storm players

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
match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, round, home_away, team, score_for, score_against, result)

broncs <- player_data |>
  filter(full_name %in% c("Shane Webcke", "Corey Parker", "Dane Carlaw")) |>
  select(player_id, full_name)

##### Analysis #####
matches_players <- player_match_data |>
  filter(opposition_team == "Melbourne Storm",
         player_id %in% broncs$player_id) |>
  group_by(player_id) |>
  summarise(matches = list(unique(match_id)),
            .groups = "drop") |>
  left_join(broncs, by = "player_id")

matches <- player_match_data |>
  filter(opposition_team == "Melbourne Storm",
         player_id %in% broncs$player_id) |>
  pull(match_id) |> unique()

results_tables <- map(.x = 1:3,
    .f = function(x) {
      player_match_data |>
        filter(team == "Melbourne Storm", match_id %in% matches_players$matches[[x]]) |>
        left_join(match_results |> select(match_id, team, result),
                  by = c("match_id", "team")) |>
        left_join(player_data |> select(player_id, full_name),
                  by = "player_id") |>
        group_by(player_id, full_name) |>
        summarise(`n_matches` = n(), 
                  n_wins = sum(result == "W"),
                  win_rate = n_wins / n_matches,
                  .groups = "drop") |>
        arrange(desc(win_rate), desc(n_wins)) |>
        mutate(win_rate = percent(win_rate, digits = 2)) |>
        filter(n_matches >= 5) |>
        filter(row_number() <= 10) |>
        select(-player_id) |>
        rename(
          `Melbourne Player` = full_name,
          `# Matches` = n_matches,
          `# Wins` = n_wins,
          `Win Rate` = win_rate
        )
  }) |>
  setNames(nm = matches_players$full_name)


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

xnormalize <- function(x) {
  x <- c(x, 0, 100)
  normalize(x)[1:(length(x) - 2)]
}

pd_formatter <-
  formatter("span", style = function(x) {
    colours <- gsub("%", "", x) |>
      as.numeric() |>
      xnormalize() |>
      colorRamp(c("green4",'white', "firebrick1"))() |>
      as.integer() |>
      matrix(byrow = TRUE, dimnames = list(c("red", "green", "blue"), NULL), nrow = 3) |>
      csscolor()
    style(display = "block",
          'text-align' = 'center',
          padding = "0 4px", 
          `border-radius` = "4px",
          color = ifelse(luminance(colours) < 0.35, "#F2F2F2", "#1A1A1A"),
          `background-color` = colours,
          font.weight = "bold")
  })

# Final table formatting
map(.x = 1:3,
    .f = function(x) {
      final_table <- results_tables[[x]] |>
        formattable(
          list(
            `Melbourne Player` = bold_formatter,
            `Win Rate` = pd_formatter
          ),
          align = c("r", "c", "c", "c"),
          table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
        ) |>
        as.htmlwidget() |>
        prependContent(tags$style("td { padding: 2px  !important;}")) |>
        prependContent(h5(class = "subtitle",
                          style = "font-family: Roboto Regular; margin-left: 25px;",
                          HTML(paste0("Best win rate for Storm against <b>", names(results_tables)[x], "</b>"))))
      final_table
      
      saveWidget(final_table, paste0("tables/html/those_storm_players_", names(results_tables)[x], ".html"))
      webshot(url = paste0("tables/html/those_storm_players_", names(results_tables)[x], ".html"), 
              file = paste0("tables/png/those_storm_players_", names(results_tables)[x], ".png"), 
              selector = "body", zoom = 4,
              vwidth = 550)
    }
)




