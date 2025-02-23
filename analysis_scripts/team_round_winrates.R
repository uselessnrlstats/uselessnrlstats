##### Description #####
# An R script to look at the most successful clubs in each round (min 5yrs)

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmlwidgets)
  library(webshot2)
}

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
team_rankings <- match_data |>
  mutate(year = year(date)) |>
  filter(year >= 1998) |>
  select(year, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(result = case_when(
    home_team_score > away_team_score & home_away == "home_team" ~ "W",
    home_team_score > away_team_score & home_away == "away_team" ~ "L",
    home_team_score < away_team_score & home_away == "home_team" ~ "L",
    home_team_score < away_team_score & home_away == "away_team" ~ "W",
    .default = "D")) |>
  select(year, round, team, result) |>
  filter(grepl("Round", round)) |>
  mutate(round = as.integer(gsub("Round (\\d+)$", "\\1", round))) |>
  left_join(team_data |> select(-team_short), by = c("team" = "team_name")) |>
  group_by(round, team_unique) |>
  summarise(matches = n(),
            wins = sum(result == "W"),
            win_rate = wins / matches,
            .groups = "drop") |>
  arrange(round, desc(win_rate), desc(matches)) |>
  filter(matches >= 10) |>
  group_by(round) |>
  select(-wins) |>
  rename(`Round` = round,
         `Win Rate` = win_rate,
         `Matches` = matches)

# Extract bottom and top teams
top_teams <- team_rankings |>
  filter(row_number() == 1) |>
  rename(`Best Team` = team_unique) |>
  ungroup()

bottom_teams <- team_rankings |>
  filter(row_number() == max(row_number())) |>
  rename(`Worst Team` = team_unique) |>
  rename(`Win Rate ` = `Win Rate`,
         `Matches ` = `Matches`) |>
  ungroup() |>
  select(-Round)

teams_table <- top_teams |> 
  cbind(bottom_teams)

# Set up table formatting
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
                       text_col = ifelse(lum < 0.35, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
              ))

# Final table formatting
final_table <- teams_table |>
  formattable(
    list(
      `Round` = bold_formatter,
      `Win Rate` = percent,
      `Win Rate ` = percent,
      `Best Team` = team_formatter,
      `Worst Team` = team_formatter
    )
  )

saveWidget(as.htmlwidget(final_table), "table.html")

webshot(url = "table.html", file = "table.png", 
        vwidth = 900, vheight = 275, zoom = 2)
