##### Description #####
# An R script to look at a ladder for the wooden spoon

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
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")
ladder_data <- read_csv("cleaned_data/nrl/ladder_round_data.csv")

##### Helper Stats #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

##### Analyis #####
spoon_ladder <- ladder_data |>
  group_by(year) |>
  filter(round == max(round)) |>
  filter(ladder_position == max(ladder_position)) |>
  ungroup() |>
  filter(year >= 2007) |>
  mutate(points_no_byes = ifelse(year == 2010, points, points - 2*byes),
         win_perc = percent(round(wins / played, 4))) |>
  arrange(year == 2010, desc(wins / played), desc(points_no_byes), desc(score_diff), desc(score_for), desc(score_against)) |>
  left_join(team_data |> select(team_name, team_unique), by = c("team" = "team_name")) |>
  mutate(
    pos = row_number(),
    pts_both = paste0("<b>", points_no_byes, "</b> (", points, ")"),
    team_label = paste(year, team_unique),
    record = paste0("<b>", paste(wins, draws, losses, sep = "-"), "</b> (", byes, ")")) |>
  select(pos, team_label, played, pts_both, record, score_diff, win_perc)

##### Table Formatting #####
team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "2px 4px 2px 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `background-color` = data.frame(team_unique = x) |>
                mutate(team_unique = gsub("(^\\d+ )", "", team_unique)) |>
                left_join(team_logos, by = "team_unique") |>
                pull(team_colour),
              color = data.frame(team_unique = x) |>
                mutate(team_unique = gsub("(^\\d+ )", "", team_unique)) |>
                left_join(team_logos, by = "team_unique") |>
                mutate(lum = luminance(team_colour),
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
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
      colorRamp(c("firebrick1",'white', "green4"))() |>
      as.integer() |>
      matrix(byrow = TRUE, dimnames = list(c("red", "green", "blue"), NULL), nrow = 3) |>
      csscolor()
    style(display = "block",
          'text-align' = 'center',
          padding = "2px 4px 2px 4px", 
          `border-radius` = "4px",
          color = ifelse(luminance(colours) < 0.5, "#F2F2F2", "#1A1A1A"),
          `background-color` = colours,
          font.weight = "bold")
  })

# Final table formatting
final_table <- spoon_ladder |>
  rename(
    `Pos.` = pos,
    `Team` = team_label,
    `Pts (+B)` = pts_both,
    `Pld` = played,
    `W-D-L (B)` = record,
    PD = score_diff,
    `Win %` = win_perc
  ) |>
  formattable(
    list(
      `Pos.` = bold_formatter,
      `Team` = team_formatter,
      `Win %` = pd_formatter
    ),
    align = c("r", "l", "c", "c", "c", "r", "r"),
    table.attr = 'class=\"table table-hoverable\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {font-size: 14px; background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  #prependContent(tags$style("table td:nth-child(1), td:nth-child(3) { width: 220px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-family: Montserrat; font-weight: bold; padding: 6px 0 0 0;",
                    "Ladder of NRL Wooden Spoon winners from 2007-2024"))
final_table

saveWidget(final_table, "tables/html/spoon_ladder.html")
webshot(url = "tables/html/spoon_ladder.html", 
        file = "tables/png/spoon_ladder.png", 
        selector = "div", zoom = 4,
        vwidth = 620)
