##### Description #####
# An R script to look at a rainbow ladder for all teams

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(TSP)
  library(purrr)
  library(scales)
}

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")
ladder_data <- read_csv("cleaned_data/nrl/ladder_round_data.csv")

##### Analysis #####
all_teams <- match_data |>
  select(away_team) |>
  rename(team_name = away_team) |>
  distinct() |>
  left_join(team_data, by = "team_name") |>
  left_join(team_logos, by = "team_unique") |>
  select(team_name, team_colour) |>
  arrange(team_name)

rainbow_order <- all_teams[c(10,35,18,25,26,27,28,6,8,9,41,20,1,29,37,16,3,24,46,7,11,5,45,12,33,36,2,4,44,49,38,39,40,30,31,47,17,13,14,42,43,21,15,22,32,23,34,19,48),]

# Finding the best round
rainbow_means_fwd <- ladder_data |> 
  distinct(competition_year, round) |> 
  split(~ competition_year + round) |>
  purrr::keep(~nrow(.) > 0) |>
  map(
    .f = function(df) {
      rd_ladder <- ladder_data |>
        filter(competition_year == df$competition_year,
               round == df$round) |>
        select(ladder_position, team)
      rd_teams_rainbow <- rainbow_order |>
        filter(team_name %in% rd_ladder$team) |>
        mutate(rainbow_position = row_number()) |>
        arrange(desc(rainbow_position)) |> mutate(rainbow_position = row_number())
      
      rainbow_mean <- rd_ladder |>
        left_join(rd_teams_rainbow, by = c("team" = "team_name")) |>
        mutate(rainbow_diff = abs(ladder_position - rainbow_position)) |>
        summarise(rainbow_mean = mean(rainbow_diff) / max(ladder_position),
                  n_teams = max(ladder_position))
      
      return(df |> cbind(rainbow_mean))
    },
    .progress = TRUE
  ) |>
  list_rbind() |>
  arrange(rainbow_mean)

##### Viewing the best round #####
#1943 Rd1
#1989 Rd8
#2001 Rd6

#1961 Rd13
#2014 Rd3
final_ladder <- ladder_data |>
  filter(year == 2021, round == 10)


##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `vertical.align` = "center",
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
              font.weight = "bold"
            ))

# Final table formatting
final_table <- final_ladder |>
  mutate(
    pos = ordinal(ladder_position),
    record = paste(wins, draws, losses, sep = "-")) |>
  select(pos, team, points, played, record, score_for, score_against, score_diff) |>
  rename(
    `Pos.` = pos,
    `Team` = team,
    Pts = points,
    `Pld` = played,
    `W-D-L` = record,
    PF = score_for,
    PA = score_against,
    PD = score_diff
  ) |>
  formattable(
    list(
      `Team` = team_formatter
    ),
    align = c("r", "r", "r", "r", "c", "r", "r", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "Rainbow Ladder"))
final_table

saveWidget(final_table, "tables/html/rainbow_ladder_all.html")
webshot(url = "tables/html/rainbow_ladder_all.html", 
        file = "tables/png/rainbow_ladder_all.png", 
        selector = "body", zoom = 4,
        vwidth = 650)
