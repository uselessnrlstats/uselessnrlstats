##### Description #####
# An R script to look at rock names scoring on debut

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
}

##### Load Data #####
player_data <- read_csv("origin/cleaned_data/nrl/player_data.csv")
all_player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("origin/cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("origin/cleaned_data/nrl/match_data.csv")
team_data <- read_csv("origin/cleaned_data/nrl/team_data.csv")

##### Helper Stats #####
produce_team_lineups <- function(id) {
  correct_positions <- c("FB", "W", "C", "C", "W", "FE", "HB", "FR", "HK", "FR", "2R", "2R", "L")
  
  player_match_data |>
    filter(match_id == id) |>
    select(team, position, player_id) |>
    arrange(team) |>
    group_by(team, position) |>
    mutate(position_unique = case_when(
      row_number() > 1 ~ paste0(position, row_number()),
      .default = position
    )) |>
    ungroup() |>
    pivot_wider(id_cols = c(position, position_unique), 
                names_from = team, values_from = player_id) |>
    select(position, 3, 4)
}
##### Analysis #####

game_3_teams <- tibble(
  `LEND QUEANS` = c("wee clasher", "wobbly scone", "aged again", "await basidium hoof", "malevolent shine", "dame rodent", "very scary handle (C)", "mike oaf auk iota", "gharry rant", "fluke if aussie", "beet trouncer", "wallet pucker", "tragic crank pair", "bent hun", "silly island con", "jean hire mania",  "long nap yak", "rooter intel", "lily blaster"),
  vs = c("FB", "W", "C", "C", "W", "FE", "HB", "FR", "HK", "FR", "2R", "2R", "L", "B", "B", "B", "B", "18", "COACH"),
  `WEASEL NUT SHOW` = c("swaddled yarn", "rain boot", "drabbest man", "chthonic serpent", "max colza", "ouija realm", "chillest memos", "jab jive rocket (C)", "bee crooners", "hyaena spa", "minimal rat", "churning tacos", "numeracy armor", "acorn wontons", "ahoy i sea", "tall cement birth", "cruel pennies", "tomb truant", "mariachi legume")
)


##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `background-color` = c("#B03060", NA, "#87CEFF"),
              color = c("#F2F2F2", NA, "#1A1A1A")))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- game_3_teams |>
  formattable(
    list(
      `LEND QUEANS` = bold_formatter,
      `WEASEL NUT SHOW` = bold_formatter
    ),
    col.names = team_formatter(c("LEND QUEANS", "vs", "WEASEL NUT SHOW")),
    align = c("r", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 14px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  #prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table td:nth-of-type(3n-2) { width: 250px;}")) |>
  prependContent(tags$style("table td:nth-of-type(3n) { width: 250px;}")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; text-align: center;",
                    "<b>Origin Game III: ANAGRAM TEAM LISTS</b>" |> HTML()))
final_table

saveWidget(final_table, "tables/html/origin_anagrams.html")
webshot(url = "tables/html/origin_anagrams.html", 
        file = "tables/png/origin_anagrams.png", 
        selector = "html", 
        zoom = 4,
        vwidth = 575)
