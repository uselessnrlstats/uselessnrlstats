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
match_results <- match_data |>
  select(match_id, year, match_no, date, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, year, match_no, date, home_away, team, score_for, score_against, result)

##### Analysis #####
match_letter_counts <- map(
  .x = LETTERS,
  .f = function(letter) {
    player_letter_counts <- player_data |>
      filter(first_name != "?",
             !is.na(first_name),
             nchar(first_name) > 1) |>
      select(player_id, full_name) |>
      mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |>
               str_to_upper() |>
               str_split(pattern = "")) |>
      rowwise() |>
      mutate(letter = letter,
             n_letter = sum(full_name_letters == letter),
             n_letters = length(full_name_letters)) |>
      select(-full_name_letters) |>
      ungroup()
    
    match_letter_counts <- player_match_data |>
      left_join(player_letter_counts, by = "player_id") |>
      group_by(match_id, team, letter) |>
      summarise(total_letters = sum(n_letters),
                total_letter = sum(n_letter),
                .groups = "drop") |>
      mutate(letter_prop = total_letter / total_letters)
    
    return(match_letter_counts)
  }
) |>
  list_rbind()

final_letter_counts <- match_letter_counts |>
  select(match_id, team, letter, letter_prop) |>
  pivot_wider(id_cols = c(match_id, letter), names_from = team, values_from = letter_prop) |>
  mutate(higher_prop = case_when(
    Blues > Maroons ~ "Blues",
    Maroons > Blues ~ "Maroons",
    .default = "Tie"
  )) |>
  filter(higher_prop != "Tie") |>
  left_join(match_results |> select(match_id, year, match_no, team, result), by = c("match_id", "higher_prop" = "team")) |>
  group_by(letter) |>
  summarise(
    n_matches = n(),
    n_wins = sum(result == "W"),
    win_rate = n_wins / n_matches,
    .groups =
  )
  

##### Game III Counts #####

game_3 <- map(
  .x = LETTERS,
  .f = function(letter) {
    player_letter_counts <- all_player_data |>
      filter(first_name != "?",
             !is.na(first_name),
             nchar(first_name) > 1) |>
      select(player_id, full_name) |>
      mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |>
               str_to_upper() |>
               str_split(pattern = "")) |>
      rowwise() |>
      mutate(letter = letter,
             n_letter = sum(full_name_letters == letter),
             n_letters = length(full_name_letters)) |>
      select(-full_name_letters) |>
      ungroup()
    
    game_3_letter_counts <- tibble(
      player_id = c(22807, 30858, 28598, 31079, 28828, 16573, 28813, 29833, 26152, 20869, 24106, 28479, 20851, 16582, 20586, 28619, 25861, 13914, 25859, 23732, 28258, 22808, 22840, 31354, 23476, 28411, 22720, 26157, 20713, 25860, 21898, 21472, 28815, 22966),
      match_id = 897234685,
      team = rep(c("Blues", "Maroons"), times = 17)
      ) |>
      left_join(player_letter_counts, by = "player_id") |>
      group_by(match_id, team, letter) |>
      summarise(total_letters = sum(n_letters),
                total_letter = sum(n_letter),
                .groups = "drop") |>
      mutate(letter_prop = total_letter / total_letters)
    
    return(game_3_letter_counts)
  }
) |>
  list_rbind()

game_3_winners <- game_3 |>
  select(match_id, team, letter, letter_prop) |>
  pivot_wider(id_cols = c(match_id, letter), names_from = team, values_from = letter_prop) |>
  mutate(`Higher for Game III` = case_when(
    Blues > Maroons ~ "NSW",
    Maroons > Blues ~ "QLD",
    .default = "-"
  )) |>
  select(letter, `Higher for Game III`)

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `background-color` = data.frame(team_short = x) |>
                left_join(team_data, by = "team_short") |>
                pull(team_colour),
              color = data.frame(team_short = x) |>
                left_join(team_data, by = "team_short") |>
                mutate(lum = luminance(team_colour),
                       text_col = ifelse(lum < 0.5, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

xnormalize <- function(x) {
  x <- c(x, 20, 80)
  normalize(x)[1:(length(x) - 2)]
}

pc_formatter <-
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
          padding = "0 4px", 
          `border-radius` = "4px",
          color = ifelse(luminance(colours) < 0.4, "#F2F2F2", "#1A1A1A"),
          `background-color` = colours,
          font.weight = "bold")
  })


# Final table formatting
final_table <- final_letter_counts |>
  left_join(game_3_winners, by = "letter") |>
  mutate(fraction = paste0(n_wins, " / ", n_matches),
         win_rate = percent(win_rate)) |>
  select(letter, fraction, win_rate, `Higher for Game III`) |>
  rename(
    LETTER = letter,
    `Wins/Total for Team<br>with Higher Letter %` = fraction,
    `Win Rate` = win_rate
  ) |>
  formattable(
    list(
      LETTER = bold_formatter,
      `Win Rate` = pc_formatter,
      `Higher for Game III` = team_formatter
    ),
    align = c("c", "c", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table td:nth-of-type(4n-2) { width: 150px;}")) |>
  #prependContent(tags$style("table td:nth-of-type(4n) { width: 150px;}")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 2px;",
                    "<b>Origin Numerology:</b> Who wins Origin matches when they have higher proportions of each letter in the team list" |> HTML()))
final_table

saveWidget(final_table, "tables/html/origin_numerology.html")
webshot(url = "tables/html/origin_numerology.html", 
        file = "tables/png/origin_numerology.png", 
        selector = "table", 
        zoom = 4,
        vwidth = 575)
