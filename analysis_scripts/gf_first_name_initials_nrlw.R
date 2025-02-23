##### Description #####
# An R script to look at players who kicked at 100% on debut

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
player_data <- read_csv("cleaned_data/nrlw/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrlw/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrlw/match_data.csv")
team_data <- read_csv("cleaned_data/nrlw/team_data.csv")  |> mutate(team_unique = team_name)
team_logos <- read_csv("cleaned_data/nrlw/team_logos.csv") |> rename(team_unique = team_name)

##### Analysis #####
first_name_initials <- player_match_data |>
  left_join(match_data, by = "match_id") |>
  select(match_id, competition_year, round, date, player_id, home_team, away_team) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  filter(round %in% c("Grand Final", "Final", "Grand Final Rep.")) |>
  mutate(round = case_when(
    round == "Grand Final" ~ "GF",
    round == "Final" ~ "GF",
    round == "Grand Final Rep." ~ "GF Rep.",
    .default = NA
  )) |>
  left_join(player_data |> select(player_id, first_name, full_name), by = "player_id") |>
  filter(first_name != "?") |>
  mutate(first_name_initial = substr(first_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = rev(LETTERS), ordered = TRUE)) |>
  group_by(match_id, competition_year, year, home_team, away_team) |>
  summarise(n_players = n(),
            n_letters = length(unique(first_name_initial)),
            letters = paste0(rev(sort(unique(first_name_initial))), collapse = ""),
            .groups = "drop") |>
  rowwise() |>
  mutate(letters_pattern = paste0("([", letters, "]+)"),
         all_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
         all_letters = gsub(letters_pattern, "<b style='color: #000000;'>\\1</b>", all_letters),
         all_letters = paste0('<span style="font-size: 14px; font-weight: bold; color: #BBBBBB">', all_letters, '</span>')) |>
  arrange(desc(n_letters), desc(year)) |>
  select(competition_year, home_team, away_team, all_letters, n_letters) |>
  mutate(home_team = gsub(" \\(W\\)", "", home_team),
         away_team = gsub(" \\(W\\)", "", away_team))

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
                mutate(team_name = paste0(team_name, " (W)")) |>
                left_join(team_data, by = "team_name") |>
                left_join(team_logos, by = "team_unique") |>
                pull(team_colour),
              color = data.frame(team_name = x) |>
                mutate(team_name = paste0(team_name, " (W)")) |>
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
              font.family = "Montserrat",
              font.weight = "bold"
            ))

# Final table formatting
final_table <- first_name_initials |>
  rename(
    `Grand<br>Final` = competition_year,
    `Home Team` = home_team,
    `Away Team` = away_team,
    `First Name Initials` = all_letters,
    `#<br>Letters` = n_letters
  ) |>
  formattable(
    list(
      `Home Team` = team_formatter,
      `Away Team` = team_formatter,
      `#<br>Letters` = num_formatter
    ),
    align = c("c", "r", "l", "c", "c"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(1) td {background-color: #ffd6f0;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 16px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "Most unique first name initials in an NRLW Grand Final"))
final_table

saveWidget(final_table, "tables/html/gf_first_name_init_nrlw.html")
webshot(url = "tables/html/gf_first_name_init_nrlw.html", 
        file = "tables/png/gf_first_name_init_nrlw.png", 
        selector = "div", zoom = 4,
        vwidth = 846)
