##### Description #####
# An R script to look at players with the letters RUOK in their name (NRLW)

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
player_data <- read_csv("cleaned_data/nrlw/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrlw/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrlw/match_data.csv")
team_data <- read_csv("cleaned_data/nrlw/team_data.csv")  |> mutate(team_unique = team_name)
team_logos <- read_csv("cleaned_data/nrlw/team_logos.csv") |> rename(team_unique = team_name)
RUOK <- c("R", "U", "O", "K")

##### Helper Stats #####
player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year),
            final_team = team[max(row_number())]) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

##### Analysis #####
ruok_data <- player_data |>
  filter(first_name != "?",
        !is.na(first_name),
        nchar(first_name) > 1) |>
  select(player_id, full_name, last_name) |>
  mutate(full_name_letters = gsub("[^a-zA-Z]", "", full_name) |>
           str_to_upper() |>
           str_split(pattern = "")) |>
  rowwise() |>
  mutate(n_letters = length(full_name_letters),
         uniq_ruok = list(RUOK[which(RUOK %in% full_name_letters)]),
         ruok = list(full_name_letters[which(full_name_letters %in% RUOK)]),
         n_uniq_ruok = length(uniq_ruok),
         n_ruok = length(ruok)) |>
  filter(n_uniq_ruok == 4) |>
  mutate(collapsed_string = paste0(ruok, collapse = ""),
         once_in_order = (collapsed_string == "RUOK"),
         any_in_order = grepl("R.*U.*O.*K.*", collapsed_string)) |>
  left_join(player_career_years |> select(player_id, career_years, last_year, final_team), 
            by = "player_id") |>
  ungroup() |>
  arrange(n_letters, career_years)
  

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
              font.weight = "bold"
            ))

# Final table formatting
final_table <- ruok_data |>
  filter(last_year >= 2023) |>
  filter(!(player_id %in% c(28259, 26269))) |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("final_team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  select(-c(team_unique)) |>
  mutate(full_name = toupper(full_name),
         full_name = gsub("([RUOK]+)", "<b style='color: #000000;'>\\1</b>", full_name),
         full_name = paste0('<span style="display: block; padding: 2px 6px 2px 6px; border-radius: 4px; font-weight: bold; background-color: #fec830; color: #00000077">', full_name, '</span>'),
         team_label = paste0('<span style="display: block; padding: 2px 6px 2px 6px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', team_abbr, '</span>')
  ) |>
  arrange(final_team, last_name) |>
  select(team_label, full_name) |>
  rename(
    Team = team_label,
    `Player Name` = full_name
  ) |>
  formattable(
    list(
    ),
    align = c("c", "c"),
    table.attr = 'class=\"table table-hoverable\" style="font-size: 13.5px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 3px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-family: Montserrat; padding: 3px 0 0 0;",
                    "<b style='font-size: 20px;'>RUOK Day</b><br>Current NRLW players with all the letters of RUOK in their name" |> HTML()))
final_table

saveWidget(final_table, "tables/html/ruok_day_nrlw.html")
webshot(url = "tables/html/ruok_day_nrlw.html", 
        file = "tables/png/ruok_day_nrlw.png", 
        selector = "div", zoom = 4,
        vwidth = 400)
