##### Description #####
# An R script to look at unique names scoring on debut

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
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv") |>
  mutate(position = case_when(
    position == "FB" ~ "Fullback",
    position == "W" ~ "Wing",
    position == "C" ~ "Centre",
    position == "FE" ~ "5/8th",
    position == "HB" ~ "Halfback",
    position == "FR" ~ "Prop",
    position == "HK" ~ "Hooker",
    position == "2R" ~ "2nd Row",
    position == "L" ~ "Lock",
    position == "B" ~ "Bench"
  ),
  position = factor(position, levels = c("Fullback", "Wing", "Centre", "5/8th", "Halfback", "Hooker", "Prop", "2nd Row", "Lock", "Bench"))
  )
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
out_of_position <- player_match_data |>
  select(match_id, team, player_id, position) |>
  left_join(match_data |> select(match_id, competition_year), by = "match_id") |>
  filter(competition_year == "NRL 2024") |>
  group_by(player_id, position) |>
  summarise(team = paste0(unique(team), collapse = " | "),
            pos_matches = n(),
            .groups = "drop") |>
  group_by(player_id) |>
  mutate(tot_matches = sum(pos_matches)) |>
  ungroup() |>
  mutate(perc_matches = pos_matches / tot_matches) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(player_id, full_name, team, position, pos_matches, tot_matches, perc_matches) |>
  arrange(perc_matches) |>
  group_by(position) |>
  filter(
    (position == "Fullback" & (row_number() %in% c(1) | perc_matches %in% c(perc_matches[1]))) |
      (position == "Wing" & (row_number() %in% c(1:2) | perc_matches %in% c(perc_matches[1:2]))) |
      (position == "Centre" & (row_number() %in% c(1:2) | perc_matches %in% c(perc_matches[1:2]))) |
      (position == "5/8th" & (row_number() %in% c(1) | perc_matches %in% c(perc_matches[1]))) |
      (position == "Halfback" & (row_number() %in% c(1) | perc_matches %in% c(perc_matches[1]))) |
      (position == "Prop" & (row_number() %in% c(1:2) | perc_matches %in% c(perc_matches[1:2]))) |
      (position == "Hooker" & (row_number() %in% c(1) | perc_matches %in% c(perc_matches[1]))) |
      (position == "2nd Row" & (row_number() %in% c(1:2) | perc_matches %in% c(perc_matches[1:2]))) |
      (position == "Lock" & (row_number() %in% c(1) | perc_matches %in% c(perc_matches[1]))) |
      (position == "Bench" & (row_number() %in% c(1:4) | perc_matches %in% c(perc_matches[1:4])))
  ) |>
  arrange(position, perc_matches)
  

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

# Final table formatting
final_table <- out_of_position |>
  left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("team" = "team_name")) |>
  left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
  select(-c(team_unique)) |>
  mutate(
    n_matches_label = paste0(pos_matches, "/", tot_matches),
    player_label = paste0('<span style="display: block; padding: 0 4px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', full_name, '</span>'),
    team_label = paste0('<span style="display: block; padding: 1px 6px 1px 6px; border-radius: 4px; font-weight: bold; background-color:', team_colour, '; color:', ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"), '">', team_abbr, '</span>')
  ) |>
  filter(!(full_name == "Lindsay Smith" & position == "2nd Row") &
           !(full_name == "Shaun Lane"& position == "Lock") &
           !(full_name %in% c("Aidan Sezer", "Josh Curran", "Trent Loiero", "Erin Clark", "Sam McIntyre"))) |>
  group_by(position) |>
  
  mutate(position_unique = paste0(position, ifelse(row_number() == 1, "", row_number())) |>
           factor(levels = c("Fullback", "Wing", "Centre", "Centre2", "Wing2", "5/8th", "Halfback", "Prop", "Hooker", "Prop2", "2nd Row", "2nd Row2", "Lock", "Bench", "Bench2", "Bench3", "Bench4", "Bench5"))) |>
  arrange(position_unique) |>
  ungroup() |>
  mutate(number = paste0(row_number(), "."),
         frac_matches = paste0(pos_matches, " / ", tot_matches)) |>
  
  # summarise(
  #   team_labels = paste0(team_label, collapse = '<p style="line-height:4px; margin:0px;"><br></p>'),
  #   players = paste0(full_name, collapse = '<p style="line-height:4px; margin:0px;"><br></p>'),
  #   pos_matches_labels = paste0(pos_matches, collapse = '<p style="line-height:4px; margin:0px;"><br></p>'),
  #   .groups = "drop"
  # ) |>
  # mutate(players = paste0("<b>", players, "</b>")) |>
  # rename(
  #   `Pos.` = position,
  #   Team = team_labels,
  #   Players = players,
  #   `Matches in Pos./<br># Matches` = matches_labels
  # ) |>
  # formattable(
  #   list(
  #     `Pos.` = bold_formatter
  #   ),
  #   align = c("l", "c", "l", "c"),
  #   table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  # ) |>
  
  select(position, team_label, number, full_name, frac_matches) |>
  rename(
    `Pos.` = position,
    ` ` = number,
    Team = team_label,
    Player = full_name,
    `# Matches<br>in Pos.` = frac_matches
  ) |>
  formattable(
    list(
      ` ` = bold_formatter,
      Player = bold_formatter
    ),
    align = c("r", "c", "r", "l", "c"),
    table.attr = 'class=\"table table-hoverable\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #ffffff;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style(paste0("table tr:nth-child(14) td {border-top: 1px solid #120b2f;}"))) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 18px; font-weight: bold; font-family: Montserrat; padding: 6px 0 0 0;",
                    "2024 NRL Out-of-Position XVIII"))
final_table

saveWidget(final_table, "tables/html/out_of_position24.html")
webshot(url = "tables/html/out_of_position24.html", 
        file = "tables/png/out_of_position24.png", 
        selector = "div", zoom = 4,
        vwidth = 460)
