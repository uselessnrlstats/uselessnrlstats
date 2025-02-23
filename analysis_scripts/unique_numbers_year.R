##### Description #####
# An R script to look at unique jersey numbers worn in a year

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
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####

# unique jersey numbers
unique_jersey_seasons <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  count(player_id, year, team, number) |>
  rename(number_matches = n) |>
  group_by(player_id, year) |>
  arrange(number) |>
  summarise(n_numbers = length(unique(number)),
            unique_numbers = paste0(unique(number), collapse = " "),
            n_matches = sum(number_matches),
            team = paste(unique(team), collapse = ", "),
            .groups = "drop") |>
  left_join(player_data |> select(player_id, full_name), 
            by = "player_id") |>
  arrange(desc(n_numbers), n_matches)


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
                       text_col = ifelse(lum < 0.35, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

number_formatter <-
  formatter("span",
            style = x ~ style(
              font.size = "12px"
            ))

# Final table formatting
final_table <- unique_jersey_seasons |>
  filter(row_number() %in% c(1:3)) |>
  select(year, full_name, team, n_matches, n_numbers, unique_numbers) |>
  rbind(data.frame(year = "", 
                   full_name = paste0("+",
                                      c(unique_jersey_seasons |> filter(n_numbers == 9) |> nrow() - 1,
                                        unique_jersey_seasons |> filter(n_numbers == 8) |> nrow())),
                   team = "",
                   n_matches = "",
                   n_numbers = c(9, 8),
                   unique_numbers = "")) |>
  rename(
    Year = year,
    Player = full_name,
    Team = team,
    `Matches` = n_matches,
    `# Numbers` = n_numbers,
    `Unique Jersey Numbers` = unique_numbers
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Player = bold_formatter,
      `# Numbers` = bold_formatter
    ),
    align = c("r", "c", "r", "c", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica; vertical-align: middle"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-family: Helvetica;",
                    "\u2003Players with the most unique jersey numbers in a season"))
final_table

saveWidget(final_table, "tables/html/unique_numbers_year.html")
webshot(url = "tables/html/unique_numbers_year.html", 
        file = "tables/png/unique_numbers_year.png", 
        selector = "body", zoom = 4,
        vwidth = 800)
