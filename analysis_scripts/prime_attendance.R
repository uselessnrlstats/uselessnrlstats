##### Description #####
# An R script to look at matches with a prime attendance number

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(primes)
}

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
match_data |>
  mutate(prime_attendance = is_prime(crowd)) |>
  filter(prime_attendance) |>
  arrange(desc(c))
  
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
final_table <- average_jersey |>
  filter(row_number() <= 10) |>
  left_join(player_number_counts, by = "player_id") |>
  select(full_name, career_years, total_matches, number_summary, number_total, av_number, av_num_1pi) |>
  mutate(
    av_num_1pi = ifelse(av_number > pi, av_num_1pi, -av_num_1pi),
    av_number = digits(av_number, 6),
    av_num_1pi = digits(av_num_1pi, 6)) |>
  rename(
    `Player` = full_name,
    Career = career_years,
    `# Matches` = total_matches,
    `Jersey Number (Matches)` = number_summary,
    `Jersey Total` = number_total,
    `Jersey Average` = av_number,
    `Diff from Pi` = av_num_1pi
  ) |>
  formattable(
    list(
      `Player Name` = bold_formatter,
      `Diff from Pi` = bold_formatter
    ),
    align = c("r", "c", "r", "l", "r", "r", "r"),
    table.attr = 'class=\"table table-striped\" style="font-size: 12px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(h4(class = "title", 
                    style = "font-weight: bold; font-family: Roboto; margin-left: 25px;",
                    "World \u03C0 Day")) |>
  prependContent(h5(class = "subtitle",
                    style = "font-family: Roboto Regular; margin-left: 25px;",
                    "NRL/NSWRL players with the closest average career jersey number to \u03C0 = 3.14159... "))
final_table

saveWidget(final_table, "tables/html/average_jersey_number.html")
webshot(url = "tables/html/average_jersey_number.html", 
        file = "tables/png/average_jersey_number.png", 
        selector = "body", zoom = 4,
        vwidth = 850)
