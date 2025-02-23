##### Description #####
# An R script to look at NRL and AFL wooden spooners having matching mascots

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
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv") |>
  rbind(tibble(team_short = "?", team_name = "?", team_unique = "?", team_mascots = "?", team_abbr = "?"))
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv") |>
  rbind(tibble(team_unique = "?", team_colour = "#ed771e", logo_path = NA))
ladder_data <- read_csv("cleaned_data/nrl/ladder_round_data.csv")
premiership_data <- read_csv("helper_stats/premiership_data.csv")

##### AFL Data #####
afl_spoons <- read_csv("analysis_scripts/extra_data/afl_wooden_spoons.csv")
afl_logos <- read_csv("analysis_scripts/extra_data/afl_clubs.csv")

##### Analysis #####
matching_spoons <- premiership_data |>
  select(competition_year, year, `Wooden Spoon`) |>
  left_join(team_data, by = c("Wooden Spoon" = "team_name")) |>
  select(competition_year, year, team_unique) |>
  rename(wooden_spoon_nrl = team_unique) |>
  right_join(afl_spoons, by = "year") |>
  filter(
    (wooden_spoon_nrl == "University" & wooden_spoon_afl == "University") |
      (wooden_spoon_nrl == "Balmain Tigers" & wooden_spoon_afl == "Richmond Tigers") |
      (wooden_spoon_nrl == "Wests Tigers" & wooden_spoon_afl == "Richmond Tigers") |
      (wooden_spoon_nrl == "Canterbury Bankstown Bulldogs" & wooden_spoon_afl == "Footscray Bulldogs") |
      (wooden_spoon_nrl == "Canterbury Bankstown Bulldogs" & wooden_spoon_afl == "Western Bulldogs") |
      (wooden_spoon_nrl == "Western Suburbs Magpies" & wooden_spoon_afl == "Collingwood Magpies") |
      (wooden_spoon_nrl == "Newtown Jets" & wooden_spoon_afl == "Essendon Bombers") |
      (wooden_spoon_nrl == "Manly Warringah Sea Eagles" & wooden_spoon_afl == "West Coast Eagles") |
      (wooden_spoon_nrl == "North Sydney Bears" & wooden_spoon_afl == "Brisbane Bears") |
      year == 2024
  ) |>
  mutate(wooden_spoon_nrl = replace_na(wooden_spoon_nrl, "Wests Tigers"))

##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

nrl_team_formatter <- 
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
                       text_col = ifelse(lum < 0.35, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

afl_team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "2px 4px 2px 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `background-color` = data.frame(team = x) |>
                left_join(afl_logos, by = "team") |>
                pull(team_colour),
              color = data.frame(team = x) |>
                left_join(afl_logos, by = "team") |>
                pull(text_colour)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold"
            ))

# Final table formatting
final_table <- matching_spoons |>
  select(wooden_spoon_nrl, year, wooden_spoon_afl) |>
  rename(
    Year = year,
    `&#x1F944;NRL Wooden Spoon` = wooden_spoon_nrl,
    `AFL Wooden Spoon&#x1F944;` = wooden_spoon_afl
  ) |>
  formattable(
    list(
      Year = bold_formatter,
      `&#x1F944;NRL Wooden Spoon` = nrl_team_formatter,
      `AFL Wooden Spoon&#x1F944;` = afl_team_formatter
      
    ),
    align = c("c", "c", "c"),
    table.attr = 'class=\"table table-hoverable\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 5px  !important;}")) |>
  prependContent(tags$style("table thead {font-size: 14px; background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}")) |>
  prependContent(tags$style("table td:nth-child(1), td:nth-child(3) { width: 220px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-family: Montserrat; font-weight: bold; padding: 3px 0 0 0;",
                    "NRL + AFL Wooden Spoons for matching mascots"))
final_table

saveWidget(final_table, "tables/html/afl_nrl_spoons2.html")
webshot(url = "tables/html/afl_nrl_spoons2.html", 
        file = "tables/png/afl_nrl_spoons2.png", 
        selector = "div", zoom = 4,
        vwidth = 600)
