##### Description #####
# An R script tracking the number of 30+ scores in a round

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot)
  library(ggrepel)
  library(ggtext)
  library(RColorBrewer)
  library(pdftools)
  remotes::install_github("wilkelab/gridtext")
  library(gridtext)
  library(ggtext)
  library(pdftools)
  library(ggimage)
  library(paletteer)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
# Match Results for each team
matchups_by_team <- player_match_data |>
  select(match_id, team, opposition_team) |>
  distinct()

match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, date, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  left_join(matchups_by_team, by = c("match_id", "team")) |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, date, round, home_away, team, opposition_team, score_for, score_against, result)

##### Analysis #####
rounds_30 <- match_results |>
  filter(!is.na(score_for)) |>
  group_by(competition_year, round) |> 
  summarise(n_matches = length(unique(match_id)), 
            n_30_scores = sum(score_for >= 30),
            min_date = min(date),
            .groups = "drop") |> 
  arrange(desc(n_30_scores), min_date) |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round),
         round = paste0(year, " ", round)) |>
  select(round, n_matches, n_30_scores)

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
final_table <- rounds_30 |>
  filter(n_30_scores >= 8) |>
  rbind(
    rounds_30 |> 
      count(n_30_scores) |> 
      rename(round = n) |> 
      arrange(desc(n_30_scores)) |>
      filter(n_30_scores %in% 6:7) |> 
      mutate(round = paste0("\u00D7", round),
             n_matches = "-") |>
      select(round, n_matches, n_30_scores)
  ) |>
  rename(
    Round = round,
    `# Matches<br>in Round` = n_matches,
    `# 30+ Scores<br>in Round` = n_30_scores
  ) |>
  formattable(
    list(
      Round = bold_formatter,
      `# 30+ Scores<br>in Round` = bold_formatter
    ),
    align = c("c", "c", "c"),
    table.attr = 'class=\"table table-hoverable\" style="font-size: 15px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 4px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-child(1) td, tr:nth-child(3) td, tr:nth-child(4) td {background-color: #ffd6f0;}")) |>
  # prependContent(tags$style("table td:nth-child(4) { width: 320px;}")) |>
  prependContent(h5(class = "title",
                    style = "text-align: center; font-size: 18px; font-weight: bold; font-family: Montserrat; padding: 3px 0 0 0;",
                    "Most 30+ scores in an NRL/NSWRL round"))
final_table

saveWidget(final_table, "tables/html/round_30_scores.html")
webshot(url = "tables/html/round_30_scores.html", 
        file = "tables/png/round_30_scores.png", 
        selector = "div", zoom = 4,
        vwidth = 400)

##### Other stats
match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         total_points = home_team_score + away_team_score,
         margin = abs(home_team_score - away_team_score)) |>
  filter(!is.na(total_points)) |>
  group_by(year) |>
  summarise(average_points = mean(total_points),
            average_margin = mean(margin),
            .groups = "drop") |>
  ggplot() +
  geom_point(aes(x = year, y = average_margin)) +
  theme_bw()


fm <- 2

av_plots <- match_data |>
  left_join(player_match_data |> 
              group_by(match_id) |>
              summarise(total_tries = sum(tries) + sum(penalty_tries),
                        .groups = "drop"),
            by = "match_id") |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         total_points = home_team_score + away_team_score,
         margin = abs(home_team_score - away_team_score)) |>
  filter(!is.na(total_points)) |>
  group_by(year) |>
  summarise(`Average Points` = mean(total_points),
            `Average Tries` = mean(total_tries),
            `Average Margin` = mean(margin),
            .groups = "drop") |>
  pivot_longer(cols = 2:4, names_to = "stat", values_to = "average") |>
  mutate(stat = factor(stat, levels = c("Average Points", "Average Tries", "Average Margin"))) |>
  filter(year >= 1998) |>
  ggplot() +
  geom_line(aes(x = year, y = average, col = stat)) +
  geom_point(aes(x = year, y = average, col = stat)) +
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                      guide = "none") +
  facet_wrap(facets = vars(stat), nrow = 3, scales = "free_y") +
  theme_bw() +
  labs(
    title = "NRL era (1998-) per-game averages by season",
    x = "Year",
    y = NULL
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    #axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(family = "Montserrat", size = 5*fm, margin = margin(l = 2), hjust = 0.5),
    axis.title.x = element_text(family = "Montserrat", size = 6*fm, margin = margin(l = 2), hjust = 0.5),
    panel.grid.minor = element_blank(),
    plot.title = element_text(family = "Montserrat", size = 8*fm, colour = "gray20", face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.title.position = "panel",
    plot.background = element_blank(),
    panel.background = element_blank(),
    strip.background = element_rect(fill = "#120b2f"),
    strip.text = element_text(family = "Montserrat", size = 6*fm, face = "bold", colour = "#ffffff", hjust = 0.5),
    plot.margin = margin(t = 5, b = 5, r = 7, l = 5),
  )
av_plots

ggsave(filename = "plots/av_plots.pdf", plot = av_plots, 
       device = cairo_pdf, width = 8, height = 6, units = "in")
pdf_convert("plots/av_plots.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/av_plots.png")
