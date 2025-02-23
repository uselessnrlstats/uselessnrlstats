##### Description #####
# An R script to create player summaries

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(showtext)
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
  library(show_col)
  library(ggnewscale)
}

#extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, round, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, round, home_away, team, score_for, score_against, result)

team_colours <- team_data |>
  left_join(team_logos, by = "team_unique") |>
  select(team_name, team_colour) |>
  deframe()

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

##### Analysis #####
player <- 22776 # Nathan Cleary
player <- 299 # Paul Gallen
player <- 28411 # Pat Carrigan

player_match_summaries <- player_match_data |>
  filter(player_id == player) |>
  left_join(match_data |> select(match_id, competition_year, round, date), 
            by = "match_id") |>
  left_join(match_results |> select(match_id, team, result), 
            by = c("match_id", "team")) |>
  # Round, Year and Result
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "R\\1", round),
         round = case_when(
           round %in% c("Prelim Final") ~ "PF",
           round %in% c("Semi Final") ~ "SF",
           round %in% c("Qualif Final", "Qualifier") ~ "QF",
           round %in% c("Elim Qualif") ~ "EF",
           round %in% c("Grand Final") ~ "GF",
           .default = round)) |>
  mutate(tries = tries + penalty_tries,
         field_goals = field_goals + field_goals2,
         sin_bins = sin_bins + sin_bins5) |>
  select(competition_year, year, round, date, team, opposition_team, result, number, position, captain, points, tries, goals, field_goals, sin_bins, send_offs) |>
  mutate(prem = (result == "W") & (round == "GF") & !(year %in% c(2007,2009)),
         runner_up = (result == "L") & (round == "GF")) |>
  left_join(team_data |> left_join(team_logos, by = "team_unique") |> select(team_name, team_abbr, team_colour), by = c("team" = "team_name")) |>
  rename(team_col = team_colour) |>
  left_join(team_data |> left_join(team_logos, by = "team_unique") |> select(team_name, team_colour), by = c("opposition_team" = "team_name")) |>
  rename(opp_team_col = team_colour) |>
  mutate(text_col = ifelse(luminance(team_col) < 0.4, "light", "dark"),
         opp_text_col = ifelse(luminance(opp_team_col) < 0.4, "light", "dark"))

grid_size <- nrow(player_match_summaries) |> sqrt() |> ceiling()

player_career_summary <- player_match_summaries |>
  summarise(matches = n(),
            years = paste0(min(year), "-", max(year)),
            teams = paste(unique(team_abbr), collapse = ","),
            tries = sum(tries),
            goals = sum(goals),
            field_goals = sum(field_goals),
            sin_bins = sum(sin_bins),
            send_offs = sum(send_offs))

##### Plot Dataset #####
plot_data <- player_match_summaries |>
  arrange(date) |>
  mutate(grid_row = ((row_number() - 1) %/% grid_size),
         grid_col = ((row_number() - 1) %% grid_size))

##### Plot #####
ggplot(plot_data) +
  # Player team
  geom_tile(aes(x = grid_col, y = grid_row, fill = team),
            height = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = team_colours) +
  new_scale_fill() +
  # Opposition_team
  geom_tile(aes(x = grid_col, y = grid_row + 0.5, fill = opposition_team, col = result), 
            height = 0.5, linewidth = 20 / grid_size, show.legend = FALSE) +
  scale_fill_manual(values = team_colours) +
  new_scale_fill() +
  # Wins / Losses
  # geom_point(data = plot_data |> filter(!prem),
  #            aes(x = grid_col, y = grid_row + 0.45, fill = result),
  #            pch = 21, size = 50 / grid_size, stroke = 6 / grid_size, colour = alpha("grey20", 1), show.legend = FALSE) +
  scale_colour_manual(values = c("W" = "#5ECE4E", "D" = "#D87406", "L" = "#C31717")) +
  new_scale_colour() +
  # Sin Bins / Send Offs
  geom_text(data = plot_data |> filter(sin_bins > 0),
            aes(x = grid_col - 0.33, y = grid_row + 0.45, label = "\u25AE"), colour = "yellow", size = 90 / grid_size, family = "Lucida Sans Unicode") +
  geom_text(data = plot_data |> filter(sin_bins > 0),
            aes(x = grid_col - 0.33, y = grid_row + 0.45, label = "\u25AF"), colour = "grey20", size = 90 / grid_size, family = "Lucida Sans Unicode") +
  geom_text(data = plot_data |> filter(sin_bins > 1),
            aes(x = grid_col - 0.16, y = grid_row + 0.45, label = "\u25AE"), colour = "yellow", size = 90 / grid_size, family = "Lucida Sans Unicode") +
  geom_text(data = plot_data |> filter(sin_bins > 1),
            aes(x = grid_col - 0.16, y = grid_row + 0.45, label = "\u25AF"), colour = "grey20", size = 90 / grid_size, family = "Lucida Sans Unicode") +
  geom_text(data = plot_data |> filter(send_offs > 0, sin_bins == 0),
            aes(x = grid_col - 0.33, y = grid_row + 0.45, label = "\u25AE"), colour = "red", size = 90 / grid_size, family = "Lucida Sans Unicode") +
  geom_text(data = plot_data |> filter(send_offs > 0, sin_bins == 0),
            aes(x = grid_col - 0.33, y = grid_row + 0.45, label = "\u25AF"), colour = "grey20", size = 90 / grid_size, family = "Lucida Sans Unicode") +
  geom_text(data = plot_data |> filter(send_offs > 0, sin_bins > 0),
            aes(x = grid_col - 0.16, y = grid_row + 0.45, label = "\u25AE"), colour = "red", size = 90 / grid_size, family = "Lucida Sans Unicode") +
  geom_text(data = plot_data |> filter(send_offs > 0, sin_bins > 0),
            aes(x = grid_col - 0.16, y = grid_row + 0.45, label = "\u25AF"), colour = "grey20", size = 90 / grid_size, family = "Lucida Sans Unicode") +
  # Scoring
  geom_text(data = plot_data |> filter(tries > 0),
            aes(x = grid_col - 0.3, grid_row + 0.1, label = tries, colour = text_col),
            size = 40 / grid_size, family = "Montserrat", fontface = "bold", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(goals > 0),
            aes(x = grid_col, grid_row + 0.1, label = goals, colour = text_col),
            size = 40 / grid_size, family = "Montserrat", fontface = "bold", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(field_goals > 0),
            aes(x = grid_col + 0.3, grid_row + 0.1, label = field_goals, colour = text_col),
            size = 40 / grid_size, family = "Montserrat", fontface = "bold", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(tries > 0),
            aes(x = grid_col - 0.3, grid_row - 0.1, label = "T", colour = text_col),
            size = 25 / grid_size, family = "Montserrat", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(goals > 0),
            aes(x = grid_col, grid_row - 0.1, label = "G", colour = text_col),
            size = 25 / grid_size, family = "Montserrat", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(field_goals > 0),
            aes(x = grid_col + 0.3, grid_row - 0.1, label = "FG", colour = text_col),
            size = 25 / grid_size, family = "Montserrat", show.legend = FALSE) +
  scale_colour_manual(values = c("light" = "grey95", "dark" = "grey5")) +
  # Premierships / Runners-up
  geom_text(data = plot_data |> filter(prem),
            aes(x = grid_col, y = grid_row + 0.42, label = "\U0001F947", colour = opp_text_col),
            size = 60 / grid_size, family = "Segoe UI Emoji", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(runner_up),
            aes(x = grid_col, y = grid_row + 0.42, label = "\U0001F948", colour = opp_text_col),
            size = 60 / grid_size, family = "Segoe UI Emoji", show.legend = FALSE) +
  # Grid Lines
  geom_segment(data = data.frame(x = (0:grid_size) - 0.5, y = -0.25, yend = grid_size - 0.25),
               aes(x = x, xend = x, y = y, yend = yend), col = "grey20", linewidth = 35 / grid_size) +
  geom_segment(data = data.frame(y = (0:grid_size) - 0.25, x = -0.5, xend = grid_size - 0.5),
               aes(x = x, xend = xend, y = y, yend = y), col = "grey20", linewidth = 35 / grid_size) +
  scale_y_reverse(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_void() +
  theme(
    text = element_text(family = "Montserrat"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = "Montserrat", size = 10, colour = "grey90", face = "bold", margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    panel.background = element_rect(fill = "grey20"),
    plot.background = element_rect(fill = "grey20")
  ) +
  labs(
    title = "NRL Player Career Summary: ?????????"
  )

ggsave(filename = "plots/player_summaries/test_player_summary.png", 
       # device = cairo_pdf,
       width = 7, height = 7, units = "in", dpi = 1080 / 7)
# pdf_convert("plots/player_summaries/test_player_summary.pdf",
#             format = "png", dpi = 1000,
#             filenames = "plots/player_summaries/test_player_summary.png")
