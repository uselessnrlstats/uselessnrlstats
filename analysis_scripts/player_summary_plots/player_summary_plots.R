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

pcs <- player_match_summaries |>
  summarise(matches = n(),
            years = paste0(min(year), "<b>-</b>", max(year)),
            w_d_l = paste0(c(sum(result == "W"), "<b style='font-size:8pt;'>W</b> ", 
                             sum(result == "D"), "<b style='font-size:8pt;'>D</b> ",
                             sum(result == "L"), "<b style='font-size:8pt;'>L</b>"), collapse = ""),
            teams = paste(unique(team_abbr), collapse = " "),
            n_teams = length(unique(team_abbr)),
            points = sum(points),
            tries = sum(tries),
            goals = sum(goals),
            field_goals = sum(field_goals),
            t_g_fg = paste0(
              c(paste0(tries, "<b style='font-size:8pt;'>T</b>"), 
                paste0(goals, "<b style='font-size:8pt;'>G</b>"), 
                paste0(field_goals, "<b style='font-size:8pt;'>FG</b>"))[which(c(tries, goals, field_goals) > 0)],
              collapse = " "),
            sin_bins = sum(sin_bins),
            send_offs = sum(send_offs))
#  mutate(teams = gsub("(\\w{3},\\w{3}),", "\\1<br>", teams, perl = T))

pcs_labels <- c(
  paste0("<b>",pcs$teams, "</b>"),
  paste0("(", pcs$years, ")"),
  paste0("<b>Pld:</b> ", pcs$matches, " (", pcs$w_d_l, ")"),
  paste0("<b>Pts:</b> ", pcs$points, " (", pcs$t_g_fg, ")"),
  paste0("<span style='font-family:Arial Unicode MS; color:#FFFF00;'>\u25AE</span><b>SB:</b> ", pcs$sin_bins),
  paste0("<span style='font-family:Arial Unicode MS; color:#FF0000;'>\u25AE</span><b>SO:</b> ", pcs$send_offs)
)
##### Plot Dataset #####
plot_data <- player_match_summaries |>
  arrange(date) |>
  mutate(grid_row = ((row_number() - 1) %/% grid_size),
         grid_col = ((row_number() - 1) %% grid_size),
         new_year = (year != lag(year)) |> replace_na(TRUE)) 

##### Plot #####
ggplot(plot_data) +
  # Player team
  geom_tile(aes(x = grid_col, y = grid_row, fill = team),
            height = 0.5, show.legend = FALSE, colour = NA) +
  scale_fill_manual(values = team_colours) +
  new_scale_fill() +
  # Opposition_team
  geom_tile(aes(x = grid_col, y = grid_row + 0.4, fill = opposition_team), 
            height = 0.75, show.legend = FALSE, colour = NA) +
  scale_fill_manual(values = team_colours) +
  new_scale_fill() +
  # Wins / Losses
  geom_tile(aes(x = grid_col, y = grid_row + 0.07, fill = result), 
            height = 0.15, colour = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("W" = "#5ECE4E", "D" = "#D87406", "L" = "#C31717")) +
  new_scale_fill() +
  # Sin Bins / Send Offs
  geom_text(data = plot_data |> filter(sin_bins > 0, field_goals == 0),
            aes(x = grid_col + 0.3, y = grid_row + 0.25, label = "\u25AE"), colour = "yellow", size = 40 / grid_size, family = "Arial Unicode MS") +
  geom_text(data = plot_data |> filter(sin_bins > 0, field_goals == 0),
            aes(x = grid_col + 0.3, y = grid_row + 0.25, label = "\u25AF"), colour = "grey20", size = 40 / grid_size, family = "Arial Unicode MS") +
  geom_text(data = plot_data |> filter(sin_bins > 1, field_goals == 0),
            aes(x = grid_col + 0.3, y = grid_row + 0.47, label = "\u25AE"), colour = "yellow", size = 40 / grid_size, family = "Arial Unicode MS") +
  geom_text(data = plot_data |> filter(sin_bins > 1, field_goals == 0),
            aes(x = grid_col + 0.3, y = grid_row + 0.47, label = "\u25AF"), colour = "grey20", size = 40 / grid_size, family = "Arial Unicode MS") +
  geom_text(data = plot_data |> filter(send_offs > 0, sin_bins == 0, field_goals == 0),
            aes(x = grid_col + 0.3, y = grid_row + 0.25, label = "\u25AE"), colour = "red", size = 40 / grid_size, family = "Arial Unicode MS") +
  geom_text(data = plot_data |> filter(send_offs > 0, sin_bins == 0, field_goals == 0),
            aes(x = grid_col + 0.3, y = grid_row + 0.25, label = "\u25AF"), colour = "grey20", size = 40 / grid_size, family = "Arial Unicode MS") +
  geom_text(data = plot_data |> filter(send_offs > 0, sin_bins > 0, field_goals == 0),
            aes(x = grid_col + 0.3, y = grid_row + 0.47, label = "\u25AE"), colour = "red", size = 40 / grid_size, family = "Arial Unicode MS") +
  geom_text(data = plot_data |> filter(send_offs > 0, sin_bins > 0, field_goals == 0),
            aes(x = grid_col + 0.3, y = grid_row + 0.47, label = "\u25AF"), colour = "grey20", size = 40 / grid_size, family = "Arial Unicode MS") +
  # SB / SO if FG also kicked
  geom_text(data = plot_data |> filter(sin_bins > 0, field_goals > 0),
            aes(x = grid_col + 0.3, y = grid_row - 0.03, label = "\u25AE"), colour = "yellow", size = 40 / grid_size, family = "Arial Unicode MS") +
  geom_text(data = plot_data |> filter(sin_bins > 0, field_goals > 0),
            aes(x = grid_col + 0.3, y = grid_row - 0.03, label = "\u25AF"), colour = "grey20", size = 40 / grid_size, family = "Arial Unicode MS") +
  geom_text(data = plot_data |> filter(send_offs > 0, field_goals > 0),
            aes(x = grid_col + 0.3, y = grid_row - 0.03, label = "\u25AE"), colour = "red", size = 40 / grid_size, family = "Arial Unicode MS") +
  geom_text(data = plot_data |> filter(send_offs > 0, field_goals > 0),
            aes(x = grid_col + 0.3, y = grid_row - 0.03, label = "\u25AF"), colour = "grey20", size = 40 / grid_size, family = "Arial Unicode MS") +
  #Scoring
  geom_text(data = plot_data |> filter(tries > 0),
            aes(x = grid_col - 0.28, grid_row + 0.45, label = tries, colour = opp_text_col),
            size = 40 / grid_size, family = "Montserrat", fontface = "bold", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(goals > 0),
            aes(x = grid_col, grid_row + 0.45, label = goals, colour = opp_text_col),
            size = 40 / grid_size, family = "Montserrat", fontface = "bold", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(field_goals > 0),
            aes(x = grid_col + 0.28, grid_row + 0.45, label = field_goals, colour = opp_text_col),
            size = 40 / grid_size, family = "Montserrat", fontface = "bold", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(tries > 0),
            aes(x = grid_col - 0.28, grid_row + 0.24, label = "T", colour = opp_text_col),
            size = 25 / grid_size, family = "Montserrat", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(goals > 0),
            aes(x = grid_col, grid_row + 0.24, label = "G", colour = opp_text_col),
            size = 25 / grid_size, family = "Montserrat", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(field_goals > 0),
            aes(x = grid_col + 0.28, grid_row + 0.24, label = "FG", colour = opp_text_col),
            size = 25 / grid_size, family = "Montserrat", show.legend = FALSE) +
  scale_colour_manual(values = c("light" = "grey95", "dark" = "grey5")) +
  # Premierships / Runners-up
  geom_text(data = plot_data |> filter(prem),
            aes(x = grid_col, y = grid_row, label = "\U0001F947", colour = opp_text_col),
            size = 60 / grid_size, family = "Segoe UI Emoji", show.legend = FALSE) +
  geom_text(data = plot_data |> filter(runner_up),
            aes(x = grid_col, y = grid_row, label = "\U0001F948", colour = opp_text_col),
            size = 60 / grid_size, family = "Segoe UI Emoji", show.legend = FALSE) +
  # Grid Lines
  geom_segment(data = data.frame(x = (0:grid_size) - 0.5, y = -0.25, yend = grid_size - 0.25),
               aes(x = x, xend = x, y = y, yend = yend), col = "grey20", linewidth = 45 / grid_size) +
  geom_segment(data = data.frame(y = (0:grid_size) - 0.25, x = -0.5, xend = grid_size - 0.5),
               aes(x = x, xend = xend, y = y, yend = y), col = "grey20", linewidth = 45 / grid_size) +
  # Years
  geom_text(data = plot_data |> filter(new_year),
            aes(x = grid_col - 0.5, y = grid_row + 0.25, label = str_replace_all(year, "|", "\n")),
            size = 35 / grid_size, family = "Montserrat", colour = "white", lineheight = 0.8, hjust = 0.5, show.legend = FALSE) +
  # Summary Info
  geom_richtext(
    data = data.frame(
      x = (c(4,4,5.5,5.5,8,8) + 0.5*(pcs$n_teams > 4)) * grid_size/10,
      y = c(-0.65, -0.4, -0.65, -0.4, -0.65, -0.4) * grid_size / 10,
      label = pcs_labels[1:6]),
    aes(x = x, y = y, label = label),
    fill = NA, label.colour = NA, size = c(3.5), hjust = c(0.5,0.5,0,0,0,0),
    text.colour = "white", family = "Montserrat") +
  # Theming
  coord_cartesian(clip = "off") +
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
    plot.title = element_text(family = "Montserrat", size = 18, colour = "white", face = "bold", margin = margin(b = -10, t = 5)),
    plot.title.position = "plot",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    panel.background = element_rect(fill = "grey20", colour = NA),
    plot.background = element_rect(fill = "grey20", colour = NA)
  )  +
  labs(
    title = "NRL Who am I?"
  )

ggsave(filename = "plots/player_summaries/test_player_summary.png", 
       # device = cairo_pdf,
       width = 7, height = 7, units = "in", dpi = 1080 / 7)
# pdf_convert("plots/player_summaries/test_player_summary.pdf",
#             format = "png", dpi = 1000,
#             filenames = "plots/player_summaries/test_player_summary.png")
