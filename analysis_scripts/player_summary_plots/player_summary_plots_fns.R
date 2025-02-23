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
  library(patchwork)
  library(ggpubr)
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

id <- 22776 # Nathan Cleary
id <- 299 # Paul Gallen
id <- 28411 # Pat Carrigan


##### Plotting Function #####
whoami_plot <- function(id, top_align_x = c(4.5,5.5,8.5), top_align_y = c(-0.65, -0.375)) {
  bg_colour <- "grey20"
  light_colour <- "grey95"
  
  # Summarise each match for the player
  player_match_summaries <- player_match_data |>
    filter(player_id == id) |>
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
  
  # Calculate grid size
  grid_size <- nrow(player_match_summaries) |> sqrt() |> ceiling()
  
  # Produce career summary stats
  pcs <- player_match_summaries |>
    summarise(matches = n(),
              years = ifelse(min(year) == max(year), min(year), paste0(min(year), "<b>-</b>", max(year))),
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
              send_offs = sum(send_offs)) |>
    mutate(teams = gsub("(\\w{3} \\w{3} \\w{3} \\w{3}) ", "\\1<br>", teams, perl = T))
  
  # Produce career summary labels
  pcs_labels <- c(
    paste0("<b>",pcs$teams, "</b>"),
    paste0("(", pcs$years, ")"),
    paste0("<b>Pld:</b> ", pcs$matches, " (", pcs$w_d_l, ")"),
    paste0("<b>Pts:</b> ", pcs$points, " (", pcs$t_g_fg, ")"),
    paste0("<span style='font-family:Arial Unicode MS; color:#FFFF00;'>\u25AE</span><b>SB:</b> ", pcs$sin_bins),
    paste0("<span style='font-family:Arial Unicode MS; color:#FF0000;'>\u25AE</span><b>SO:</b> ", pcs$send_offs)
  )
  
  # Create dataset for plotting
  plot_data <- player_match_summaries |>
    arrange(date) |>
    mutate(grid_row = ((row_number() - 1) %/% grid_size),
           grid_col = ((row_number() - 1) %% grid_size),
           new_year = (year != lag(year)) |> replace_na(TRUE)) 
  
  # Create the plot
  whoami <- ggplot(plot_data) +
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
              aes(x = grid_col + 0.3, y = grid_row + 0.25, label = "\u25AF"), colour = bg_colour, size = 40 / grid_size, family = "Arial Unicode MS") +
    geom_text(data = plot_data |> filter(sin_bins > 1, field_goals == 0),
              aes(x = grid_col + 0.3, y = grid_row + 0.47, label = "\u25AE"), colour = "yellow", size = 40 / grid_size, family = "Arial Unicode MS") +
    geom_text(data = plot_data |> filter(sin_bins > 1, field_goals == 0),
              aes(x = grid_col + 0.3, y = grid_row + 0.47, label = "\u25AF"), colour = bg_colour, size = 40 / grid_size, family = "Arial Unicode MS") +
    geom_text(data = plot_data |> filter(send_offs > 0, sin_bins == 0, field_goals == 0),
              aes(x = grid_col + 0.3, y = grid_row + 0.25, label = "\u25AE"), colour = "red", size = 40 / grid_size, family = "Arial Unicode MS") +
    geom_text(data = plot_data |> filter(send_offs > 0, sin_bins == 0, field_goals == 0),
              aes(x = grid_col + 0.3, y = grid_row + 0.25, label = "\u25AF"), colour = bg_colour, size = 40 / grid_size, family = "Arial Unicode MS") +
    geom_text(data = plot_data |> filter(send_offs > 0, sin_bins > 0, field_goals == 0),
              aes(x = grid_col + 0.3, y = grid_row + 0.47, label = "\u25AE"), colour = "red", size = 40 / grid_size, family = "Arial Unicode MS") +
    geom_text(data = plot_data |> filter(send_offs > 0, sin_bins > 0, field_goals == 0),
              aes(x = grid_col + 0.3, y = grid_row + 0.47, label = "\u25AF"), colour = bg_colour, size = 40 / grid_size, family = "Arial Unicode MS") +
    # SB / SO if FG also kicked
    geom_text(data = plot_data |> filter(sin_bins > 0, field_goals > 0),
              aes(x = grid_col + 0.3, y = grid_row - 0.03, label = "\u25AE"), colour = "yellow", size = 40 / grid_size, family = "Arial Unicode MS") +
    geom_text(data = plot_data |> filter(sin_bins > 0, field_goals > 0),
              aes(x = grid_col + 0.3, y = grid_row - 0.03, label = "\u25AF"), colour = bg_colour, size = 40 / grid_size, family = "Arial Unicode MS") +
    geom_text(data = plot_data |> filter(send_offs > 0, field_goals > 0),
              aes(x = grid_col + 0.3, y = grid_row - 0.03, label = "\u25AE"), colour = "red", size = 40 / grid_size, family = "Arial Unicode MS") +
    geom_text(data = plot_data |> filter(send_offs > 0, field_goals > 0),
              aes(x = grid_col + 0.3, y = grid_row - 0.03, label = "\u25AF"), colour = bg_colour, size = 40 / grid_size, family = "Arial Unicode MS") +
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
                 aes(x = x, xend = x, y = y, yend = yend), col = bg_colour, linewidth = 45 / grid_size) +
    geom_segment(data = data.frame(y = (0:grid_size) - 0.25, x = -0.5, xend = grid_size - 0.5),
                 aes(x = x, xend = xend, y = y, yend = y), col = bg_colour, linewidth = 45 / grid_size) +
    # Years
    geom_text(data = plot_data |> filter(new_year),
              aes(x = grid_col - 0.5, y = grid_row + 0.25, label = str_replace_all(year, "|", "\n")),
              size = 35 / grid_size, family = "Montserrat", colour = light_colour, lineheight = 0.8, hjust = 0.5, show.legend = FALSE) +
    # Summary Info
    geom_richtext(
      data = data.frame(
        x = rep(top_align_x, each = 2) * grid_size/10 - 0.5,
        y = rep(top_align_y, times = 3) * grid_size / 10 - 0.1,
        label = pcs_labels[1:6]),
      aes(x = x, y = y, label = label),
      fill = NA, label.colour = NA, 
      size = c(ifelse(pcs$n_teams > 4, 3, 3.5),3.5,3.5,3.5,3.5,3.5), hjust = c(0.5,0.5,0,0,0,0), lineheight = c(0.2,0,0,0,0,0),
      text.colour = light_colour, family = "Montserrat") +
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
      plot.title = element_text(family = "Montserrat", size = 18, colour = light_colour, face = "bold", margin = margin(b = -10, t = 5)),
      plot.title.position = "plot",
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      panel.background = element_rect(fill = bg_colour, colour = NA),
      plot.background = element_rect(fill = bg_colour, colour = NA)
    )  +
    labs(
      title = "NRL Who am I?"
    )
    # # Logo
    # geom_image(data = data.frame(
    #             x = ifelse(grid_size^2 == pcs$matches, grid_size - 0.75, grid_size - 0.75),
    #             y = ifelse(grid_size^2 == pcs$matches, -0.5, -0.5) * grid_size / 10 - 0.1,
    #             image = "UNS_logos/UNS_watermark_light.png"),
    #            aes(x = x, y = y, image = image),
    #            size = 0.04)
  
  show(whoami)
  print(player_data |> filter(player_id == id) |> pull(full_name))
  
  ggsave(filename = "plots/player_summaries/test_player_summary.png", 
         plot = whoami, # device = cairo_pdf,
         width = 7, height = 7, units = "in", dpi = 1080)
}

##### Info Page #####
info_page <- function(id) {
  # Set up
  dark_colour <- "grey5"
  bg_colour <- "grey20"
  light_colour <- "grey95"
  
  description <- "Guess the NRL player from their career summary! Each tile in the grid represents every NRL match the player played in, from their debut (top-left) to final game (bottom-right). The colours of the tiles indicate the player's team and opposition for that match, and whether they won, lost or drew. Scoring and foul play info is included if applicable, and the beginning of each season is labelled in the grid by year."
  
  rainbow_order <- (match_data |>
                      select(away_team) |>
                      rename(team_name = away_team) |>
                      distinct() |>
                      left_join(team_data, by = "team_name") |>
                      left_join(team_logos, by = "team_unique") |>
                      select(team_unique, team_colour) |>
                      distinct() |>
                      arrange(team_unique))[c(7,13,20,21,22,6,15,1,23,29,11,19,5,8,4,34,9,26,28,2,3,37,30,31,32,24,35,12,33,16,17,25,18,27,14,36),]
  
  # Produce summary of teams involved
  teams_played <- player_match_data |>
    filter(player_id == id) |>
    pivot_longer(cols = c(team, opposition_team), names_to = "type", values_to = "team_name") |>
    select(team_name) |>
    distinct() |>
    left_join(team_data |> select(team_name, team_unique, team_abbr), by = c("team_name")) |>
    left_join(team_logos |> select(team_unique, team_colour), by = "team_unique") |>
    mutate(text_col = ifelse(luminance(team_colour) < 0.4, light_colour, dark_colour),
           team_unique = factor(team_unique, levels = rainbow_order$team_unique)) |>
    arrange(team_unique) |>
    mutate(tile_x = 0.15,
           text_x = 0.35) |>
    select(-team_name) |> 
    distinct()
  
  ##### Produce patchwork #####
  teams_segment <- ggplot(teams_played) +
    geom_tile(aes(x = tile_x, y = team_unique), 
              fill = teams_played$team_colour, colour = bg_colour, 
              linewidth = 0.5, width = 0.3) +
    geom_text(aes(x = text_x, y = team_unique, label = team_unique |> str_wrap(width = 20)), 
              family = "Montserrat", fontface = "bold", colour = light_colour, 
              hjust = 0, size = ifelse(nrow(teams_played) <= 17, 3.3, 4.4 - nrow(teams_played) / 15), 
              lineheight = 0.85) +
    coord_cartesian(clip = "off") +
    scale_y_discrete(limits = rev, expand = c(0,0)) +
    scale_x_continuous(limits = c(0,2), expand = c(0,0)) +
    theme_void() +
    theme(
      text = element_text(family = "Montserrat"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  #ggtitle(~""*underline(bold("Teams")))
  teams_segment
  
  description_segment <- ggplot() +
    annotate("text_box", x = 0, y = 0,
             label = description,
             box.colour = NA, colour = light_colour, fill = NA,
             family = "Montserrat", size = 3.2, halign = 0, valign = 0, hjust = 0, vjust = 0,
             width = 1) +
    annotate("segment", x = 0, xend = 2, y = 0, yend = 0,
             colour = light_colour, linewidth = 1) +
    coord_cartesian(clip = "off") +
    scale_y_continuous(expand = c(0,0), limits = c(-0.1,0.4)) +
    scale_x_continuous(expand = c(0,0), limits = c(0,2)) +
    theme_void() +
    theme(
      plot.margin = margin(0,0,0,0),
    )
  description_segment
  
  example_segment <- ggplot() +
    # Demo
    annotate("tile", x = 0, y = 0, height = 0.5, fill = "#1e90ff", col = bg_colour, linewidth = 5) +
    annotate("tile", x = 0, y = 0.4, height = 0.75, fill = "#e6b123") +
    annotate("tile", x = 0, y = 0.07, height = 0.15, fill = "#5ECE4E") +
    annotate("text", x = 0.3, y = 0.25, label = c("\u25AE", "\u25AF"), colour = c("yellow", bg_colour), size = 26, family = "Arial Unicode MS") +
    annotate("text", x = 0.3, y = 0.47, label = c("\u25AE", "\u25AF"), colour = c("red", bg_colour), size = 26, family = "Arial Unicode MS") +
    annotate("text", x = c(-0.28,0,-0.28,0), y = c(0.45,0.45,0.24,0.24), label = c("1", "4", "T", "G"),
             size = c(30,30,18,18), family = "Montserrat", fontface = "bold", colour = bg_colour) +
    annotate("segment", x = c(-0.5, 0.5), xend = c(-0.5, 0.5), y = -0.25, yend = 0.75, col = bg_colour, linewidth = 27) +
    annotate("segment", x = -0.5, xend = 0.5, y = c(-0.25, 0.75), yend = c(-0.25, 0.75), col = bg_colour, linewidth = 27) +
    # Labels
    annotate("curve", x = -0.35, xend = -0.4, y = 0.075, yend = -0.29, curvature = -0.52, col = light_colour, linewidth = 2, lineend = "round") +
    annotate("richtext", x = -0.38, y = -0.29, fill = NA, label.colour = NA, size = 3.5, hjust = 0, vjust = 0.8, lineheight = 1, text.colour = light_colour, family = "Montserrat",
             label = "<b>RESULT<b><br><b style='color:#5ECE4E;'>WIN</b>,<br><b style='color:#D87406;'>DRAW</b> or <br><b style='color:#C31717;'>LOSS</b>") +
    annotate("curve", x = -0.1, xend = -0.09, y = -0.1, yend = -0.29, curvature = -0.1, col = light_colour, linewidth = 2, lineend = "round") +
    annotate("richtext", x = -0.08, y = -0.29, fill = NA, label.colour = NA, size = 3.5, hjust = 0, vjust = 0.5, lineheight = 1, text.colour = light_colour, family = "Montserrat",
             label = "<b>TEAM</b> (e.g. <b style='color:#1e90ff;'>GCT</b>)") +
    annotate("curve", x = 0.15, xend = 0.05, y = 0.20, yend = -0.2, curvature = -0.2, col = light_colour, linewidth = 2, lineend = "round") +
    annotate("richtext", x = 0.07, y = -0.2, fill = NA, label.colour = NA, size = 3.5, hjust = 0, vjust = 0.5, lineheight = 1, text.colour = light_colour, family = "Montserrat",
             label = "<b>OPPOSITION</b> (e.g. <b style='color:#e6b123;'>PAR</b>)") +
    annotate("segment", x = -0.33, xend = 0.1, y = 0.57, yend = 0.57, col = light_colour, linewidth = 2, lineend = "round") +
    annotate("curve", x = -0.115, xend = -0.2, y = 0.57, yend = 0.71, curvature = -0.2, col = light_colour, linewidth = 2, lineend = "round") +
    annotate("richtext", x = -0.21, y = 0.71, fill = NA, label.colour = NA, size = 3.5, hjust = 1, vjust = 0.8, lineheight = 1, text.colour = light_colour, family = "Montserrat",
             label = "<b>SCORING</b><br>Tries (T), Goals (G)<br>Field Goals (FG)") +
    annotate("segment", x = 0.22, xend = 0.38, y = 0.61, yend = 0.61, col = light_colour, linewidth = 2, lineend = "round") +
    annotate("curve", x = 0.3, xend = 0.23, y = 0.61, yend = 0.71, curvature = -0.1, col = light_colour, linewidth = 2, lineend = "round") +
    annotate("richtext", x = 0.22, y = 0.71, fill = NA, label.colour = NA, size = 3.5, hjust = 1, vjust = 0.8, lineheight = 1, text.colour = light_colour, family = "Montserrat",
             label = "<b>FOUL PLAY</b><br>Sin Bins <span style='font-family:Arial Unicode MS; color:#FFFF00;'>\u25AE</span><br>Send Offs <span style='font-family:Arial Unicode MS; color:#FF0000;'>\u25AE</span>") +
    coord_fixed(clip = "off") +
    scale_y_reverse(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    theme_void() +
    theme(
      plot.margin = margin(t = 2),
    )
  example_segment
  
  info_page <- example_segment + teams_segment + description_segment +
    plot_layout(design = c(
      area(t = 4, b = 16, l = 4, r = 10),
      area(t = 2, b = min(16, max(4, nrow(teams_played))), l = 1, r = 3),
      area(t = 1, b = 3, l = 4, r = 10)
    )) +
    plot_annotation(
      title = "NRL Who am I?", 
      theme = theme(plot.title = element_text(family = "Montserrat", size = 18, colour = light_colour, face = "bold", margin = margin(b = -20, t = 5, l = -10)))
    ) &
    theme(
      panel.background = element_rect(fill = bg_colour, colour = NA),
      plot.background = element_rect(fill = bg_colour, colour = NA),
      plot.margin =  margin(t = 10, r = 10, b = 10, l = 10)
    )
  info_page
  
  ggsave(filename = "plots/player_summaries/test_player_info.png", 
         plot = info_page, # device = cairo_pdf,
         width = 7, height = 7, units = "in", dpi = 1080)
}

##### Plotting #####
id <- 28411
whoami_plot(id)
info_page(id)

##### Player Summaries #####
player_match_data |> 
  left_join(team_data |> select(team_name, team_abbr), by = c("team" = "team_name")) |> 
  group_by(player_id) |> left_join(match_data |> select(match_id, date), by = "match_id") |> 
  summarise(matches = n(), 
            teams = length(unique(team_abbr)),
            debut = min(year(date))) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id")  -> summaries

