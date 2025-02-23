##### Description #####
# An R script to look at player win-rates always being below 0.5

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(scales)
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

extrafont::loadfonts()

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
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

player_career_years <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  group_by(player_id) |>
  summarise(first_year = min(year),
            last_year = max(year)) |>
  mutate(career_years = if_else(
    first_year == last_year,
    paste(first_year),
    paste0(first_year, "-", last_year)
  ))

##### Analysis #####
win_rates <- player_match_data |>
  select(player_id, match_id, team) |>
  left_join(match_results |> select(match_id, date, team, result), 
            by = c("match_id", "team")) |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(
    match_no = row_number(),
    wins = cumsum(result == "W"),
    draws = cumsum(result == "D"),
    losses = cumsum(result == "L"),
    win_rate = wins / match_no,
    final_win_rate = max(wins) / max(match_no)) |>
  filter(all(win_rate > 0.5)) |>
  arrange(desc(match_no), match_no) |>
  #filter(max(match_no) >= 100) |>
  ungroup() |>
  left_join(player_data |> select(player_id, full_name, last_name), by = "player_id") |>
  left_join(player_career_years |> select(player_id, last_year), by = "player_id") |>
  filter(last_year == 2024) |>
  select(player_id, full_name, last_name, last_year, team, match_no, win_rate, final_win_rate) |>
  arrange(final_win_rate) |>
  mutate(full_name = factor(full_name, levels = unique(full_name)),
         rank = as.numeric(full_name)) |>
  filter(player_id %in% unique(player_id)[1:30] | full_name == "Kodi Nikorima") |>
  group_by(player_id) |>
  mutate(label = paste0(ordinal(rank), ": ", full_name, " (", max(match_no), ifelse(last_year == 2024, "*", ""), " @ ", formattable::percent(final_win_rate, 2), ")"),
         label = ifelse(last_name == "Nikorima", paste0("<b>", label, "</b>"), label)) |>
  ungroup()

# 476 Michael Buettner
# 10917 John McMartin
# 1375 Mark Minichello
# 533 Jeff Hardy
# 17283 Tyson Frizell

##### Plotting #####
win_rate_plot <- ggplot(win_rates) +
  theme_classic() +
  geom_segment(x = 0, y = 0.50, xend = 330, yend = 0.50, col = "red", linetype = 2) +
  geom_line(aes(x = match_no, y = win_rate, colour = full_name, alpha = (full_name == "Kodi Nikorima")), linewidth = 1) +
  scale_colour_manual(values = c('#AA3377', '#4477AA', '#228833','#CCBB44', '#EE6677'), #c(brewer.pal(6, "Set2")),
                      labels = unique(win_rates$label),
                      guide = guide_legend(
                        title = "Player (games @ win-rate)",
                        direction = "vertical",
                        position = "inside"
                      )) +
  scale_alpha_manual(values = c(0.5, 1),
                     guide = "none") +
  scale_x_continuous(limits = c(0,330), expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0.45,1), expand = c(0.01,0),
                     breaks = seq(0, 1, by = 0.1),
                     labels = label_percent()) +
  labs(
    x = "Game Number",
    y = "Career Win Rate",
    title = "Lowest final NSWRL/NRL win rate with career win rate\nalways >50% (min 100 games)"
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    panel.grid.major = element_blank(),
    # legend.key.width = unit(0.15*fm, "in"),
    legend.key.height = unit(0.2, "in"),
    legend.ticks = element_blank(),
    legend.title = element_markdown(family = "Montserrat", face = "bold", size = 9),
    legend.text = element_markdown(family = "Montserrat", size = 7),
    axis.title = element_markdown(family = "Montserrat", face = "bold", size = 9),
    axis.text = element_markdown(family = "Montserrat", size = 7),
    legend.position.inside = c(0.75, 0.75),
    plot.title = element_text(family = "Montserrat", size = 12, face = "bold", margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
win_rate_plot

ggsave(filename = "plots/lowest_win_rate_above_0.5_plot.pdf", plot = win_rate_plot, device = cairo_pdf,
       width = 5, height = 5, units = "in")
pdf_convert("plots/lowest_win_rate_above_0.5_plot.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/lowest_win_rate_above_0.5_plot.png") 
