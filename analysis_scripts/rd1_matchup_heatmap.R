##### Description #####
# An R script to look at all round one matchups

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(ggrepel)
  library(ggtext)
  library(RColorBrewer)
  library(pdftools)
  remotes::install_github("wilkelab/gridtext")
  library(gridtext)
  library(pdftools)
  library(ggimage)
  library(paletteer)
}

#extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
current_teams <- match_data |>
  filter(competition_year == "NRL 2024") |>
  distinct(away_team) |>
  rename(team_name = away_team) |>
  left_join(team_data |> select(team_name, team_unique, team_mascots),
            by = c("team_name")) |>
  left_join(team_logos, by = "team_unique") |>
  arrange(team_unique)

##### Analysis #####
rd1_matches <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "Rd\\1", round)) |>
  select(competition_year, year, match_id, round, home_team, away_team) |>
  left_join(team_data |> select(team_name, team_unique),
            by = c("home_team" = "team_name")) |>
  select(-home_team) |> 
  rename(team1 = team_unique) |>
  left_join(team_data |> select(team_name, team_unique),
            by = c("away_team" = "team_name")) |>
  select(-away_team) |> 
  rename(team2 = team_unique) |>
  filter(team1 %in% current_teams$team_unique,
         team2 %in% current_teams$team_unique) |>
  filter(round == "Rd1")

plot_data <- rbind(
  rd1_matches,
  rd1_matches |> rename(team2 = team1, team1 = team2)
) |>
  #filter(year >= 1998) |>
  arrange(year) |>
  group_by(team1, team2) |>
  summarise(
    count = n(),
    years = paste(year, collapse = "<br>"),
    .groups = "drop"
  ) |>
  mutate(across(c(1,2), \(x) factor(x, levels = current_teams$team_unique, ordered = TRUE))) |>
  complete(team1, team2,
           fill = list(count = 0, matches = NA)) |>
  mutate(match_ind = (count > 0))

##### Plot #####
fm <- 2

rd1_matchups_plot <- ggplot(plot_data) +
  geom_tile(aes(x = team1, y = team2,
                fill = match_ind), colour = "grey90", linewidth = 0.5, show.legend = TRUE) +
  geom_tile(data = current_teams |> 
              mutate(team_unique = factor(team_unique, levels = team_unique, ordered = TRUE)),
            aes(x = team_unique, y = team_unique), fill = "grey90", colour = "grey90", linewidth = 0.5) +
  geom_tile(data = rd1_2025,
            aes(x = team1, y = team2),
            colour = "#1874CD", fill = NA, linewidth = 1) +
  geom_image(data = current_teams |> 
               mutate(team_unique = factor(team_unique, levels = team_unique, ordered = TRUE)),
             mapping = aes(x = 0, y = team_unique, image = logo_path)) +
  geom_image(data = current_teams |> 
               mutate(team_unique = factor(team_unique, levels = team_unique, ordered = TRUE)),
             mapping = aes(x = team_unique, y = 18, image = logo_path)) +
  scale_fill_manual(values = c("white", "#54C9AD"),
                    guide = "none",
                    drop = FALSE) +
  scale_y_discrete(position = "left", limits = rev, expand = c(0,0), drop = FALSE) +
  scale_x_discrete(position = "top", expand = c(0,0), drop = FALSE) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = "Montserrat"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_markdown(family = "Montserrat", size = 10*fm, colour = "gray20", face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_markdown(family = "Montserrat", size = 6*fm, colour = "gray20", face = "bold", margin = margin(b = 20)),
    plot.title.position = "plot",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "NSWRL/NRL Rd1 Match-ups",
    subtitle = "<span style = 'color: #54C9AD;'>Teal matches</span> have occurred before in a NSWRL/NRL Rd1<br>
    <span style = 'color: #1874CD;'>Blue matches</span> are to be played in Rd1 2025"
  )
#rd1_matchups_plot

ggsave(filename = "plots/rd1_matchups1.pdf", plot = rd1_matchups_plot, device = cairo_pdf,
       width = 9, height = 9, units = "in")
pdf_convert("plots/rd1_matchups1.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/rd1_matchups1.png")
