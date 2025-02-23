##### Description #####
# An R script to look at players who debuted in rd1

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

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
matches_with_no_try <- player_match_data |>
  select(player_id, match_id, team, opposition_team, number, tries) |>
  left_join(match_data |> select(match_id, competition, date),
            by = "match_id") |>
  arrange(date) |>
  group_by(player_id) |>
  mutate(prior_career_tries = lag(cumsum(tries)) |> replace_na(0)) |>
  filter(prior_career_tries == 0) |>
  ungroup() |>
  filter(competition == "NRL") |>
  left_join(team_data |> select(team_name, team_unique), 
            by = c("team" = "team_name")) |>
  left_join(player_data |> select(player_id, full_name),
            by = "player_id") |>
  group_by(number) |>
  summarise(
    n_matches = n(),
    n_tries = sum(tries > 0),
    prob = n_tries / n_matches,
    .groups = "drop"
  )

##### Plotting #####
jersey_prob_plot <- matches_with_no_try |>
  filter(number <= 25) |>
  ggplot() +
  theme_classic() +
  geom_bar(aes(x = number, y = prob, fill = number), 
           stat = "identity", linewidth = 1, alpha = 0.8) +
  # geom_text(aes(x = number, y = n + 3, label = n),
  #           family = "Montserrat", size = 3, alpha = 0.5) +
  scale_fill_gradientn(colors = paletteer_d("ggthemes::Hue_Circle")[c(19, 1:18)], #c(brewer.pal(6, "Set2")),
                       guide = "none") +
  scale_x_continuous(limits = c(0.5,25.5), breaks = 1:25, expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 0.35), 
                     breaks = seq(0, 1, by = 0.05), 
                     labels = label_percent(),
                     expand = c(0.01,0)) +
  labs(
    x = "Jersey Number",
    y = "Probability of Scoring your First Try",
    title = "Probability of scoring your first try if wearing each\njersey number (NRL era)"
  ) +
  theme(
    text = element_text(family = "Montserrat"),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(family = "Montserrat", size = 12, face = "bold", margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
jersey_prob_plot

ggsave(filename = "plots/debut_try_number_prob_plots.pdf", plot = jersey_prob_plot, device = cairo_pdf,
       width = 6, height = 6, units = "in")
pdf_convert("plots/debut_try_number_prob_plots.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/debut_try_number_prob_plots.png")
