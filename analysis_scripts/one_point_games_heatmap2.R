##### Description #####
# An R script to look at all one-point matches

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
}

extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####
current_teams <- match_data |>
  filter(competition_year == "NRL 2023") |>
  distinct(away_team) |>
  rename(team_name = away_team) |>
  left_join(team_data |> select(team_name, team_unique, team_mascots),
            by = c("team_name")) |>
  left_join(team_logos, by = "team_unique") |>
  arrange(team_unique)

##### Analysis #####
match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "R\\1", round),
         round = case_when(
           round %in% c("Prelim Final") ~ "PF",
           round %in% c("Semi Final") ~ "SF",
           round %in% c("Qualif Final", "Qualifier") ~ "QF",
           round %in% c("Elim Qualif") ~ "EF",
           round %in% c("Grand Final") ~ "GF",
           .default = round
         ),
         margin = abs(home_team_score - away_team_score)) |>
  filter(competition == "NRL", 
         margin == 1) |>
  mutate(
    winning_team = case_when(
      home_team_score > away_team_score ~ home_team,
      home_team_score < away_team_score ~ away_team,
      .default = NA),
    losing_team = case_when(
      home_team_score > away_team_score ~ away_team,
      home_team_score < away_team_score ~ home_team,
      .default = NA)
  ) |>
  rowwise() |>
  mutate(
    score = paste0(max(home_team_score, away_team_score),"-", 
                   min(home_team_score, away_team_score))
  ) |>
  ungroup() |>
  select(competition_year, year, match_id, round, winning_team, losing_team, score) |>
  left_join(team_data |> select(team_name, team_unique, team_mascots),
          by = c("winning_team" = "team_name")) |>
  select(-winning_team) |> 
  rename(winning_team = team_unique,
         winning_team_mascot = team_mascots) |>
  left_join(team_data |> select(team_name, team_unique, team_mascots),
            by = c("losing_team" = "team_name")) |>
  select(-losing_team) |> 
  rename(losing_team = team_unique,
         losing_team_mascot = team_mascots) |>
  filter(winning_team %in% current_teams$team_unique,
         losing_team %in% current_teams$team_unique) |>
  mutate(match = paste(year, round))
  
plot_data <- match_results |>
  arrange(year) |>
  group_by(winning_team_mascot, losing_team_mascot) |>
  summarise(
    count = n(),
    matches = paste(match, collapse = "<br>"),
    .groups = "drop"
  ) |>
  mutate(across(c(1,2), \(x) factor(x, levels = current_teams$team_mascots, ordered = TRUE))) |>
  complete(winning_team_mascot,losing_team_mascot,
           fill = list(count = 0, matches = NA)) |>
  mutate(count = factor(count, levels = 0:max(count), ordered = TRUE))

##### Plot #####
# font_add_google("Montserrat")
# showtext_auto(enable = TRUE)
fm <- 2
# showtext_opts(dpi = 300)

#  nlevels(plot_data$count) - 1
colours <- c(
  "#FFFFFF", 
  rev(
    paletteer_c(
      "viridis::mako",
      10
     )
  )[c(1,3,6,8)]
)

one_point_wins_plot <- ggplot(plot_data) +
  geom_tile(aes(x = winning_team_mascot, y = losing_team_mascot,
                fill = count), colour = "grey80", linewidth = 0.5, show.legend = TRUE) +
  geom_tile(data = current_teams |> 
              mutate(team_mascots = factor(team_mascots, levels = team_mascots, ordered = TRUE)),
            aes(x = team_mascots, y = team_mascots), fill = "grey80", colour = "grey80", linewidth = 0.5) +
  geom_image(data = current_teams |> 
               mutate(team_mascots = factor(team_mascots, levels = team_mascots, ordered = TRUE)),
             mapping = aes(x = 0, y = team_mascots, image = logo_path)) +
  geom_image(data = current_teams |> 
               mutate(team_mascots = factor(team_mascots, levels = team_mascots, ordered = TRUE)),
             mapping = aes(x = team_mascots, y = 18, image = logo_path)) +
  scale_fill_manual(values = colours,
                    guide = guide_legend(
                      title = "Matches",
                      labels = paste(" ", 0:4, " "),
                      override.aes = list(shape = 15, size = 6, alpha = 1, linewidth = 0.25)
                    ),
                    drop = FALSE) +
  scale_y_discrete(position = "left", limits = rev, expand = c(0,0), drop = FALSE) +
  scale_x_discrete(position = "top", expand = c(0,0), drop = FALSE) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = "Montserrat"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_text(family = "Montserrat", size = 8*fm, angle = 90, margin = margin(r = 20)),
    axis.title.x = element_text(family = "Montserrat", size = 8*fm, margin = margin(b = 20)),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    legend.key.width = unit(0.15*fm, "in"),
    legend.key.height = unit(0.15*fm, "in"),
    legend.ticks = element_blank(),
    legend.title = element_markdown(family = "Montserrat", size = 10*fm, face = "bold", hjust = 0),
    legend.title.position = "top",
    legend.text = element_markdown(family = "Montserrat", size = 10*fm, margin = margin(l = 5), hjust = 0.3),
    plot.title = element_text(family = "Montserrat", size = 14*fm, colour = "gray20", face = "bold", margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  labs(
    x = "Winning Team",
    y = "Losing Team",
    title = "NRL era matches with 1 point margins"
  )
one_point_wins_plot

ggsave(filename = "plots/1_point_margins2.pdf", plot = one_point_wins_plot, device = cairo_pdf,
       width = 10, height = 9, units = "in")
pdf_convert("plots/1_point_margins2.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/1_point_margins2.png")
