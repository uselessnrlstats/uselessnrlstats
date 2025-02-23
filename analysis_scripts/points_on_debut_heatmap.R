##### Description #####
# An R script to look at a heatmap of all numbers of points on debut

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
  library(ggtext)
  library(pdftools)
  library(ggimage)
  library(paletteer)
  library(show_col)
}

#extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
match_data <- read_csv("cleaned_data/nrl/match_data.csv") |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric(),
         round = gsub("Round (\\d+)", "R\\1", round),
         round = case_when(
           round %in% c("Prelim Final", "Prelim Playoff", "Major Prelim Semi", "Minor Prelim Semi", "Minor Prelim", "Major Prelim") ~ "PF",
           round %in% c("Semi Final", "Minor Semi", "Major Semi", "Minor Semi Rep.") ~ "SF",
           round %in% c("Qualif Final", "Qualifier", "Minor Qualif", "Major Qualif") ~ "QF",
           round %in% c("Elim Qualif", "Elim Prelim Final") ~ "EF",
           round %in% c("Grand Final", "Final", "Grand Final Rep.") ~ "GF",
           round %in% c("Grand Final Chall.") ~ "GFC",
           round %in% c("Playoff") ~ "PO",
           .default = round
         ))
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Helper Stats #####

##### Analysis #####
plot_data <-  player_match_data |>
  left_join(match_data |> select(match_id, year, round, date)) |>
  arrange(date) |>
  group_by(player_id) |>
  filter(row_number() == 1) |>
  ungroup() |>
  left_join(team_data |> select(team_name, team_abbr), by = c("team" = "team_name")) |>
  left_join(team_data |> select(team_name, team_abbr) |> rename(opp_team_abbr = team_abbr), by = c("opposition_team" = "team_name")) |>
  left_join(player_data |> select(player_id, full_name), by = "player_id") |>
  select(match_id, player_id, full_name, team_abbr, opp_team_abbr, tries, goals, field_goals, field_goals2, points, year, round, date) |>
  mutate(summary = paste0(
    points, ": ", paste(full_name, year, round, team_abbr, "v", opp_team_abbr),
    " ", tries, "T ", goals, "G ", field_goals, "FG"
  )) |>
  mutate(player_no = row_number()) |>
  mutate(grid_row = ((player_no %/% 100) + 1) |> factor(),
         grid_col = (player_no %% 100) |> factor())

##### Plot #####
# font_add_google("Montserrat")
# showtext_auto(enable = TRUE)
fm <- 2
# showtext_opts(dpi = 300)

#  nlevels(plot_data$count) - 1
colours <- c(
  "#FFFFFF", 
  (
    paletteer_dynamic(
      "cartography::wine.pal",
      20
     )
  )
)

debut_points_plot <- ggplot(plot_data) +
  geom_tile(aes(x = grid_col, y = grid_row,
                fill = points), colour = NA, linewidth = 0, show.legend = TRUE) +
  scale_fill_gradientn(colours = colours,
                       guide = guide_colourbar(
                         title = "Points"
                         )
                       ) +
  scale_y_discrete(limits = rev) +
  scale_x_discrete(position = "bottom", expand = c(0,0)) +
  theme_void()
  theme(
    text = element_text(family = "Montserrat"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_text(family = "Montserrat", size = 8*fm, angle = 90, margin = margin(r = 5)),
    axis.title.x = element_text(family = "Montserrat", size = 8*fm, margin = margin(b = 5)),
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
debut_points_plot

ggsave(filename = "plots/1_point_margins.pdf", plot = one_point_wins_plot, device = cairo_pdf,
       width = 10, height = 9, units = "in")
pdf_convert("plots/1_point_margins.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/1_point_margins.png")
