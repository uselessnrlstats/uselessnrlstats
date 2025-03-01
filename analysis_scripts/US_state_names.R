##### Description #####
# An R script to look at US states appearing in player names

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(stringr)
  library(tidyverse)
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
  library(countries)
  library(ozmaps)
  library(sf)
  library(tools)
  library(ggpp)
  library(tigris)
}

#extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
player_data <- rbind(
  read_csv("cleaned_data/nrl/player_data.csv"),
  read_csv("cleaned_data/nrlw/player_data.csv")) |>
  select(1:4)
player_match_data <- rbind(
  read_csv("cleaned_data/nrl/player_match_data.csv") |> select(-sin_bins5),
  read_csv("cleaned_data/nrlw/player_match_data.csv"))
match_data <- rbind(
  read_csv("cleaned_data/nrl/match_data.csv"),
  read_csv("cleaned_data/nrlw/match_data.csv"))
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

fm <- 2

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

US_states <- states() |>
  select(STUSPS, NAME, GEOID, geometry) |>
  filter(!(NAME %in% c("District of Columbia", "Puerto Rico", "American Samoa", "Commonwealth of the Northern Mariana Islands", "Guam", "United States Virgin Islands")))

##### Analysis #####
# Player Career Summaries
players_in <- player_data |>
  rowwise() |>
  filter(
    any(c(full_name, first_name, last_name) %in% US_states$NAME)
  )

player_data |>
  rowwise() |>
  filter(
    any(str_detect(string = tolower(first_name), pattern = tolower(US_states$NAME))) |
      any(str_detect(string = tolower(last_name), pattern = tolower(US_states$NAME))) |
      any(str_detect(string = tolower(full_name), pattern = tolower(US_states$NAME)))
  )

player_states <- US_states |>
  mutate(centroid = st_centroid(geometry)) |>
  rowwise() |>
  mutate(
    players = paste(
      sort(player_data$full_name[c(which(player_data$first_name == NAME), which(player_data$last_name == NAME))]),
      collapse = "\n"),
    named = !(players == "")
  ) |>
  ungroup() |>
  shift_geometry(geoid_column = "GEOID", preserve_area = FALSE, position = "below")

##### Plot #####
name_map <- ggplot(player_states) +
  geom_sf(aes(geometry = geometry, fill = named), col = "gray80") +
  scale_fill_manual(name = "<b>In an NRL/NRLW<br>player's name:</b>",
                    labels = c("No", "Yes"),
                    values = c("#0A3161", "#B31942"),
                    guide = guide_legend(
                      direction = "vertical",
                      position = "inside"
                    )) +
  geom_text_repel(
    data = player_states |> 
      st_drop_geometry() |> 
      select(players, named, centroid) |> 
      st_set_geometry("centroid") |>
      st_transform(crs = st_crs(player_states)) |>
      filter(named),
    aes(geometry = centroid, label = players),
    #parse = TRUE,
    family = "Montserrat",
    stat = "sf_coordinates",
    size = 4.5,
    lineheight = 0.9,
    min.segment.length = 0,
    box.padding = 0.8,
    #max.overlaps = Inf,
    force = 3,
    arrow = arrow(length = unit(0.012, "npc")),
    point.padding = unit(0.01, "npc"),
    color = "grey10",
    segment.size = 1,
    direction = "x",
    nudge_x = c(-300000, -500000, 200000),
    nudge_y = c(300000, -1500000, 500000),
    hjust = c(1, 0.5, 0.5),
    vjust = 0.5
  ) +
  theme_classic() +
  #xlim(c(146.15, 147.12)) + ylim(c(-19.78, -18.92)) +
  theme(
    text = element_text(family = "Montserrat"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    # plot.title = element_text(family = "Montserrat", size = 7*fm, colour = "gray20", face = "bold", margin = margin(b = 10)),
    # plot.title.position = "panel",
    plot.background = element_blank(),
    panel.background = element_blank(),
    #plot.margin = margin(t = 2, r = -20, b = 2, l = -20),
    legend.background = element_blank(),
    legend.title = element_markdown(family = "Montserrat", size = 5*fm, hjust = 0),
    legend.text = element_text(family = "Montserrat", size = 4*fm, margin = margin(l = 2), hjust = 1),
    legend.position.inside = c(0.1, 0.3),
    legend.margin = margin(t = -5)
  )
  # labs(
  #   title = "US States in NRL/NRLW player names"
  # )
name_map

ggsave(filename = "plots/us_states_map.pdf", plot = name_map, 
       device = cairo_pdf, width = 8, height = 5, units = "in")
pdf_convert("plots/us_states_map.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/us_states_map.png")
