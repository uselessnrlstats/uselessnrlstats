##### Description #####
# An R script to look at countries appearing in player names

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
  library(rnaturalearthdata)
  library(sf)
}

extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
ref_data <- read_csv("cleaned_data/nrl/ref_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")

fm <- 2

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

# Shapefiles
AUS_LGAs <- st_read("analysis_scripts/extra_data/AUS_LGAs/LGA_2023_AUST_GDA94.shp")
AUS_suburbs <- st_read("analysis_scripts/extra_data/AUS_suburbs/SAL_2021_AUST_GDA94.shp") |>
  mutate(SAL_NAME21 = gsub(" \\(.+\\)", "", SAL_NAME21)) |>
  filter(AUS_CODE21 == "AUS", SHAPE_Area > 0) |>
  mutate(LGA = st_intersects(geometry, AUS_LGAs)) |>
  rowwise() |>
  mutate(LGA = AUS_LGAs$LGA_NAME23[LGA] |> list()) 

townsville_suburbs <- AUS_suburbs |>
  rowwise() |>
  filter("Townsville" %in% LGA)

##### Helper Stats #####
# Player Career Summaries
player_teams <- player_match_data |>
  group_by(player_id) |>
  summarise(teams = list(unique(team)))

##### Analysis #####
players_in <- player_data |>
  rowwise() |>
  filter(
    any(c(full_name, first_name, last_name) %in% townsville_suburbs$SAL_NAME21)
  ) |>
  left_join(player_teams, by = "player_id") |>
  filter("North Queensland Cowboys" %in% teams)

player_suburbs <- townsville_suburbs |>
  mutate(centroid = st_centroid(geometry)) |>
  rowwise() |>
  mutate(
    players = paste(
      sort(players_in$full_name[c(which(players_in$first_name == SAL_NAME21), which(players_in$last_name == SAL_NAME21))]),
      collapse = "\n"),
    named = !(players == "")
  ) |>
  ungroup()

##### Plot #####
name_map <- ggplot(player_suburbs) +
  geom_sf(aes(geometry = geometry, fill = named), col = "gray10") +
  scale_fill_manual(name = "<b>In an NRL Player's Name:</b>",
                    labels = c("No", "Yes"),
                    values = c("azure1", "deepskyblue2"),
                    guide = guide_legend(
                      position = "bottom",
                      direction = "horizontal"
                    )) +
  xlim(c(145.5,147.5)) + ylim(c(-21, -18.5))
  geom_text_repel(
    data = player_countries |> filter(named),
    aes(geometry = centroid, label = players),
    family = "Montserrat",
    stat = "sf_coordinates",
    size = 3,
    lineheight = 0.9,
    min.segment.length = 0.3,
    box.padding = 0.5,
    max.overlaps = Inf,
    force = 0,
    arrow = arrow(length = unit(0.012, "npc")),
    nudge_x = c(-5, 15, 40, 10, -30, -20, -30),
    nudge_y = c(40, -15, -70, 60, -15, 20, -50),
    point.padding = unit(0, "npc"),
    color = "grey30",
    hjust = 0.5,
    vjust = c(0.5, 0.5, 0, 0.5, 0.5, 0.5, 0)
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Montserrat"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = "Montserrat", size = 7*fm, colour = "gray20", face = "bold", margin = margin(b = 10)),
    plot.title.position = "panel",
    plot.background = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(t = 2, r = -20, b = 2, l = -20),
    legend.title = element_markdown(family = "Montserrat", size = 5*fm, hjust = 0.6),
    legend.text = element_text(family = "Montserrat", size = 4*fm, margin = margin(l = 2), hjust = 0.5),
    legend.position = "bottom",
    legend.margin = margin(t = -5)
  ) +
  labs(
    title = "         Countries that are also NRL player first- or surnames"
  )
name_map

ggsave(filename = "plots/name_map.pdf", plot = name_map, 
       device = cairo_pdf, width = 10, height = 7, units = "in")
pdf_convert("plots/name_map.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/name_map.png")
