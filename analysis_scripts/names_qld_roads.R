##### Description #####
# An R script to look at player names in roads

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
  # library(showtext)
}

extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
ref_data <- read_csv("cleaned_data/nrl/ref_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")

fm <- 2

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

bris_roads_raw <- st_read("analysis_scripts/extra_data/QLD_roads/Queensland_roads_and_tracks.shp") |>
  st_transform(crs = st_crs(4326)) |>
  filter(lga_name_l == "Brisbane City" | lga_name_r == "Brisbane City")

bris_roads <- bris_roads_raw |>
  select(road_name_, road_id, road_name, road_type, road_name1, locality_l, locality_r, geometry) |>
  group_by(road_name_, road_id, road_name, road_type, road_name1) |>
  summarise() |>
  filter(!is.na(road_type))

bris_lga <- st_read("analysis_scripts/extra_data/AUS_LGAs/LGA_2023_AUST_GDA94.shp") |>
  filter(LGA_CODE23 == 31000)
bris_mainland <- bris_lga |>
  st_cast(to = "POLYGON") |>
  arrange(desc(st_area(geometry))) |>
  filter(row_number() <= 2)

bris_mainland_bbox <- st_bbox(bris_mainland)

aus_shape <- st_read("analysis_scripts/extra_data/AUS_outline/AUS_2021_AUST_GDA94.shp") |> 
  filter(AUS_CODE21 == "AUS") |>
  st_transform(crs = st_crs(4326))

##### Helper Stats #####
# Player Career year Span
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
players_in <- player_data |>
  filter(first_name != "?",
         !is.na(first_name),
         nchar(first_name) > 1) |>
  rowwise() |>
  filter(
    any(c(full_name, first_name, last_name) %in% bris_roads$road_name)
  ) |>
  ungroup() |>
  select(player_id, first_name, last_name, full_name) |>
  left_join(player_career_years, by = "player_id") |>
  filter(last_year >= 2023)

# player_data |>
#   rowwise() |>
#   filter(
#     any(str_detect(string = tolower(first_name), pattern = tolower(tv_suburbs$SUB_NAME))) |
#       any(str_detect(string = tolower(last_name), pattern = tolower(tv_suburbs$SUB_NAME))) |
#       any(str_detect(string = tolower(full_name), pattern = tolower(tv_suburbs$SUB_NAME)))
#   ) |>
#   left_join(player_teams, by = "player_id")

player_streets <- bris_roads |>
  #mutate(centroid = st_centroid(geometry)) |>
  rowwise() |>
  mutate(
    players = paste(
      sort(players_in$full_name[c(which(players_in$first_name == road_name), which(players_in$last_name == road_name))]),
      collapse = ","),
    named = !(players == "")
  ) |>
  ungroup()

##### Plot #####
name_map <- ggplot(player_streets) +
  geom_sf(aes(geometry = geometry, colour = named,), linewidth = 0.2) +
  scale_colour_manual(name = NULL,
                      labels = c("No", "Yes"),
                      values = c("plum4", "palegreen1"),
                      guide = guide_legend(
                        direction = "vertical",
                        override.aes = list(linewidth = 6, fill = c("plum4", "palegreen1"))
                      )) +
  theme_classic() +
  xlim(bris_mainland_bbox[c(1,3)]) + ylim(bris_mainland_bbox[c(2,4)]) +
  theme(
    text = element_text(family = "Montserrat", colour = "grey90"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = "Montserrat", size = 7*fm, face = "bold", margin = margin(t = -10, l = 2)),
    plot.title.position = "panel",
    plot.background = element_rect(fill = "gray10", colour = "gray10"),
    panel.background = element_rect(fill = "gray10", colour = "gray10"),
    plot.margin = margin(t = -5, r = -2, b = -5, l = -2),
    legend.background = element_blank(),
    legend.title = element_markdown(family = "Montserrat", size = 5*fm, hjust = 0.6),
    legend.text = element_text(family = "Montserrat", size = 4*fm, margin = margin(l = 2), hjust = 0.5),
    legend.text.position = "right",
    legend.position = "inside",
    legend.position.inside = c(0.1,0.1),
    legend.margin = margin(t = -5)
  ) +
  labs(
    title = "Brisbane streets with 2023-24 NRL player names"
  )
name_map

ggsave(filename = "plots/brisbane_street_map.pdf", plot = name_map, 
       device = cairo_pdf, width = 6, height = 5.75, units = "in")
pdf_convert("plots/brisbane_street_map1.pdf", 
            format = "png", dpi = 180*6,
            filenames = "plots/brisbane_street_map.png")
