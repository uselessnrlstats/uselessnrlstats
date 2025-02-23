##### Description #####
# An R script to look at Shaun Lane's across the world

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
  library(jsonlite)
  # library(showtext)
}

extrafont::loadfonts()

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
ref_data <- read_csv("cleaned_data/nrl/ref_data.csv")

fm <- 2

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

##### Analysis #####
shaun_lane <- fromJSON(txt = "analysis_scripts/extra_data/shaun_lane.json") |>
  as.data.frame() |>
  select(name, display_name, lon, lat) |>
  mutate(name = paste(name, "(street)")) |>
  rbind(data.frame(name = "Shaun Lane (person)", display_name = "Shaun Lane", lon = 151.00000746396432, lat =	-33.8087178)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

##### Plot #####
name_map <- ggplot() +
  geom_sf(data = countries50,
          aes(geometry = geometry), fill = "azure1", col = "gray10") +
  geom_sf(data = shaun_lane,
          aes(geometry = geometry, col = name), size = 3, alpha = 0.8) +
  scale_colour_manual(name = NULL,
                      values = c("dodgerblue2", "darkgoldenrod1"),
                      guide = guide_legend(
                        position = "bottom",
                        direction = "horizontal",
                        override.aes = list(alpha = 1)
                      )) +
  theme_classic() +
  theme(
    text = element_text(family = "Montserrat"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = "Montserrat", size = 5*fm, colour = "gray20", face = "bold", margin = margin(l = 30)),
    plot.title.position = "plot",
    plot.background = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(t = 2, r = -20, b = 2, l = -20),
    legend.title = element_blank(),
    legend.text = element_text(family = "Montserrat", size = 4*fm, margin = margin(t = 2), hjust = 0.5, vjust = 0.6),
    legend.position = "bottom",
    legend.margin = margin(t = -15)
  ) +
  labs(
    title = "A definitive map of known Shaun Lane's"
  )
name_map

ggsave(filename = "plots/shaun_lanes.pdf", plot = name_map, 
       device = cairo_pdf, width = 6, height = 4, units = "in")
pdf_convert("plots/shaun_lanes.pdf", 
            format = "png", dpi = 180*6,
            filenames = "plots/shaun_lanes.png")
