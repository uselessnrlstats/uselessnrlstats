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
  # library(showtext)
}

extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
ref_data <- read_csv("cleaned_data/nrl/ref_data.csv")

# font_add_google("Montserrat")
# showtext_auto(enable = TRUE)
# showtext_opts(dpi = 300)
fm <- 2

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

##### Analysis #####
player_data |>
  rowwise() |>
  filter(
    any(c(full_name, first_name, last_name) %in% rnaturalearthdata::countries50$geounit)
  )

player_data |>
  rowwise() |>
  filter(
    any(str_detect(tolower(first_name), tolower(rnaturalearthdata::countries50$geounit))) |
      any(str_detect(tolower(last_name), tolower(rnaturalearthdata::countries50$geounit))) |
      any(str_detect(tolower(full_name), tolower(rnaturalearthdata::countries50$geounit)))
  )

player_countries <- countries50 |>
  mutate(centroid = st_centroid(geometry)) |>
  rowwise() |>
  mutate(
    players = paste(
      sort(player_data$full_name[c(which(player_data$first_name == geounit), which(player_data$last_name == geounit))]),
      collapse = "\n"),
    named = !(players == "")
  ) |>
  ungroup()

##### Plot #####
name_map <- ggplot(player_countries) +
  geom_sf(aes(geometry = geometry, fill = named), col = "gray10") +
  scale_fill_manual(name = "<b>In an NRL Player's Name:</b>",
                    labels = c("No", "Yes"),
                    values = c("azure1", "deepskyblue2"),
                    guide = guide_legend(
                      position = "bottom",
                      direction = "horizontal"
                    )) +
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
