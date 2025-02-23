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

# font_add_google("Montserrat")
# showtext_auto(enable = TRUE)
# showtext_opts(dpi = 300)
fm <- 2

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

tv_suburbs <- st_read("analysis_scripts/extra_data/TCC_suburbs/TCC_Suburbs_GDA2020.shp") |>
  st_transform(crs = st_crs(4326)) |>
  mutate(SUB_NAME = SUB_NAME |> tolower() |> toTitleCase())

aus_shape <- st_read("analysis_scripts/extra_data/AUS_outline/AUS_2021_AUST_GDA94.shp") |> 
  filter(AUS_CODE21 == "AUS") |>
  st_transform(crs = st_crs(4326))

##### Helper Stats #####
# Player Career Summaries
player_teams <- player_match_data |>
  group_by(player_id) |>
  summarise(teams = list(unique(team)))

##### Analysis #####
players_in <- player_data |>
  rowwise() |>
  filter(
    any(c(full_name, first_name, last_name) %in% tv_suburbs$SUB_NAME)
  ) |>
  left_join(player_teams, by = "player_id") |>
  filter("North Queensland Cowboys" %in% teams)

# player_data |>
#   rowwise() |>
#   filter(
#     any(str_detect(string = tolower(first_name), pattern = tolower(tv_suburbs$SUB_NAME))) |
#       any(str_detect(string = tolower(last_name), pattern = tolower(tv_suburbs$SUB_NAME))) |
#       any(str_detect(string = tolower(full_name), pattern = tolower(tv_suburbs$SUB_NAME)))
#   ) |>
#   left_join(player_teams, by = "player_id")

player_suburbs <- tv_suburbs |>
  mutate(centroid = st_centroid(geometry)) |>
  rowwise() |>
  mutate(
    players = paste(
      sort(players_in$full_name[c(which(players_in$first_name == SUB_NAME), which(players_in$last_name == SUB_NAME))]),
      collapse = ","),
    players = gsub(SUB_NAME, paste0("underline(bold('", SUB_NAME, "'))"), players),
    players = gsub(" ", "~", players),
    players = ifelse(str_detect(players, ","), 
                     paste0("atop(", players, ")"),
                     players),
    named = !(players == "")
  ) |>
  ungroup()

##### Plot #####
name_map <- ggplot(player_suburbs) +
  geom_sf(aes(geometry = geometry, fill = named), col = "gray10") +
  scale_fill_manual(name = "<b>In a Cowboy's Name:</b>",
                    labels = c("No", "Yes"),
                    values = c("#ffdf00", "#001641"),
                    guide = guide_legend(
                      position = "bottom",
                      direction = "horizontal"
                    )) +
  geom_text_repel(
    data = player_suburbs |> filter(named),
    aes(geometry = centroid, label = players),
    parse = TRUE,
    family = "Montserrat",
    stat = "sf_coordinates",
    size = 3.5,
    lineheight = 0.9,
    min.segment.length = 0.5,
    box.padding = 0.5,
    #max.overlaps = Inf,
    force = 3,
    arrow = arrow(length = unit(0.012, "npc")),
    point.padding = unit(0.01, "npc"),
    color = "grey30",
    direction = "x",
    nudge_x = c(-0.05,-0.37,-0.3,0.22),
    nudge_y = c(0.2,0.05,-0.1,0.17),
    hjust = 0.5,
    vjust = 0.5
  ) +
  theme_classic() +
  xlim(c(146.15, 147.12)) + ylim(c(-19.78, -18.92)) +
  theme(
    text = element_text(family = "Montserrat"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = "Montserrat", size = 7*fm, colour = "gray20", face = "bold", margin = margin(b = 10)),
    plot.title.position = "panel",
    plot.background = element_blank(),
    panel.background = element_blank(),
    #plot.margin = margin(t = 2, r = -20, b = 2, l = -20),
    legend.title = element_markdown(family = "Montserrat", size = 5*fm, hjust = 0.6),
    legend.text = element_text(family = "Montserrat", size = 4*fm, margin = margin(l = 2), hjust = 0.5),
    legend.position = "bottom",
    legend.margin = margin(t = -5)
  ) +
  labs(
    title = "Townsville suburbs in Cowboys player names"
  )
name_map

ggsave(filename = "plots/townsville_map.pdf", plot = name_map, 
       device = cairo_pdf, width = 6, height = 6, units = "in")
pdf_convert("plots/townsville_map.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/townsville_map.png")
