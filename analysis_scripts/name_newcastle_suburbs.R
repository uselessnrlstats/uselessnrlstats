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

#extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
coach_data <- read_csv("cleaned_data/nrl/coach_data.csv")
ref_data <- read_csv("cleaned_data/nrl/ref_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
teams_data <- read_csv("cleaned_data/nrl/team_data.csv")

# font_add_google("Montserrat")
# showtext_auto(enable = TRUE)
# showtext_opts(dpi = 300)
fm <- 2

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

AUS_LGAs <- st_read("analysis_scripts/extra_data/AUS_LGAs/LGA_2023_AUST_GDA94.shp")
AUS_suburbs <- st_read("analysis_scripts/extra_data/AUS_suburbs/SAL_2021_AUST_GDA94.shp") |>
  mutate(SAL_NAME21 = gsub(" \\(.+\\)", "", SAL_NAME21)) |>
  filter(AUS_CODE21 == "AUS", SHAPE_Area > 0) |>
  mutate(LGA = st_intersects(geometry, AUS_LGAs)) |>
  rowwise() |>
  mutate(LGA = AUS_LGAs$LGA_NAME23[LGA] |> list()) 

newcastle_suburbs <- AUS_suburbs |>
  rowwise() |>
  filter("Newcastle" %in% LGA) |>
  #filter(all(LGA %in% c("Lake Macquarie", "Newcastle", "Port Stephens"))) |>
  st_transform(crs = st_crs(4326)) |>
  mutate(SAL_NAME21 = SAL_NAME21 |> tolower() |> toTitleCase())

aus_shape <- st_read("analysis_scripts/extra_data/AUS_outline/AUS_2021_AUST_GDA94.shp") |> 
  filter(AUS_CODE21 == "AUS") |>
  st_transform(crs = st_crs(4326))

##### Analysis #####
# Player Career Summaries
player_teams <- player_match_data |>
  group_by(player_id) |>
  summarise(teams = list(unique(team)))

players_in <- player_data |>
  rowwise() |>
  filter(
    any(c(full_name, first_name, last_name) %in% newcastle_suburbs$SAL_NAME21)
  ) |>
  left_join(player_teams, by = "player_id") |>
  filter(any(c("Newcastle", "Newcastle Knights", "Hunter Mariners") %in% teams))

# player_data |>
#   rowwise() |>
#   filter(
#     any(str_detect(string = tolower(first_name), pattern = tolower(newcastle_suburbs$SAL_NAME21))) |
#       any(str_detect(string = tolower(last_name), pattern = tolower(newcastle_suburbs$SAL_NAME21))) |
#       any(str_detect(string = tolower(full_name), pattern = tolower(newcastle_suburbs$SAL_NAME21)))
#   ) |>
#   left_join(player_teams, by = "player_id") |>
#   View()

player_suburbs <- newcastle_suburbs |>
  mutate(centroid = st_centroid(geometry)) |>
  rowwise() |>
  mutate(
    players = paste(
      sort(players_in$full_name[c(which(players_in$first_name == SAL_NAME21), which(players_in$last_name == SAL_NAME21))]),
      collapse = ","),
    players = gsub(SAL_NAME21, paste0("underline(bold('", SAL_NAME21, "'))"), players),
    players = gsub(" ", "~", players),
    players = ifelse(str_detect(players, ","), 
                     paste0("atop(", players, ")"),
                     players),
    named = !(players == "")
  ) |>
  ungroup()

##### Plot #####
name_map <- ggplot(player_suburbs) +
  geom_sf(aes(geometry = geometry, fill = named), col = "gray70", linewidth = 0.5) +
  geom_sf(data = st_union(player_suburbs), aes(geometry = geometry), fill = NA, col = "gray20", linewidth = 0.6) +
  scale_fill_manual(name = "<b>In a Knight's Name:</b>",
                    labels = c("No", "Yes"),
                    values = c("#0253a3", "#e92d2b"),
                    guide = guide_legend(
                      position = "bottom",
                      direction = "horizontal",
                      override.aes = list(colour = "grey20")
                    )) +
  geom_text_repel(
    data = player_suburbs |> filter(named),
    aes(geometry = centroid, label = players),
    parse = TRUE,
    family = "Montserrat",
    stat = "sf_coordinates",
    size = 3,
    lineheight = 0.8,
    min.segment.length = 0.5,
    box.padding = 0.5,
    #max.overlaps = Inf,
    force = 3,
    arrow = arrow(length = unit(0.012, "npc")),
    point.padding = unit(0.01, "npc"),
    color = "grey20",
    segment.color = "grey20",
    segment.size = 1,
    direction = "x",
    nudge_x = c(0.07),
    nudge_y = c(0.095),
    hjust = 0.5,
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
    plot.title = element_text(family = "Montserrat", size = 7*fm, colour = "gray20", face = "bold", margin = margin(b = 10), hjust = 0.5),
    plot.title.position = "panel",
    plot.background = element_blank(),
    panel.background = element_blank(),
    #plot.margin = margin(t = 2, r = -20, b = 2, l = -20),
    legend.title = element_markdown(family = "Montserrat", size = 5*fm, hjust = 0.5),
    legend.text = element_text(family = "Montserrat", size = 4*fm, margin = margin(l = 2), hjust = 0.5),
    legend.position = "bottom",
    legend.margin = margin(t = -5)
  ) +
  labs(
    title = "Newcastle suburbs in Knights player names"
  )
name_map

ggsave(filename = "plots/newcastle_map.pdf", plot = name_map, 
       device = cairo_pdf, width = 6, height = 6, units = "in")
pdf_convert("plots/newcastle_map.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/newcastle_map.png")
