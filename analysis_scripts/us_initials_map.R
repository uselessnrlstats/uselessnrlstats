##### Description #####
# An R script to look at Vegas 2025 player initials = US state abbreviations

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
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot)
  library(scales)
}

#extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv") |>
  mutate(text_colour = ifelse(luminance(team_colour) < 0.4, "#F2F2F2", "#1A1A1A"))

US_states <- us_map(regions = "states")

US_states <- states() |>
  select(STUSPS, NAME, GEOID, geometry) |>
  filter(!(NAME %in% c("District of Columbia", "Puerto Rico", "American Samoa", "Commonwealth of the Northern Mariana Islands", "Guam", "United States Virgin Islands")))
##### Helper Stats #####
player_initials <- player_data |>
  filter(first_name != "?") |>
  mutate(first_name_initial = substr(first_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = rev(LETTERS), ordered = TRUE),
         last_name_initial = substr(last_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE),
         initials = paste0(first_name_initial, last_name_initial)) |>
  select(player_id, full_name, initials)

##### Analysis #####
lv_raiders_players <- data.frame(team = "Canberra Raiders", caps_name = c("Kaeo WEEKES", "Savelio TAMALE", "Matthew TIMOKO", "Sebastian KRIS", "Xavier SAVAGE", "Ethan STRANGE", "Jamal FOGARTY", "Corey HORSBURGH", "Tom STARLING", "Joseph TAPINE", "Hudson YOUNG", "Zac HOSKING", "Morgan SMITHIES", "Owen PATTIE", "Simi SASAGI", "Josh PAPALII", "Ata MARIOTA", "Danny LEVI", "Trey MOONEY", "Ethan SANDERS", "Chevy STEWART", "Jed STUART"))
lv_wahs_players <- data.frame(team = "New Zealand Warriors", caps_name = c("Charnze NICOLL-KLOKSTAD", "Taine TUAUPIKI", "Ali LEIATAUA", "Adam POMPEY", "Roger TUIVASA-SHECK", "Chanel HARRIS-TAVITA", "Luke METCALF", "James FISHER-HARRIS", "Wayde EGAN", "Mitchell BARNETT", "Kurt CAPEWELL", "Marata NIUKORE", "Erin CLARK", "Dylan WALKER", "Jackson FORD", "Demitric VAIMAUGA", "Selumiela HALASIMA", "Samuel HEALEY", "Te Maire MARTIN", "Eddie IEREMIA-TOEAVA", "Bunty AFOA", "Edward KOSI"))
lv_panthers_players <- data.frame(team = "Penrith Panthers", caps_name = c("Dylan EDWARDS", "Casey McLEAN", "Izack TAGO", "Luke GARNER", "Paul ALAMOTI", "Jack COLE", "Nathan CLEARY", "Moses LEOTA", "Mitch KENNY", "Lindsay SMITH", "Scott SORENSEN", "Liam MARTIN", "Isaah YEO", "Daine LAURIE", "Isaiah PAPALI'I", "Matt EISENHUTH", "Luron PATEA", "Brad SCHNEIDER", "Mavrik GEYER", "Jesse McLEAN", "Luke SOMMERTON", "Blaize TALAGI"))
lv_sharks_players <- data.frame(team = "Cronulla Sutherland Sharks", caps_name = c("William KENNEDY", "Samuel STONESTREET", "Jesse RAMIEN", "Kayal IRO", "Ronaldo MULITALO", "Braydon TRINDALL", "Nicho HYNES", "Addin FONUA-BLAKE", "Blayke BRAILEY", "Oregon KAUFUSI", "Briton NIKORA", "Teig WILTON", "Cameron McINNES", "Daniel ATKINSON", "Siosifa TALAKAI", "Braden HAMLIN-UELE", "Thomas HAZELTON", "Tuku HAU TAPUHA", "Mawene HIROTI", "Billy BURNS", "Jayden BERRELL", "Hohepa PURU"))

vegas_2025_players <- rbind(lv_raiders_players, lv_wahs_players, lv_panthers_players, lv_sharks_players) |>
  mutate(full_name_lower = tolower(caps_name)) |>
  select(-caps_name)
  
vegas_initials <- player_data |>
  mutate(full_name_lower = tolower(full_name)) |>
  left_join(vegas_2025_players, by = c("full_name_lower")) |>
  filter(!is.na(team)) |>
  select(player_id, team) |>
  left_join(player_initials, by = "player_id")

##### Plot #####
plot_data <- US_states |>
  left_join(vegas_initials, by = c("abbr" = "initials")) |>
  mutate(centroid = st_centroid(geom)) |>
  rowwise() |>
  mutate(
    label = full_name
  ) |>
  ungroup()

fm <- 2

name_map <- ggplot(plot_data) +
  geom_sf(aes(geometry = geom, fill = team), col = "gray20") +
  scale_fill_manual(name = NULL,
                    values = team_logos$team_colour |> set_names(team_logos$team_unique),
                    na.value = "grey90",
                    guide = "none") +
  geom_text_repel(
    data = plot_data |> filter(!is.na(label)),
    aes(geometry = centroid, label = abbr, colour = team),
    fontface = "bold",
    family = "Montserrat",
    stat = "sf_coordinates",
    size = 4,
    min.segment.length = 0, force = 0,
    segment.size = 1,
    direction = "x",
    nudge_x = c(0, 0, -200000, 0, 0, 0, 400000, 0, 0, 0),
    nudge_y = c(0, 0,  200000, 0, 0, 0, 0, 0, 0, 0),
    hjust = 0.5, vjust = 0.5
  ) +
  scale_colour_manual(
    name = NULL,
    values = c("Canberra Raiders" = "#002856", "New Zealand Warriors" = "#262c68", "Penrith Panthers" = "#FF0090", "Cronulla Sutherland Sharks" = "#221f20"),
    na.value = "grey90",
    guide = "none"
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
    plot.background = element_rect(fill = "#c6f8efff"),
    panel.background = element_rect(fill = "#c6f8efff")
    #plot.margin = margin(t = 2, r = -20, b = 2, l = -20)
  )
# labs(
#   title = "US States in NRL/NRLW player names"
# )
name_map

ggsave(filename = "plots/us_state_initials.pdf", plot = name_map, 
       device = cairo_pdf, width = 8, height = 5, units = "in")
pdf_convert("plots/us_state_initials.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/us_states_initials.png")


##### Table Formatting #####
luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

team_formatter <- 
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "2px 4px 2px 4px", 
              `border-radius` = "4px",
              font.weight = "bold", 
              `background-color` = data.frame(team_name = x) |>
                left_join(team_data, by = "team_name") |>
                left_join(team_logos, by = "team_unique") |>
                pull(team_colour),
              color = data.frame(team_name = x) |>
                left_join(team_data, by = "team_name") |>
                left_join(team_logos, by = "team_unique") |>
                mutate(lum = luminance(team_colour),
                       text_col = ifelse(lum < 0.4, "#F2F2F2", "#1A1A1A")) |>
                pull(text_col)))

bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold",
              display = "block"
            ))

num_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat"
            ))

num_bold_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.size = "13px",
              font.family = "Montserrat",
              font.weight = "bold"
            ))

# Final table formatting
final_table <- vegas_initials |>
  left_join(US_states |> st_drop_geometry() |> select(abbr, full),
            by = c("initials" = "abbr")) |>
  filter(!(is.na(full))) |>
  arrange(initials) |>
  select(team, full_name, initials, full) |>
  rename(
    Team = team,
    Player = full_name,
    Initials = initials,
    State = full
  ) |>
  formattable(
    list(
      Team = team_formatter,
      Initials = bold_formatter
    ),
    align = c("r", "l", "c", "l"),
    table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Helvetica"'
  ) |>
  as.htmlwidget() |>
  prependContent(tags$style("td { padding: 2px  !important;}")) |>
  prependContent(tags$style("table thead {background-color: #120b2f; color: #ffffff; font-size: 14px; border-bottom: 3px solid #ffffff;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {border-top: 0.5px solid #b8b6c1;}")) |>
  prependContent(tags$style("table thead tr th:nth-of-type(n) {vertical-align: middle;}")) |>
  prependContent(tags$style("table tr:nth-of-type(n) td {vertical-align: middle;}"))
final_table

saveWidget(final_table, "tables/html/us_initials25.html")
webshot(url = "tables/html/us_initials25.html", 
        file = "tables/png/us_initials25.png", 
        selector = "div", zoom = 4,
        vwidth = 650)
 