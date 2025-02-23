##### Description #####
# An R script to visualise player transfers between clubs

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(circlize)
  library(igraph)
  library(tidygraph)
  library(pdftools)
  library(tools)
  library(extrafont)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

##### Analysis #####
player_transfers <- player_match_data |>
  select(player_id, match_id, team) |>
  left_join(player_data |> select(player_id, full_name),
          by = "player_id") |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  mutate(year = year(date)) |>
  arrange(full_name, date) |>
  group_by(player_id, full_name) |>
  mutate(team_from = case_when(
    team != lag(team) ~ lag(team),
    row_number() == 1 ~ "Debut",
    .default = NA
  )) |>
  filter(!is.na(team_from)) |>
  rename(team_to = team) |>
  select(player_id, full_name, year, team_from, team_to)


##### Plotting Data #####
chord_data <- player_transfers |>
  mutate(team_from = ifelse(team_from == "Debut", team_to, team_from)) |>
  left_join(team_data, by = c("team_from" = "team_name")) |>
  left_join(team_logos, by = "team_unique") |>
  mutate(team_from = team_unique) |>
  select(player_id, full_name, year, team_from, team_to) |>
  left_join(team_data, by = c("team_to" = "team_name")) |>
  left_join(team_logos, by = "team_unique") |>
  mutate(team_to = team_unique) |>
  select(player_id, full_name, year, team_from, team_to) |>
  filter(year >= 2020) |>
  arrange(year, full_name) |>
  group_by(team_from, team_to) |>
  summarise(
    counts = n(),
    players = paste0(paste0(year, ": ", full_name), collapse = ", "),
    .groups = "drop"
  ) |>
  filter(team_from != team_to) |>
  left_join(team_data |> select(team_unique, team_mascots) |> distinct(), by = c("team_from" = "team_unique")) |>
  mutate(team_from = team_mascots) |> select(-team_mascots) |>
  left_join(team_data |> select(team_unique, team_mascots) |> distinct(), by = c("team_to" = "team_unique")) |>
  mutate(team_to = team_mascots) |> select(-team_mascots)

##### circlize #####
circos.clear()
circos.par(start.degree = 90, 
           gap.degree = 1.5, 
           track.margin = c(-0.1, 0.1), 
           points.overflow.warning = FALSE)
par(mar = rep(0, 4))

teams <- team_data |>
  filter(team_mascots %in% unique(c(chord_data$team_from, chord_data$team_to))) |>
  pull(team_mascots) |> unique()

colours <- team_data |>
  select(team_unique, team_mascots) |>
  distinct() |>
  filter(team_mascots %in% unique(c(chord_data$team_from, chord_data$team_to))) |>
  left_join(team_logos, by = "team_unique") |>
  pull(team_colour) |>
  setNames(teams)

# Base plot
chordDiagram(
  x = chord_data, 
  grid.col = colours,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows"), 
  self.link = 1,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.07),
  link.arr.type = "big.arrow",
  link.arr.length = 0.025,
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  h.ratio = 0.6)

circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    
    xlim <- get.cell.meta.data("xlim")
    sector.index <- get.cell.meta.data("sector.index") |> toupper()
    
    #text direction (dd)
    theta <- circlize(mean(xlim), 1.3)[1, 1] %% 360
    dd <- ifelse(theta > 0 & theta < 180, "bending.inside", "bending.outside")
    
    # Add names to the sector.
    circos.text(
      x = mean(xlim),
      y = 1.3,
      labels = sector.index,
      facing = dd,
      cex = 1.1,
      family = "Montserrat"
    )
  }
)

dev.print(cairo_pdf, height = 12, width = 12, "plots/transfers_2020-24.pdf")
pdf_convert("plots/transfers_2020-24.pdf", 
            format = "png", dpi = 500,
            filenames = "plots/transfers_2020-24.png")

##### chorddiag #####

cd_colours <- team_logos |>
  filter(team_unique %in% unique(chord_data$team_to)) |>
  arrange(team_unique) |>
  select(-logo_path) |>
  left_join(team_data |> select(team_unique, team_mascots), by = "team_unique") |>
  distinct()

chorddiag(data = chord_data |> as_tbl_graph() |> as_adjacency_matrix(attr = "counts") |> as.matrix(),
          groupNames = cd_colours$team_mascots,
          margin = 100,
          groupPadding = 1,
          groupColors = cd_colours$team_colour,
          groupThickness = 0.03,
          groupnamePadding = 5,
          groupnameFontsize = 12,
          groupedgeColor = NULL,
          chordedgeColor = NULL,
          showTicks = FALSE,
          tooltipGroupConnector = "    &#x25B6;    "
)
