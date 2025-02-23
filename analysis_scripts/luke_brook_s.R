##### Description #####
# An R script to look at rock names scoring on debut

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot)
  library(osmdata)
  library(sf)
  library(leaflet)
  library(ggmap)
}
register_stadiamaps("081d3be4-c77a-4f06-a34a-978dfd10f293", write = TRUE)

##### Load Data #####
luke1 <- (opq(bbox = "Europe") |>
  add_osm_feature(key = "name", value = "Luke Brook") |>
  osmdata_sf())$osm_lines |>
  select(-c(osm_id, layer, tunnel))

luke1.5 <- (opq_osm_id(type = "way", id = c(750121826, 323363094)) |>
  opq_string () |>
  osmdata_sf ())$osm_lines |>
  select(-c(layer, source, tunnel)) |>
  mutate(name = "Luke Brook") |>
  relocate(name, .before = waterway)

luke2 <- (opq(bbox = "America") |>
  add_osm_feature(key = "name", value = "Luke Brook") |>
  osmdata_sf())$osm_lines |>
  select(-c(osm_id, source, layer, tunnel))

luke2.5 <- (opq_osm_id(type = "way", id = c(152749107, 152749181, 152749223, 152749151)) |>
  opq_string () |>
  osmdata_sf ())$osm_lines |>
  select(-c(source)) |>
  mutate(name = "Luke Brook") |>
  relocate(name, .before = waterway)

luke_brook <- rbind(luke1, luke1.5, luke2, luke2.5) |>
  distinct(name, waterway, geometry) |>
  rownames_to_column("osm_id") |>
  mutate(group = c(1,1,1,1,1,1,2,2,3,3,3,3,3),
         length = st_length(geometry)) |>
  group_by(name, waterway, group) |>
  summarise(geometry = st_union(geometry),
            length = sum(length),
            LB_length = length / 1.8,
            .groups = "drop") |>
  mutate(centre = st_centroid(geometry))

##### Maps Stadia #####
# Map 1 - Britain #
map1_bbox <- st_bbox(luke_brook[1,]) |> unname() |> set_names(c("left", "bottom", "right", "top"))

map1 <- get_stadiamap(map1_bbox, zoom = 2, maptype = "stamen_toner")
ggmap(map1)
  


##### Maps Leaflet #####
# Map 1 - Britain #
map1 <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png?api_key=081d3be4-c77a-4f06-a34a-978dfd10f293",
    attribution = paste('&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                        '&copy; <a href="https://stamen.com/" target="_blank">Stamen Design</a> ' ,
                        '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                        '&copy; <a href="https://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>'),
    options = tileOptions(variant='stamen_toner_lite', apikey = '081d3be4-c77a-4f06-a34a-978dfd10f293')
  )  |>
  addPolylines(data = luke_brook[1,],
               weight = 6,
               color =  "#00008B",
               label = "Luke Brook, Marstow, Herefordshire, England, United Kingdom",
               smoothFactor = 0)|>
  setView(lng = -2.647486, lat = 51.89328, zoom = 14)
map1

saveWidget(map1, "plots/luke_brooks/map1.html", selfcontained = FALSE)
webshot(url = "plots/luke_brooks/map1.html",
        file = "plots/luke_brooks/map1.png",
        delay = 1, zoom = 5,
        vheight = 1000, vwidth = 500)

##### Map 2 - Canada 1 #####
map2 <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png?api_key=081d3be4-c77a-4f06-a34a-978dfd10f293",
    attribution = paste('&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                        '&copy; <a href="https://stamen.com/" target="_blank">Stamen Design</a> ' ,
                        '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                        '&copy; <a href="https://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>'),
    options = tileOptions(variant='stamen_toner_lite', apikey = '081d3be4-c77a-4f06-a34a-978dfd10f293')
  )  |>
  addPolylines(data = luke_brook[2,],
               weight = 6,
               color =  "#00008B",
               label = "Luke Brook, Saint-Paul Parish, Kent County, New Brunswick, Canada",
               smoothFactor = 0) |>
  setView(lng = -66.78114, lat = 46.69704, zoom = 15)
map2

saveWidget(map2, "plots/luke_brooks/map2.html", selfcontained = FALSE)
webshot(url = "plots/luke_brooks/map2.html",
        file = "plots/luke_brooks/map2.png",
        delay = 1, zoom = 5,
        vheight = 500, vwidth = 1000)


map3 <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png?api_key=081d3be4-c77a-4f06-a34a-978dfd10f293",
    attribution = paste('&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                        '&copy; <a href="https://stamen.com/" target="_blank">Stamen Design</a> ' ,
                        '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                        '&copy; <a href="https://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>'),
    options = tileOptions(variant='stamen_toner_lite', apikey = '081d3be4-c77a-4f06-a34a-978dfd10f293')
  )  |>
  addPolylines(data = luke_brook[3,],
               weight = 6,
               color =  "#00008B",
               label = "Luke Brook, Stanley Parish, New Brunswick, Canada",
               smoothFactor = 0) |>
  setView(lng = -65.03329, lat = 46.36331, zoom = 14)

map3

saveWidget(map3, "plots/luke_brooks/map3.html", selfcontained = FALSE)
webshot(url = "plots/luke_brooks/map3.html",
        file = "plots/luke_brooks/map3.png",
        delay = 1, zoom = 5,
        vheight = 500, vwidth = 1000)

# map1_inset <-leaflet(options = leafletOptions(zoomControl = FALSE)) |>
#   addProviderTiles(providers$Stadia.Outdoors) |>
#   addCircleMarkers(data = (luke_brook |> st_drop_geometry() |> st_set_geometry("centre"))[1,],
#                    weight = 30,
#                    radius = 30,
#                    color =  "#00008B") |>
#   setView(lng = -2.5, lat = 54.5, zoom = 7)
# map1_inset




# addMiniMap(
#   tiles = providers$Stadia.Outdoors,
#   position = "topright",
#   width = 200, height = 200,
#   zoomLevelFixed = 7,
#   toggleDisplay = FALSE,
#   aimingRectOptions = list(color = "maroon", weight = 1, clickable = FALSE)) |>