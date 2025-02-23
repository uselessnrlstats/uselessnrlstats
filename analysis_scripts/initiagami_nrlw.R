##### Description #####
# An R script to look at INITIAGAMI - unique player initials

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
 # library(showtext)
}

extrafont::font_import()
extrafont::loadfonts()

##### Load Data #####
player_data <- read_csv("cleaned_data/nrlw/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrlw/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrlw/match_data.csv")

# font_add_google("Montserrat")
# showtext_auto(enable = TRUE)
# showtext_opts(dpi = 300)
fm <- 2

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

##### Analysis #####
initial_data <- player_data |>
  mutate(last_name = str_replace(last_name, "\\? ", "")) |>
  filter(first_name != "?") |>
  mutate(first_name_initial = substr(first_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE),
         last_name_initial = substr(last_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE)) |>
  select(player_id, full_name, first_name, last_name, first_name_initial, last_name_initial)

plot_data <- initial_data |>
  count(first_name_initial, last_name_initial) |>
  complete(first_name_initial, last_name_initial,
           fill = list(n = NA)) |>
  mutate(text_col = (n >= 0.5*max(plot_data$n, na.rm = T)))

##### Plot #####

initiagami <- ggplot(plot_data) +
  geom_tile(aes(x = last_name_initial, y = first_name_initial,
                fill = n), col = alpha("gray20", 0.2), linewidth = 0.1) +
  # geom_tile(data = plot_data |> filter(first_name_initial == "I", last_name_initial == "I"),
  #           aes(x = last_name_initial, y = first_name_initial), fill = NA, col = "gray20", linewidth = 1) +
  geom_text(aes(x = last_name_initial, y = first_name_initial,
                    label = n, colour = text_col),
                size = 3, alpha = 1, vjust = 0.55, na.rm = TRUE) +
  scale_fill_gradientn(name = "# Players",
                       colours = paletteer_c("grDevices::Teal", 30) |> rev(),
                       guide = guide_coloursteps(
                         direction = "horizontal",
                         show.limits = TRUE
                       ),
                       n.breaks = 8,
                       na.value = "#FFFFFF",
                       trans = "sqrt") +
  scale_colour_manual(values = c("grey30", "white"),
                      guide = "none",
                      na.value = alpha("white", 0)) +
  scale_x_discrete(position = "top", expand = c(0,0), drop = FALSE) +
  scale_y_discrete(position = "left", limits = rev, expand = c(0,0), drop = FALSE) +
 theme_classic() +
 theme(
    text = element_text(family = "Montserrat"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_text(family = "Montserrat", size = 6*fm, angle = 90, margin = margin(r = 5)),
    axis.title.x = element_text(family = "Montserrat", size = 6*fm, margin = margin(b = 5)),
    axis.text.y = element_text(size = 5*fm, margin = margin(r = 1)),
    axis.text.x.top = element_text(size = 5*fm, margin = margin(b = 1)),
    panel.grid.major = element_blank(),
    plot.title = element_text(family = "Montserrat", size = 10*fm, colour = "gray20", face = "bold", margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10),
    legend.title = element_text(family = "Montserrat", size = 5*fm, hjust = 0.5),
    legend.title.position = "top",
    legend.text = element_text(family = "Montserrat", size = 4*fm, margin = margin(t = 2), hjust = 0.5),
    legend.position = "bottom",
    legend.margin = margin(t = -5),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(3, "lines")
  ) +
  labs(
    x = "Surname Initial",
    y = "First Name Initial",
    title = "Initiagami: NRLW"
  )
initiagami

ggsave(filename = "plots/initiagami_nrlw.pdf", plot = initiagami, 
       device = cairo_pdf, width = 8, height = 8, units = "in")
pdf_convert("plots/initiagami_nrlw.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/initiagami_nrlw.png")

###### Players with unique initials #####
player_data |>
  select(first_name, last_name, full_name) |>
  filter(first_name != "?") |>
  mutate(first_name_initial = substr(first_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = rev(LETTERS), ordered = TRUE),
         last_name_initial = substr(last_name, 1, 1) |> 
           str_to_upper() |>
           factor(levels = LETTERS, ordered = TRUE),
         initials = ifelse(is.na(first_name),
                           NA,
                           paste0(first_name_initial, last_name_initial))) |>
  group_by(initials) |>
  summarise(count = n(),
            names = paste0(full_name, collapse = ", ")) |>
  filter(count == 1) |>
  mutate(print_col = paste0(initials, ": ", names)) |>
  pull(print_col) |>
  print()

# First player with each initial
player_debuts <- player_match_data |>
  left_join(match_data |> select(match_id, date),
            by = "match_id") |>
  group_by(player_id) |>
  summarise(debut = min(date),
            .groups = "drop")

initial_data |>
  rowwise() |>
  mutate(full_name = paste(first_name, last_name)) |>
  ungroup() |>
  left_join(player_debuts, by = "player_id") |>
  group_by(first_name_initial, last_name_initial) |>
  filter(debut == max(debut)) |>
  filter(first_name_initial == last_name_initial) |>
  arrange(first_name_initial) |>
  pull(full_name) |>
  paste0(collapse = "_")

##### New Initials #####
player_debuts <- player_match_data |>
  left_join(match_data, by = "match_id") |>
  arrange(date) |>
  group_by(player_id) |>
  summarise(debut_year = year(min(date)),
            .groups = "drop")

initial_data |>
  left_join(player_debuts, by = "player_id") |>
  filter(first_name != "?") |>
  mutate(initials = ifelse(is.na(first_name),
                           NA,
                           paste0(first_name_initial, last_name_initial))) |>
  group_by(initials) |>
  summarise(count = n(),
            names = paste0(full_name, collapse = ", "),
            debut_year = min(debut_year)) |>
  filter(debut_year == 2024) |>
  mutate(print_col = paste0(initials, ": ", names)) |>
  pull(print_col) |>
  print()
