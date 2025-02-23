##### Description #####
# An R script to look at sin bins / send-offs by decade and position

##### Libraries #####
{
  library(lubridate)
  library(readr)
  library(tidyverse)
  library(formattable)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
  library(showtext)
  library(ggrepel)
  library(ggtext)
  library(RColorBrewer)
  library(pdftools)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")

##### Analysis #####
match_position_data <- player_match_data |>
  group_by(match_id, position) |>
  summarise(sin_bins = sum(sin_bins + sin_bins5),
            send_offs = sum(send_offs),
            .groups = "drop")

send_offs_decade <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, year) |>
  left_join(match_position_data, by = "match_id") |>
  mutate(position = case_when(
                      position == "FB" ~ "Fullback",
                      position == "W" ~ "Wing",
                      position == "C" ~ "Centre",
                      position == "FE" ~ "Five-Eighth",
                      position == "HB" ~ "Halfback",
                      position == "HK" ~ "Hooker",
                      position == "FR" ~ "Prop",
                      position == "2R" ~ "Second Row",
                      position == "L" ~ "Lock",
                      position == "B" ~ "Bench"
                    ),
         position = factor(position, levels = c("Fullback", "Wing", "Centre", "Five-Eighth", "Halfback", "Hooker", "Prop", "Second Row", "Lock", "Bench"))
  ) |>
  mutate(decade = floor(year / 10) * 10,
         decade = ifelse(decade == 1900, 1910, decade)) |>
  group_by(decade, position) |>
  summarise(matches = length(unique(match_id)),
            bins_send_offs = sum(sin_bins + send_offs),
            .groups = "drop") |>
  na.omit() |>
  complete(decade, position, fill = list(matches = 0, bins_send_offs = 0)) |>
  group_by(decade) |>
  mutate(matches = max(matches)) |>
  ungroup() |>
  mutate(rate = bins_send_offs / matches,
         decade = paste0("<b>", decade, "s</b>", "<br>(",matches, ")"))

##### Plot #####
font_add_google("Montserrat")
showtext_auto(enable = TRUE)
fm <- 1
showtext_opts(dpi = 300)

send_offs_plot <- ggplot(send_offs_decade) +
  geom_tile(aes(x = decade, y = position,
                fill = rate)) +
  geom_text(aes(x = decade, y = position,
                label = bins_send_offs, colour = rate > 0.075),
            size = 5*fm, alpha = 0.6, vjust = 0.55) +
  scale_fill_gradientn(colours = brewer.pal(9, "Blues")[2:9],
                       breaks = seq(0, 0.14, by = 0.02),
                       labels = c(0, seq(0.02, 0.14, 0.02)),
                       guide = guide_colourbar(title = "Bins + Send-Offs / Match",
                                               position = "bottom",
                                               direction = "horizontal")) +
  scale_colour_manual(values = c("grey10", "grey90"),
                      guide = "none") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev) +
  theme_classic() +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_markdown(family = "Montserrat", size = 16*fm),
    axis.title.x = element_markdown(family = "Montserrat", size = 16*fm),
    axis.text.y = element_text(family = "Montserrat", size = 12*fm, margin = margin(r = -1), hjust = 1),
    axis.text.x.top = element_markdown(family = "Montserrat", size = 12*fm, lineheight = 0.7, margin = margin(t = 3, b = -3)),
    legend.key.width = unit(0.8*fm, "in"),
    legend.key.height = unit(0.15*fm, "in"),
    legend.ticks = element_blank(),
    legend.title = element_markdown(family = "Montserrat", size = 12*fm, face = "bold", hjust = 0.5),
    legend.title.position = "top",
    legend.text = element_text(family = "Montserrat", size = 10*fm, margin = margin(t = 3)),
    plot.title = element_markdown(family = "Montserrat", size = 20*fm, colour = "gray20", face = "bold", margin = margin(b = 10)),
    plot.title.position = "plot"
  ) +
  labs(
    x = "<b>Decade</b> (Matches)",
    y = "<b>Position</b>",
    title = "Total NSWRL/NRL Sin bins and Send-offs by decade and position"
  )
send_offs_plot

ggsave(filename = "plots/send_offs_decade.pdf", plot = send_offs_plot, 
       width = 10, height = 9, units = "in")
pdf_convert("plots/send_offs_decade.pdf", 
            format = "png", dpi = 1000,
            filenames = "plots/send_offs_decade.png")
