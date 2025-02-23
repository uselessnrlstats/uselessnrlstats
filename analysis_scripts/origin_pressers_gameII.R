##### Description #####
# An R script to look at countries appearing in player names

##### Libraries #####
{
  library(tidyverse)
  library(readr)
  library(stringr)
  library(RColorBrewer)
  library(wordcloud)
  library(wordcloud2)
  library(monochromeR)
  library(htmltools)
  library(htmlwidgets)
  library(webshot2)
}

extrafont::loadfonts()

##### Load Data #####
top_words <- read_csv("analysis_scripts/extra_data/wordFrequency.csv") |>
  filter(row_number() <= 100) |>
  pull(lemma)

qld <- readLines("analysis_scripts/extra_data/SOO II 2024 post-match/QLD_presser.txt") |>
  strsplit(split = " ") |> unlist() |>
  as.tibble() |>
  count(value) |>
  filter(!(value %in% top_words)) |>
  arrange(desc(n))
nsw <- readLines("analysis_scripts/extra_data/SOO II 2024 post-match/NSW_presser.txt") |>
  strsplit(split = " ") |> unlist() |>
  as.tibble() |>
  count(value) |>
  filter(!(value %in% top_words)) |>
  arrange(desc(n))

luminance <- function(col) {c(c(.299, .587, .114) %*% col2rgb(col)/255)}

qld_colours <- generate_palette("maroon", "go_both_ways", n_colours = 20)[5:15]
nsw_colours <- generate_palette("skyblue", "go_both_ways", n_colours = 20)[5:15]

##### Analysis #####
qld_wc <- wordcloud2(qld,
           fontFamily = "Montserrat Medium",
           size = 3,
           color = sample(qld_colours, dim(qld)[1], replace = TRUE),
           backgroundColor = "white",
           shape = "circle",
           ellipticity = 1,
           rotateRatio = 1,
           widgetsize = c(2000,2000))

nsw_wc <- wordcloud2(nsw,
                     fontFamily = "Montserrat Medium",
                     size = 2,
                     color = sample(nsw_colours, dim(nsw)[1], replace = TRUE),
                     backgroundColor = "black",
                     shape = "circle",
                     ellipticity = 1,
                     rotateRatio = 1,
                     widgetsize = c(2000,2000))

##### Widget Save #####
saveWidget(qld_wc, "plots/qld_wcII.html", selfcontained = FALSE)
webshot(url = "plots/qld_wcII.html", 
        file = "plots/qld_wcII.png",
        delay = 5,
        zoom = 5,
        vwidth = 2000,
        selector = '#canvas')

saveWidget(nsw_wc, "plots/nsw_wcII.html", selfcontained = FALSE)
webshot(url = "plots/nsw_wcII.html", 
        file = "plots/nsw_wcII.png",
        delay = 5,
        zoom = 5,
        vwidth = 2000,
        selector = '#canvas')
