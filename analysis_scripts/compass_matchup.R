##### Description #####
# An R script to look at players who debuted in the same game

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
  library(chorddiag)
  library(igraph)
  library(tidygraph)
  library(pdftools)
  library(tools)
  library(extrafont)
  library(png)
  library(raster)
}

##### Load Data #####
player_data <- read_csv("cleaned_data/nrl/player_data.csv")
player_match_data <- read_csv("cleaned_data/nrl/player_match_data.csv")
match_data <- read_csv("cleaned_data/nrl/match_data.csv")
team_data <- read_csv("cleaned_data/nrl/team_data.csv")
team_logos <- read_csv("cleaned_data/nrl/team_logos.csv")

compass_teams <- c("North Queensland Cowboys", "Eastern Suburbs Roosters", "South Sydney Rabbitohs", "Wests Tigers")
compass_teams1 <- c("North Queensland Cowboys", "Sydney Roosters", "South Sydney Rabbitohs", "Wests Tigers")

##### Helper Stats #####
match_results <- match_data |>
  mutate(year = gsub(".+ (\\d+)", "\\1", competition_year) |> as.numeric()) |>
  select(match_id, competition_year, year, round, time, home_team, home_team_score, away_team, away_team_score) |>
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") |>
  left_join(match_data |> select(match_id, home_team, away_team), by = "match_id") |>
  mutate(opposition = ifelse(team == home_team, away_team, home_team)) |>
  select(-c(home_team, away_team)) |>
  mutate(home_away = toupper(substr(home_away, 1, 1)),
         score_for = ifelse(home_away == "H", home_team_score, away_team_score),
         score_against = ifelse(home_away == "H", away_team_score, home_team_score),
         result = case_when(
           score_for > score_against ~ "W",
           score_for < score_against ~ "L",
           .default = "D")) |>
  select(match_id, competition_year, year, round, time, home_away, team, opposition, score_for, score_against, result) |>
  left_join(team_data |> select(team_name, team_unique), by = c("team" = "team_name")) |>
  left_join(team_data |> select(team_name, team_unique) |> rename(opp_unique = team_unique), by = c("opposition" = "team_name"))

##### Analysis #####
chord_data <- match_results |>
  filter(team_unique %in% compass_teams & opp_unique %in% compass_teams,
         year >= 2000) |>
  group_by(team_unique, opp_unique) |>
  summarise(n_wins = sum(result == "W"),
            n_draws = sum(result == "D"),
            n_losses = sum(result == "L"),
            points_for = sum(score_for),
            points_against = sum(score_against),
            .groups = "drop") |>
  mutate(team_unique = factor(team_unique, levels = compass_teams, ordered = TRUE),
         opp_unique = factor(opp_unique, levels = compass_teams, ordered = TRUE)) |>
  arrange(team_unique, opp_unique)

teams <- team_logos |>
  filter(team_unique %in% unique(chord_data$team_unique)) |>
  pull(team_unique)

colours <- team_logos |>
  filter(team_unique %in% unique(chord_data$team_unique)) |>
  pull(team_colour) |>
  setNames(teams)

cd_colours <- tibble(team_unique = compass_teams) |>
  left_join(team_logos, by = "team_unique")

##### Plot #####
compass <- chordDiagram(
  x = chord_data |> select(team_unique, opp_unique, n_wins), 
  grid.col = colours,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows"), 
  diffHeight  = -0.04,
  self.link = 1,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = "default", 
  link.largest.ontop = FALSE)

# ggsave(filename = "plots/compass_matchup.pdf", plot = compass, 
#        width = 5, height = 5, units = "in")
# pdf_convert("plots/compass_matchup.pdf", 
#             format = "png", dpi = 1000,
#             filenames = "plots/compass_matchup.png")

compass2 <- chorddiag(
  data = chord_data |> select(team_unique, opp_unique, n_wins) |> as_tbl_graph() |> as_adjacency_matrix(attr = "n_wins") |> as.matrix(),
  groupNames = c(NA, NA, NA, NA),
  margin = 100,
  groupPadding = 1,
  groupColors = cd_colours$team_colour,
  groupThickness = 0.03,
  groupnamePadding = 5,
  groupnameFontsize = 12,
  groupedgeColor = NULL,
  chordedgeColor = NULL,
  showTicks = TRUE,
  tickInterval = 2,
  tooltipNames = compass_teams,
  tooltipGroupConnector = "    &#x25B6;    "
)
compass2

saveWidget(compass2, "plots/compass_matchup.html")
webshot(url = "plots/compass_matchup.html",
        file = "plots/compass_matchup.png",
        selector = "div", zoom = 4,
        vwidth = 900)

#blue = rgb(0, 43, 92)
#red = rgb(203, 32, 34)
#green = rgb(7, 78, 22)
#orange = rgb(237, 119, 30)


##### Experimentation #####
{
  m <- data.frame(order = 1:4,
                  team = c("North Queensland Cowboys", "Eastern Suburbs Roosters", "South Sydney Rabbitohs", "Wests Tigers"),
                  V3 = c(0, 25, 17, 24),
                  V4 = c(13, 0, 19, 9),
                  V5 = c(19, 30, 0, 17),
                  V6 = c(21, 30, 26, 0),
                  r = c(0, 203, 7, 237),
                  g = c(43, 32, 78, 119),
                  b = c(92, 34, 22, 30),
                  stringsAsFactors = FALSE)
  
  ### Create a data frame
  df1 <- m[, c(1,2, 7:9)]
  
  ### Create a matrix
  m1 <- m[,-c(1:2, 7:9)]
  m2 <- as.matrix(m1)
  dimnames(m2) <- list(win = df1$team, lose = df1$team)
  
  ### Sort order of data.frame and matrix for plotting in circos
  df1 <- arrange(df1, order)
  df1$team <- factor(df1$team, levels = df1$team)
  m3 <- m2[levels(df1$team),levels(df1$team)]
  
  ### Define ranges of circos sectors and their colors (both of the sectors and the links)
  df1$xmin <- 0
  df1$xmax <- rowSums(m3) + colSums(m3)
  n <- nrow(df1)
  
  df1$rcol <- rgb(df1$r, df1$g, df1$b, max = 255)
  df1$lcol <- rgb(df1$r, df1$g, df1$b, alpha = 200, max = 255)
}
## Plot sectors (outer part)
{
  par(mar = rep(0,4))
  circos.clear()

  ### Basic circos graphic parameters
  circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0.15), start.degree = 131, gap.degree = 4)

  ### Sector details
  circos.initialize(factors = df1$team, xlim = cbind(df1$xmin, df1$xmax))
}

### Plot sectors
circos.trackPlotRegion(ylim = c(0, 1), sectors = df1$team, track.height=0.1,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name <- get.cell.meta.data("sector.index")
                         i <- get.cell.meta.data("sector.numeric.index")
                         xlim <- get.cell.meta.data("xlim")
                         ylim <- get.cell.meta.data("ylim")
                         
                         #plot main sector
                         circos.rect(xleft = xlim[1], xright = xlim[2], ybottom = ylim[1] - 0.2, ytop = ylim[2], 
                                     col = df1$rcol[i], border = df1$rcol[i])
                         
                         #blank in part of main sector
                         circos.rect(xleft = xlim[1] + rowSums(m3)[i], xright = xlim[2] - 0.3, ybottom = ylim[1] - 0.17, ytop = ylim[1] + 0.3,
                                     col = "white", border = "white")
                         
                         #white line all the way around
                         circos.rect(xleft = xlim[1], xright = xlim[2], ybottom = 0.3,  ytop = 0.32, 
                                     col = "white", border = "white")
                         
                         #text direction (dd)
                         theta <- circlize(mean(xlim), 1.3)[1, 1] %% 360
                         dd <- ifelse(theta > 0 & theta < 180, "bending.inside", "bending.outside")
                         #plot team labels
                         circos.text(x = mean(xlim), y = 0.7, labels = toupper(name), facing = dd, 
                                     family = "Montserrat", font = 2, cex = 1.5, col = "white")
                         # wins and losses labels
                         circos.text(x = xlim[1] + rowSums(m3)[i]/2, y = 0.05, labels = "WINS", facing = dd, 
                                     family = "Montserrat", font = 1, cex = 1, col = "white")
                         circos.text(x = mean(c(xlim[1] + rowSums(m3)[i], xlim[2])), y = 0.05, labels = "LOSSES", facing = dd, 
                                     family = "Montserrat", font = 1, cex = 1, col = df1$rcol[i])
                       })


## Plot links (inner part)
### Add sum values to df1, marking the x-position of the first links
### out (sum1) and in (sum2). Updated for further links in loop below.
{
  df1$sum1 <- numeric(n)
  df1$sum2 <- rowSums(m3)
  
  ### Create a data.frame of the flow matrix sorted by flow size, to allow largest flow plotted first
  df2 <- cbind(as.data.frame(m3), orig = rownames(m3), stringsAsFactors=FALSE)
  
  df3 <- reshape(df2, idvar="orig", varying=list(1:n), direction="long",
                 timevar="dest", time=rownames(m3),  v.names = "m3")
  
  df4 <- df3 |> 
    mutate(orig = factor(orig, levels = compass_teams, ordered = TRUE),
           dest = factor(dest, levels = compass_teams, ordered = TRUE)) |>
    arrange(orig, dest) |>
    filter(m3 > 0) |>
    mutate(win1 = c(40, 21, 0, 0, 55, 25, 19, 0, 36, 26, 17, 0),
           win2 = c(53, 40, 21, 25, 85, 55, 36, 19, 62, 50, 26, 17),
           loss1 = c(85, 92, 106, 94, 62, 76, 77, 107, 50, 53, 98, 111),
           loss2 = c(98, 111, 127, 119, 92, 106, 94, 126, 76, 77, 107, 128))
}

### Plot links

for(k in 1:nrow(df4)){
  #i,j reference of flow matrix
  i <- match(df4$orig[k],df1$team)
  j <- match(df4$dest[k],df1$team)
  
  #plot link
  circos.link(sector.index1 = df1$team[i], point1 = c(df4$win1[k], df4$win2[k]),
              sector.index2 = df1$team[j], point2 = c(df4$loss1[k], df4$loss2[k]),
              rou = 0.7,
              col = df1$lcol[i],
              border = NA,
              h.ratio = 0.75)
  circos.text(sector.index = df1$team[i], 
              x = mean(c(df4$win1[k], df4$win2[k])), y = -0.9, label = df4$m3[k],
              facing = "downward", family = "Montserrat", col = "white", cex = 1.4)
  circos.text(sector.index = df1$team[j], 
              x = mean(c(df4$loss1[k], df4$loss2[k])), y = -0.9, label = df4$m3[k],
              facing = "downward", family = "Montserrat", col = "white", cex = 1.4)
}

#dev.off()
dev.print(cairo_pdf, height = 12, width = 12, "plots/compass_good1.pdf")
pdf_convert("plots/compass_good1.pdf", 
            format = "png", dpi = 500,
            filenames = "plots/compass_good1.png")

