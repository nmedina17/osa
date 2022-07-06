# refs ----

library(here); i_am("figs/fig1.R")
#median,dist
#plotResultsTbl,cleanData
taxRank <- quote(
  gen
)
source(here("analysis/stats.R"))
#plotResultsTbl
library(glue)
#annotate()

library(oir)
library(gginnards)
#append_layers()
library(ggpubr)
#ggarrange()


#

#userInput
#sortbyPvals

measure1a <- quote(
  kg17
)
yAxisLabel1a <- quote(
  expression(
    paste(
      "Biomass (kg ha"^-1,
      ")"
    )
  )
)
measure1b <- quote(
  spg
)
yAxisLabel1b <- quote(
  expression(
    paste(
      "Wood density (g",
      " cm"^-3,
      ")"
    )
  )
)
measure1c <- quote(
  stems
)
yAxisLabel1c <- quote(
  expression(
    paste(
      "Stems (0.04 ha"^-1,
      ")"
    )
  )
)
measure1d <- quote(
  luz
)
yAxisLabel1d <- quote(
  "Canopy light (%)"
)


varForm <- median ~ dist
xAxisLabel <- quote(
  "Distance to edge (m)"
)
statData <- plotResultsTbl1
underData <- cleanData




#panel----

graph1a <-  statData %>%
  dotGraph(
    measure1a,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel1a,
    ..addBins = T,
    ..cleanData = underData,
    ..addCenters = T
  ) +
  scale_y_continuous(
    trans = "log10",
    labels = trans_format(
      "log10", math_format()
    )
  )
graph1a



#panel----

graph1b <- statData %>%
  dotGraph(
    measure1b,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel1b,
    ..addBins = T,
    ..cleanData = underData,
    ..addCenters = T,
    ..addLines = T
  )
graph1b



#panel----

statData$pval[[9]] <-
  #varsDoubled
  plotResultsTbl12$pval[[9 * 2]] %>%
  last()

graph1c <- statData %>%
  dotGraph(
    measure1c,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel1c,
    ..addBins = T,
    # ..cleanData = underData,
    ..addCenters = T,
    ..addCurve = T
   # ..addLines = T
  )
graph1c



#panel----

graph1d <- statData %>%
  dotGraph(
    measure1d,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel1d,
    ..addBins = T,
    ..addCenters = T
  )
graph1d



#panel----

measure1e <- quote(tissue.c)
yAxisLabel1e <- "Tissue C (%)"
graph1e <- statData %>%
  dotGraph(
    measure1e,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel1e,
    ..addBins = T,
    ..addCenters = T,
    ..addP = F,
    ..cleanData = underData
  )
graph1e



#arrange----



fig1 <- ggarrange(
  graph1a,
  graph1b,
  graph1e,
  #hists
  graph1c,
  graph1d,
  nrow = 3,
  ncol = 2,
  labels = c(
    "a",
    "b",
    "c",
    "d",
    "e"
  ),
  font.label = list(
    size = 8
  )
  # widths = 0.25,
  # heights = 0.25
)

  # annotate_figure(
  #   bottom = "
  #   Regenerating stand properties, specifically (a) aboveground biomass (n = ?), (b) wood density (n = ?), (c) stem density (n = 30), and (d) canopy light availability (n = 30). Raw data (gray) (n = 1200) underly plot medians (white) (n = 3) which underly distance stratum medians (black) ± 1 median absolute deviation (n = 5). Regression lines follow strata medians if marginally significant.
  #   "
  # )


fig1


# ggpubr::ggsave("fig1.pdf", fig1, path = "figs", width = 3, height = 3, units = "in")
# ggpubr::ggsave("fig1.png", fig1, path = "figs", width = 3, height = 3, units = "in")
