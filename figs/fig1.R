library(here)
i_am(
  "figs/fig1.R"
)
#median,dist
#plotResultsTbl,cleanData
source(
  here(
    "analysis/stats.R"
  )
)
#plotResultsTbl
library(glue)
#annotate()

library(oir)
library(gginnards)
#append_layers()
library(ggpubr)
#ggarrange()


#userInput
#sortbyPvals

measure1a <- quote(
  kg17
)
yAxisLabel1a <- quote(
  "Aboveground biomass (kg)"
)
measure1b <- quote(
  spg
)
yAxisLabel1b <- quote(
  expression(
    paste(
      "Wood density (cm"^3,
      " g"^-1,
      ")"
    )
  )
)
measure1c <- quote(
  h
)
yAxisLabel1c <- quote(
  "Tree height (m)"
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




#panel

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
    trans = "log10"
  )
graph1a



#panel

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



#panel

graph1c <- statData %>%
  dotGraph(
    measure1c,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel1c,
    ..addBins = T,
    ..cleanData = underData,
    ..addCenters = T
  )
graph1c



#panel

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



fig1 <- ggarrange(
  graph1a,
  graph1b,
  graph1c,
  graph1d,
  nrow = 2,
  ncol = 2,
  labels = c(
    "a",
    "b",
    "c",
    "d"
  )
)


fig1


ggsave(
  "fig1.pdf",
  fig1,
  path = "figs",
  width = 3,
  height = 3,
  units = "in"
)
