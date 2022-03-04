# refs ----

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


#

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
      "Wood density (g"^-1,
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
      "Stems (441 m"^-2,
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
  ),
  font.label = list(
    size = 8
  )
  # widths = 0.25,
  # heights = 0.25
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

ggsave(
  "fig1.png",
  fig1,
  path = "figs",
  width = 3,
  height = 3,
  units = "in"
)
