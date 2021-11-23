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
  "Distance to secondary forest edge (m)"
)
statData <- plotResultsTbl1
underData <- cleanData




#panel

graph1a <-  statData %>%
  dotGraph(
    measure1,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel1,
    ..addBins = T,
    ..cleanData = underData
  ) +
  scale_y_continuous(
    trans = "log10"
  )
graph1a



#panel

graph1b <- statData %>%
  dotGraph(
    measure2,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2,
    ..addBins = T,
    ..cleanData = underData
  )
graph1b



#panel

graph1c <- statData %>%
  dotGraph(
    measure3,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel3,
    ..addBins = T,
    ..cleanData = underData
  )
graph1c



#panel

graph1d <- statData %>%
  dotGraph(
    measure4,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel4,
    ..addBins = T
  )
graph1d



fig1 <- ggarrange(
  graph1a,
  graph1b,
  graph1c,
  graph1d,
  nrow = 2,
  ncol = 2
)

fig1
