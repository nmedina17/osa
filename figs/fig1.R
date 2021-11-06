library(here)
i_am(
  "figs/fig1.R"
)
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

measure1 <- quote(
  kg17
)
yAxisLabel1 <- quote(
  "Aboveground biomass (kg)"
)
measure2 <- quote(
  spg
)
yAxisLabel2 <- quote(
  expression(
    paste(
      "Wood density (cm"^3,
      " g"^-1,
      ")"
    )
  )
)
measure3 <- quote(
  h
)
yAxisLabel3 <- quote(
  "Tree height (m)"
)
measure4 <- quote(
  luz
)
yAxisLabel4 <- quote(
  "Canopy light (%)"
)


varForm <- median ~ dist
xAxisLabel <- quote(
  "Distance to secondary forest edge (m)"
)
statData <- plotResultsTbl1
underData <- cleanData




#panel

graph1 <-  statData %>%
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
graph1



#panel

graph2 <- statData %>%
  dotGraph(
    measure2,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2,
    ..addBins = T,
    ..cleanData = underData
  )
graph2



#panel

graph3 <- statData %>%
  dotGraph(
    measure3,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel3,
    ..addBins = T,
    ..cleanData = underData
  )
graph3



#panel

graph4 <- statData %>%
  dotGraph(
    measure4,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel4,
    ..addBins = T
  )
graph4



fig1 <- ggarrange(
  graph1,
  graph2,
  graph3,
  graph4,
  nrow = 2,
  ncol = 2
)

fig1
