library(here)
i_am(
  "figs/fig2.R"
)
#taxMetric
source(
  here(
    "analysis/stats.R"
  )
)
#plotResultsTbl
library(glue)
#annotate()

library(oir)
library(ggpubr)


#plan:
#-richness
#-diversity
#-taxaKernels--Vochysia,Guatteria,Virola


#userInput


measure1 <- quote(
  richness
)
yAxisLabel1 <- "Richness"

measure2 <- quote(
  entropy
)
yAxisLabel2 <- quote(
  "Diversity"
)

measure3 <- quote(
  Vochysia
)
yAxisLabel3 <- quote(
  glue(
    measure3,
    " prevalence"
  )
)

measure4 <- quote(
  Ficus
)
yAxisLabel4 <- quote(
  glue(
    measure4,
    " prevalence"
  )
)


varForm <- {
  mainMetric %>%
    eval()
} ~ dist
#check.env...
taxForm <- {
  taxMetric %>%
    eval()
} ~ dist
xAxisLabel <- quote(
  "Distance to secondary forest edge (m)"
)
statData12 <- plotResultsTbl1
statData34 <- taxaResultsTbl1 %>%
  rename(
    "variable" = "gen"
  )
underData <- cleanData



#panel

graph1 <- statData12 %>%
  dotGraph(
    measure1,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel1,
    ..addBins = T
  )
graph1


#panel

graph2 <- statData12 %>%
  dotGraph(
    measure2,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2,
    ..addBins = T
  )

#addLine
graph2


#panel

graph3 <- statData34 %>%
  dotGraph(
    measure3,
    taxForm[[3]],
    taxForm[[2]],
    xAxisLabel,
    yAxisLabel3,
    ..addBins = T
  ) +
  scale_y_log10()
graph3


#panel

graph4 <- statData34 %>%
  dotGraph(
    measure4,
    taxForm[[3]],
    taxForm[[2]],
    xAxisLabel,
    yAxisLabel4,
    ..addBins = T
  )
graph4
#Ficus2few...

#HERE


#panel

<<<<<<< HEAD
# graph5 <- statData34 %>%
#   dotGraph(
#     measure5,
#     varForm[[3]],
#     varForm[[2]],
#     xAxisLabel,
#     yAxisLabel5,
#     ..addBins = T
#   )
# graph5
=======
graph5 <- statData34 %>%
  dotGraph(
    measure5,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel5,
    ..addBins = T
  )
graph5
>>>>>>> d64599d (ready pull merge)



fig2 <- ggarrange(
  graph1,
  graph2,
  graph3,
  graph4
  # graph5
)
fig2
