library(here)
source(
  here(
    "figs/style.R"
  )
)
source(
  here(
    "analysis/stats.R"
  )
)
#plotResultsTbl
library(glue)
#annotate()


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

)
yAxisLabel3 <- quote(

)


varForm <- n ~ dist
varForm1 <- median ~ dist
xAxisLabel <- quote(
  "Distance to secondary forest edge (m)"
)
statData12 <- plotResultsTbl1
statData34 <- taxaResultsTbl1
underData <- cleanData



#panel

graph1 <- statData12 %>%
  dotGraph(
    measure1,
    varForm1[[3]],
    varForm1[[2]],
    xAxisLabel,
    yAxisLabel1,
    ..addGroups = T
  )
graph1


#panel

graph2 <- statData12 %>%
  dotGraph(
    measure2,
    varForm1[[3]],
    varForm1[[2]],
    xAxisLabel,
    yAxisLabel2,
    ..addGroups = T
  )
graph2


#panel

#HERE

graph3 <- statData34 %>%
  dotGraph(
    measure3,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel3,
    ..addGroups = T,
    ..y1 = varForm1[[2]]
  )
graph3


#panel

graph4 <- statData34 %>%
  dotGraph(
    measure4,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel4,
    ..addGroups = T
  )
graph4


#panel

graph5 <- statData34 %>%
  dotGraph(
    measure5,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel5,
    ..addGroups = T
  )
graph5



fig2 <- ggarrange(
  graph1,
  graph2,
  graph3,
  graph4,
  graph5
)
fig2
