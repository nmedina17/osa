library(here)
i_am(
  "figs/fig2.R"
)
#taxMetric,mainMetric
#plotResultsTbl1,taxaResultsTbl1
taxRank <- quote(
  gen
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
library(ggpubr)


#plan:
#-richness
#-diversity
#-taxaKernels--Vochysia,Guatteria,Virola


#userInput


measure2a <- quote(
  richness
)
yAxisLabel2a <- "Richness"

measure2b <- quote(
  entropy
)
yAxisLabel2b <- quote(
  "Diversity"
)

measure2c <- quote(
  Vochysia
)
taxMetricUnit <- "kg"
yAxisLabel2c <- quote(
  glue(
    measure2c,
    " prevalence",
    " (",
    taxMetricUnit,
    ")"
  )
)

measure2d <- quote(
  Ficus
)
yAxisLabel2d <- quote(
  glue(
    measure2d,
    " prevalence",
    " (",
    taxMetricUnit,
    ")"
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
  "Distance to edge (m)"
)
statData2ab <- plotResultsTbl1
statData2cd <- taxaResultsTbl1 %>%
  rename(
    variable = all_of(
      taxRank
    )
  )
statData2e <- ordTbl



#panel

graph2a <- statData2ab %>%
  dotGraph(
    measure2a,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2a,
    ..addCenters = T
  )
graph2a


#panel

graph2b <- statData2ab %>%
  dotGraph(
    measure2b,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2b,
    ..addCenters = T
  )

#addLine
graph2b


#panel

graph2c <- statData2cd %>%
  dotGraph(
    measure2c,
    taxForm[[3]],
    taxForm[[2]],
    xAxisLabel,
    yAxisLabel2c,
    ..addCenters = T
  ) +
  scale_y_log10()
graph2c


#panel

graph2d <- statData2cd %>%
  dotGraph(
    measure2d,
    taxForm[[3]],
    taxForm[[2]],
    xAxisLabel,
    yAxisLabel2d,
    ..addCenters = T
  )
graph2d
#Ficus2few...


#panel

graph2e <- ordTbl %>%
  ggplot(
    aes(
      x = PC1,
      y = PC2,
      color = as_factor(
        dist
      )
    )
  ) +
  geom_point() +
  stat_ellipse() +
  theme(
    legend.position = "top"
  )
graph2e



fig2 <- ggarrange(
  ggarrange(
    graph2a,
    graph2b,
    graph2c,
    graph2d,
    ncol = 2,
    nrow = 2,
    labels = c(
      "a", "b", "c", "d"
    )
  ),
  graph2e,
  labels = c(
    "", "e"
  ),
  ncol = 2
)
fig2
