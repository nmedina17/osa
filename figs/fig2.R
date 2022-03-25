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


#defs----


#userInput


measure2a <- quote(
  entropy
)
yAxisLabel2a <- quote(
  "Diversity (H')"
)

measure2b <- quote(
  richness
)
yAxisLabel2b <- quote(
  "Richness"
)

measure2e <- quote(
  Vochysia
)
taxMetricUnit <- "kg"
yAxisLabel2e <- quote(
  glue(
    measure2e,
    " prevalence",
    " (",
    taxMetricUnit,
    ")"
  )
)

measure2f <- quote(
  Ficus
)
yAxisLabel2f <- quote(
  glue(
    measure2f,
    " prevalence",
    " (",
    taxMetricUnit,
    ")"
  )
)

measure2c <- quote(
  kg17
)
yAxisLabel2c <- quote(
  glue(
    "Biomass (",
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
statData2ef <- taxaResultsTbl1 %>%
  rename(
    "variable" = all_of(
      taxRank
    )
  )
statData2c <- ordTbl



#panel----

plotResultsTbl1$pval[[2]] <-
  #bug=doublePosition
  plotResultsTbl12$pval[[2 * 2]] %>%
  last()
graph2a <- statData2ab %>%
  dotGraph(
    measure2a,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2a,
    ..addCenters = T,
    ..addCurve = T
  )
graph2a


#panel----

graph2b <- statData2ab %>%
  dotGraph(
    measure2b,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2b,
    ..addCenters = T,
    ..addLines = T
  )
#addLine
graph2b


#panel----

graph2e <- statData2ef %>%
  dotGraph(
    measure2e,
    taxForm[[3]],
    taxForm[[2]],
    xAxisLabel,
    yAxisLabel2e,
    ..addCenters = T
  ) +
  scale_y_log10()
graph2e


#panel----

graph2f <- statData2ef %>%
  dotGraph(
    measure2f,
    taxForm[[3]],
    taxForm[[2]],
    xAxisLabel,
    yAxisLabel2f,
    ..addCenters = T,
    ..addLines = T
  )
graph2f
#Ficus2few...


#panel----

graph2d <- ordTbl %>%
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
  ) +
  labs(
    color = "Dist (m)"
  ) +
  scale_color_brewer(
    direction = -1
  ) +
  annotate(
    "text",
    label = glue(
      "P = ",
      ordStat$
        aov.tab$
        `Pr(>F)` %>%
        first() %>%
        round(
          digits = 2
        )
    ),
    x = -5,
    y = -5
  ) +
  annotate(
    "text",
    label = glue(
      "P = ",
      ordStat$aov.tab$R2 %>%
        first() %>%
        round(
          digits = 3
        )
    ),
    x = 5,
    y = 5
  ) +

  theme(
    #allsmaller
    legend.text = element_text(
      size = 8
    ),
    legend.key.size = unit(
      0.2, "cm"
    ),
    legend.text.align = 0,
    legend.key.width = unit(
      0.2, "cm"
    ),
    legend.title = element_text(
      size = 8
    ),
    legend.margin = margin(
      l = -5,
      t = -5,
      b = -5,
      r = -5
    )
  )
graph2d


#panel----

graph2c <- cleanData %>%
  ggplot(
    aes(
      x = reorder(
        fam,
        -eval(
          measure2c
        )
      ),
      y = measure2c %>%
        eval(),
      color = as_factor(
        dist
      )
    )
  ) +
  geom_quasirandom() +
  stat_summary(
    fun.data = "median_mad",
    color = "black"
    # size = 1
  ) +
  theme(
    axis.text.x = element_text(
      angle = -45,
      hjust = 0
    ),
    legend.position = "top"
  ) +
  scale_y_log10() +
  scale_color_brewer(
    direction = -1
  ) +
  labs(
    color = "Dist (m)",
    x = "Family",
    y = yAxisLabel2c %>%
      eval()
  ) +

  theme(
    #allsmaller
    legend.text = element_text(
      size = 8
    ),
    legend.key.size = unit(
      0.2, "cm"
    ),
    legend.text.align = 0,
    legend.key.width = unit(
      0.2, "cm"
    ),
    legend.title = element_text(
      size = 8
    ),
    legend.margin = margin(
      l = -5,
      t = -5,
      b = -5,
      r = -5
    )
  )
graph2c



#fullfig----


fig2 <- ggarrange(
  graph2a,
  graph2b,
  graph2c,
  graph2d,
  graph2e,
  graph2f,
  ncol = 3,
  nrow = 2,
  labels = c(
    "a", "b", "c",
    "d", "e", "f"
  )
)


fig2


#save----


ggsave(
  "fig2.pdf",
  fig2,
  path = "figs",
  width = 5,
  height = 3,
  units = "in"
)

ggsave(
  "fig2.png",
  fig2,
  path = "figs",
  width = 5,
  height = 3,
  units = "in"
)
