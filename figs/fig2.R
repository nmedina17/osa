here::i_am("figs/fig2.R")
#taxMetric,mainMetric
#plotResultsTbl1,taxaResultsTbl1
taxRank <- quote(
  gen
)
source(here::here("analysis/stats.R"))
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


measure2a <- quote(entropy)
yAxisLabel2a <- quote("Diversity (H')")

measure2b <- quote(richness)
yAxisLabel2b <- quote("Richness")

measure2e <- quote(Vochysia)
taxMetricUnit <- "kg"
yAxisLabel2e <- quote(glue(measure2e, " ", " (", taxMetricUnit, " ha^-1", ")"))

measure2f <- quote(Ficus)
yAxisLabel2f <- quote(glue(measure2f, " ", " (", taxMetricUnit, " ha^-1", ")"))

measure2c <- quote(kg17)
yAxisLabel2c <- quote(glue("Biomass (", taxMetricUnit, " ha^-1",")"))


varForm <- {mainMetric %>% eval()} ~ dist
#check.env...
taxForm <- {taxMetric %>% eval()} ~ dist
xAxisLabel <- quote("Distance to edge (m)")
statData2ab <- plotResultsTbl1
statData2ef <- taxaResultsTbl1 %>% rename("variable" = all_of(taxRank))
statData2c <- ordTbl



#panel----

plotResultsTbl1$pval[[2]] <-
  #bug=doublePosition
  plotResultsTbl12$pval[[2 * 2]] %>% last()
graph2a <- statData2ab %>%
  dotGraph(
    measure2a,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2a,
    ..addCenters = T,
    ..addCurve = T,
    ..addPxy = c(1, 0.5)
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
    eval(yAxisLabel2e),
    ..addCenters = T
  ) + scale_y_log10()
graph2e


#panel----

graph2f <- statData2ef %>%
  dotGraph(
    measure2f,
    taxForm[[3]],
    taxForm[[2]],
    xAxisLabel,
    eval(yAxisLabel2f),
    ..addCenters = T,
    ..addLines = T,
    ..addPxy = c(0, 40000)
  )
graph2f
#Ficus2few...


#panel----

graph2d <- ordTbl %>%
  ggplot(aes(x = PC1, y = PC2, color = as_factor(dist))) +
  geom_point() +
  stat_ellipse() +
  theme(legend.position = "top") +
  labs(
    color = "Dist (m)",
    x = glue("PC1 (", ordTbl$PC1_prop[1] * 100, "%)"),
    y = glue("PC2 (", ordTbl$PC2_prop[1] * 100, "%)")
  ) +
  # scale_color_brewer(
  #   type = "div", #"qual"
  #   direction = -1
  # ) +
  scale_color_manual(
    values = c(
      "brown", "green", "blue",
      "orange", "red", "yellow"
    )
  ) +
  annotate(
    "text",
    label = glue(
      "P = ",
      ordStat$`Pr(>F)` %>%
        first() %>% round(2)
    ),
    x = -5, y = -5,
    size = 2
  ) +
  annotate(
    "text",
    label = glue(
      "R2",
      " = ",
      ordStat$R2 %>%
        first() %>% round(2)
    ),
    x = 5, y = 5,
    size = 2
  ) +

  theme(
    #allsmaller
    legend.text = element_text(
      size = 6
    ),
    legend.key.size = unit(0.2, "cm"),
    legend.text.align = 0,
    legend.key.width = unit(0.2, "cm"),
    legend.title = element_text(size = 6),
    legend.margin = margin(l = -5, t = -5, b = -5, r = -5)
  )
graph2d


#panel----

graph2c <- cleanData %>% mutate(fam = str_sub(fam, end = 5)) %>%
  ggplot(
    aes(
      y = reorder(fam, -eval(measure2c) # %>%
          # head(
          #   n = 10
          # )
      ),
      x = measure2c %>% eval(),
      color = as_factor(dist)
    )
  ) +
  geom_quasirandom(size = 0.125, groupOnX = F) +
  stat_summary(fun.data = "median_mad", color = "black", size = 0.25) +
  theme(
    axis.text.y = element_text(size = 3),
    # axis.text.x = element_text(
    #   angle = -45,
    #   hjust = 0,
    #   size = 4
    # ),
    legend.position = "top"
  ) +
  # scale_y_log10() +
  scale_x_continuous(
    trans = "log10",
    labels = trans_format("log10", math_format())
  ) +
  # scale_color_brewer(
  #   direction = -1
  # ) +
  scale_color_manual(
    values = c(
      "brown", "green", "blue",
      "orange", "red", "yellow"
    )
  ) +
  labs(color = "Dist (m)", y = "Family", x = yAxisLabel2c %>% eval()) +

  theme(
    #allsmaller
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.2, "cm"),
    legend.text.align = 0,
    legend.key.width = unit(0.2, "cm"),
    legend.title = element_text(size = 6),
    legend.margin = margin(l = -5, t = -5, b = -5, r = -5)
  )
graph2c



#fullfig----


fig2 <- ggpubr::ggarrange(
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


# ggsave(
#   "fig2.pdf",
#   fig2,
#   path = "figs",
#   width = 5,
#   height = 3,
#   units = "in"
# )
#
# ggsave(
#   "fig2.png",
#   fig2,
#   path = "figs",
#   width = 5,
#   height = 3,
#   units = "in"
# )
