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
#.GlobalEnv::plotResultsTbl
library(glue)
#labs


#userInput
measure1 <- quote(
  luz
)
yAxisLabel1 <- quote(
  "Canopy light (%)"
)
measure2 <- quote(
  h
)
yAxisLabel2 <- quote(
  "Tree height (m)"
)
measure3 <- quote(
  spg
)
yAxisLabel3 <- quote(
  expression(
    paste(
      "Wood density (cm"^3,
      " g"^-1,
      ")"
    )
  )
)
measure4 <- quote(
  kg17
)
yAxisLabel4 <- quote(
  "Aboveground biomass (kg)"
)

varForm <- median ~ dist
xAxisLabel <- quote(
  "Distance to secondary forest edge (m)"
)
# statData <- plotResultsTbl1
underData <- cleanData




#panel

graph1 <-  plotResultsTbl1 %>%
  dotGraph(
    measure1,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel1,
    ..addGroups = T
  )

graph1



#panel

graph2 <- plotResultsTbl1 %>%
  dotGraph(
    measure2,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2,
    underData,
    ..addGroups = T
  )

graph2



#panel

graph3 <- plotResultsTbl1 %>%
  dotGraph(
    measure2,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2
  ) +

  geom_point(
    data = {
      plotResultsTbl1 %>%
        filter(
          variable == measure3
        ) %>%
        select(
          "varData1"
        ) %>%
        unnest()
    },
    color = graphPointColor,
    size = graphPointSize
  ) %>%


  #move
  append_layers(
    geom_quasirandom(
      data = cleanData,
      aes(
        y = measure3
      ),
      color = "gray",
      size = 1
    ),

    position = "bottom"
  )

graph3



#panel

graph4 <- plotResultsTbl1 %>%
  dotGraph(
    measure2,
    varForm[[3]],
    varForm[[2]],
    xAxisLabel,
    yAxisLabel2
  ) +

  geom_point(
    data = {
      plotResultsTbl1 %>%
        filter(
          variable == measure4
        ) %>%
        select(
          "varData1"
        ) %>%
        unnest()
    },
    color = graphPointColor,
    size = graphPointSize
  ) %>%


  #move
  append_layers(
    geom_quasirandom(
      data = cleanData,
      aes(
        y = measure4
      ),
      color = "gray",
      size = 1
    ),

    position = "bottom"
  ) +

  scale_y_continuous(
    trans = "log10"
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
