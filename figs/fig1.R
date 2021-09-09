library(here)
source(
  here(
  "figs/style.R"
  )
)
source(
  here(
    "analysis/statsTbl.R"
  )
)
library(ggpmisc)
#stat_poly_eq()
library(gginnards)
#shift_layers()


#userInput
varForm <- median ~ dist
plotPointColor <- "black"
plotPointSize <- 3
xAxisLabel <- "Distance to secondary forest edge (m)"



#panel


plotLightData <- plotVarsTbl %>%
  filter(
    #main
    variable == "luz"
  )


plotLight <- plotLightData %>%
  select(
    varData
  ) %>%
  unnest() %>%


  linePlot(
    varForm[[3]],
    varForm[[2]]
  ) +


  geom_point(
    data = {
      plotLightData %>%
        select(
          varData1
        ) %>%
        unnest()
    },
    color = plotPointColor,
    size = plotPointSize
  ) +

  labs(
    x = xAxisLabel,
    y = "Canopy light (%)"
  ) +

  stat_poly_eq(
    data = {
      plotLightData %>%
        select(
          varData1
        ) %>%
        unnest()
    },
    aes(
      label = stat(
          p.value.label
      )
    )
  )


plotLight



#panel


plotHeightData <- plotVarsTbl %>%
  filter(
    #main
    variable == "h"
  )


plotHeight <- plotHeightData %>%
  select(
    varData
  ) %>%
  unnest() %>%


  linePlot(
    varForm[[3]],
    varForm[[2]]
  ) %>%


  #move
  append_layers(
    geom_quasirandom(
      data = {
        cleanData %>%
          select(
            h,
            dist
          )
        },
      aes(
        y = h
      ),
      color = "gray",
      size = 1
    ),

    position = "bottom"
  ) +

  geom_point(
    data = {
      plotHeightData %>%
        select(
          varData1
        ) %>%
        unnest()
    },
    color = plotPointColor,
    size = plotPointSize
  ) +

  labs(
    x = xAxisLabel,
    y = "Tree height (m)"
  ) +

  stat_poly_eq(
    data = {
      plotHeightData %>%
        select(
          varData1
        ) %>%
        unnest()
    },
    aes(
      label = stat(
        p.value.label
      )
    )
  )


plotHeight



#panel


plotWoodData <- plotVarsTbl %>%
  filter(
    #main
    variable == "spg"
  )


plotWood <- plotWoodData %>%
  select(
    varData
  ) %>%
  unnest() %>%


  linePlot(
    varForm[[3]],
    varForm[[2]]
  ) %>%


  #move
  append_layers(
    geom_quasirandom(
      data = {
        cleanData %>%
          select(
            spg,
            dist
          )
      },
      aes(
        y = spg
      ),
      color = "gray",
      size = 1
    ),

    position = "bottom"
  ) +

  geom_point(
    data = {
      plotWoodData %>%
        select(
          varData1
        ) %>%
        unnest()
    },
    color = plotPointColor,
    size = plotPointSize
  ) +

  labs(
    x = xAxisLabel,
    y = expression(
      paste(
        "Wood density (cm"^3,
        " g"^-1,
        ")"
      )
    )
  ) +

  stat_poly_eq(
    data = {
      plotWoodData %>%
        select(
          varData1
        ) %>%
        unnest()
    },
    aes(
      label = stat(
        p.value.label
      )
    )
  )


plotWood



#panel


plotMassData <- plotVarsTbl %>%
  filter(
    #main
    variable == "kg17"
  )


plotMass <- plotMassData %>%
  select(
    varData
  ) %>%
  unnest() %>%


  linePlot(
    varForm[[3]],
    varForm[[2]]
  ) %>%


  #move
  append_layers(
    geom_quasirandom(
      data = {
        cleanData %>%
          select(
            kg17,
            dist
          )
      },
      aes(
        y = kg17
      ),
      color = "gray",
      size = 1
    ),

    position = "bottom"
  ) +

  geom_point(
    data = {
      plotMassData %>%
        select(
          varData1
        ) %>%
        unnest()
    },
    color = plotPointColor,
    size = plotPointSize
  ) +

  labs(
    x = xAxisLabel,
    y = expression(
      paste(
        "Aboveground biomass (kg)"
      )
    )
  ) +

  scale_y_continuous(
    trans = "log10"
  ) +

  stat_poly_eq(
    data = {
      plotMassData %>%
        select(
          varData1
        ) %>%
        unnest()
    },
    aes(
      label = paste(
        stat(
          p.value.label
        ),
        stat(
          adj.rr.label
        ),
        sep = "~~~"
      )
    )
  ) +

  stat_smooth(
    method = "lm",
    color = "black",
    lty = "dashed"
  )


plotMass



fig1 <- ggarrange(
  plotMass,
  plotHeight,
  plotWood,
  plotLight,
  nrow = 2,
  ncol = 2
)

fig1
