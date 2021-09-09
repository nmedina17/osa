library(tidyverse)
#geom_quasirandom()
library(ggbeeswarm)
library(here)
source(
  here(
    "analysis/statsTbl.R"
  )
)


style <- theme_bw() +
  theme()

theme_set(
  style
)

graphPointColor <- "black"
graphPointSize <- 3


lineGraph <- function(
  #graphData
  #varData1
  ..varData,
  ..x,
  ..y,
  ..xlab,
  ..ylab
) {

  ..varData %>%

    ggplot(
      aes(
        x = {
          ..x %>%
            eval()
        },
        y = {
          ..y %>%
            eval()
        }
      )
    ) +

    geom_quasirandom(
      color = "black",
      shape = 21,
      fill = "white",
      size = 2
    ) +

    labs(
      x = xlab,
      y = ylab
    ) +

    geom_point(
      data = {
        graphData %>%
          select(
            "varData1"
          ) %>%
          unnest()
      },
      color = graphPointColor,
      size = graphPointSize
    )
}
