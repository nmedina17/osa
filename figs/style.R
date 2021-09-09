library(tidyverse)
#geom_quasirandom()
library(ggbeeswarm)


style <- theme_bw() +
  theme()

theme_set(
  style
)


linePlot <- function(
  ...data,
  ..x,
  ..y
) {

  ...data %>%

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
    )
}
