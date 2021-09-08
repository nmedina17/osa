library(tidyverse)


linePlot <- function(
  ...data,
  ..x,
  ..y
) {

  ...data %>%

    ggplot(
      aes(
        x = ..x,
        y = ..y
    )
  ) +
    theme_classic() +

    geom_point(
      aes(
        color = "gray"
      )
    ) +

    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "black"
      )
    )
}
