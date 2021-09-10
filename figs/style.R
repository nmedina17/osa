library(tidyverse)
#geom_quasirandom()
library(ggbeeswarm)
library(here)
# source(
#   here(
#     "analysis/statsTbl.R"
#   )
# )
library(gginnards)
#append_layers()


style <- theme_bw() +
  theme()

theme_set(
  style
)


dotGraph <- function(
  ..varData,
  # $variable
  # $varData
  # $varData1
  #pval
  ..var,
  ..x,
  ..y,
  ..xlab,
  ..ylab,
  ..addGroups = F,
  ..y1 = ..y,
  ..cleanData = NULL
) {

  graph <- ..varData %>%

    filter(
      variable == ..var
    ) %>%
    select(
      "varData"
    ) %>%
    unnest() %>%


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
      x = {
        ..xlab %>%
          eval()
        },
      y = {
        ..ylab %>%
          eval()
      }
    ) +

    annotate(
      "text",
      label = glue(
        "P = ",
        {
          ..varData %>%
            filter(
              variable == ..var
            ) %>%
            pull(
              "pval"
            ) %>%
            last() %>%
            # as_vector() %>%
            as.double() %>%
            round(
              3
            )
        }
      ),
      x = 1,
      y = 1,
      hjust = 0
    )


  #toggles


  graph <- if(
    is.null(
      ..cleanData
    )
  ) {

    graph

  } else {

    graph <- graph %>%

      #move
      append_layers(

        geom_quasirandom(
          data = ..cleanData,
          aes(
            y = {
              ..var %>%
                eval()
            }
          ),
          color = "gray",
          size = 1
        ),

        position = "bottom"
      )
  }


  graph <- if(
    ..addGroups == T
  ) {

    graph <- graph +

      geom_point(
        data = {
          ..varData %>%
            filter(
              variable == ..var
            ) %>%
            select(
              "varData1"
            ) %>%
            unnest()
        },
        color = "black",
        size = 3,
        aes(
          y = {
            ..y1 %>%
              eval()
          }
        )
      )

  } else {

    graph
  }
}
