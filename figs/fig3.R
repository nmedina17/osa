library(
  here
)
i_am(
  "figs/fig3"
)
taxRank <- quote(
  mainDispersal
)
source(
  here(
    "figs/stats.R"
  )
)
# source(
#   here(
#     "figs/fig1.R"
#   )
# )
library(oir)


#plan:
#dist-x-Xkernels4dispersalModes

measure31 <- quote(
  wind
)
yAxisLabel1 <- quote(
  "Aboveground biomass (kg)"
)
varForm1 <- {
  taxMetric %>%
    eval()
} ~ dist * mainDispersal
xAxisLabel <- quote(
  "Distance to secondary forest edge (m)"
)
statData1 <- taxaResultsTbl %>%
  rename(
    "variable" = "mainDispersal"
  ) %>%

  filter(
    !is.na(
      variable
    )
  ) #waterSignif
# underData <-



#panel

graph1 <- statData1 %>%
  dotGraph(
    measure31,
    varForm1[[3]][[2]],
    varForm1[[2]],
    xAxisLabel,
    yAxisLabel1
    # ..useGroups = varForm1[[3]][[3]]
) +
  scale_y_log10()

graph1
