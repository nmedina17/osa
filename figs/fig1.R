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


#userInput
varForm <- median ~ dist


plotVarsTbl %>%
  filter(
    #main
    variable == "luz"
  ) %>%


  pull(
    varData
  ) %>%

  #HERE


  as_tibble() %>%
  linePlot(
    varForm[[2]] %>%
      eval(),
    varForm[[3]] %>%
      eval()
  )
