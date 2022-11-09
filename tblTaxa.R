source(here::here("analysis/stats.R")); library(tidyverse)
taxaResListTbl1 <- taxaResultsTbl1 |>
  filter(isModelOK) |> select(gen, statPrint) |> unnest() |>
  filter(term == "dist") |> select(gen, estimate, std.error, statistic, p.value) |>
  filter(gen != "-") |> arrange(estimate) #|> filter(p.value < 0.125) #onlyFicus!
# taxaTbl <-
taxaResListTbl1 |> knitr::kable() |>
  kableExtra::kable_minimal()
  kableExtra::row_spec(1, bold = T) #pause.
