library(here); source(here("analysis/clean.R"))
library(tidyverse); library(rstatix)

plotVarsTbl <- rawData %>%
  group_by(dist, plot) %>%
  mutate(richness = n_distinct(sp)) %>%
  select(luz, incl, #abiotic
         dap, h, spg, kg17, #biotic
         richness) %>% #taxonomic
  get_summary_stats(type = "median") %>%
  nest(varData = -variable)

treeVarsTbl <- rawData %>%
  group_by(dist, plot, fam, gen, sp) %>%
  select(dap, h, spg, kg17) %>%
  get_summary_stats(type = "median_mad") %>%
  mutate(median1 = log10(median + 1)) %>% #4statTests
  nest(varData = -variable)

#funcw/formulahard
