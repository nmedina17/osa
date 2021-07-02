source(here("analysis/clean.R"))
library(tidyverse)
library(rstatix)

plotVarsTbl <- dat %>%
  group_by(dist, plot) %>%
  select(luz, incl, #abiotic
         dap, h, spg, kg17) %>%
  get_summary_stats(type = "median") %>%
  nest(vardata = -variable)


treeVarsTbl <- dat %>%
  group_by(dist, plot, fam, gen, sp) %>%
  select(dap, h, spg, kg17) %>%
  #mutate(richness?) #maybevegan
  get_summary_stats(type = "median_mad") %>%
  nest(vardata = -variable)

