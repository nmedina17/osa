source(here("analysis/clean.R"))
library(tidyverse)
library(rstatix)

plotVarsTbl <- dat %>%
  group_by(dist, plot) %>%
  select(N, W, luz, incl) %>%
  get_summary_stats(type = "median") %>%
  select(-n) %>% #deprecated
  nest(vardata = -variable)


dattaxasum <- dat %>%
  group_by(dist, plot, fam, gen, sp) %>%
  select(dap, h, spg, kg17) %>%
  get_summary_stats(type = "median_mad") #-n
