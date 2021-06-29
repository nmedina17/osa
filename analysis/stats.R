source("clean.R")

library(tidyverse)
library(rstatix)

dats <- dat %>%
  group_by(dist, plot) %>%
  get_summary_stats(type = "median_mad")
