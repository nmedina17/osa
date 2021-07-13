library(here); source(here("analysis/clean.R"))
library(tidyverse); library(rstatix);
library(vegan) #diversity()

spTbl <- rawData %>% #4vegan
  add_count(gen, plot, name = "n_gen", sort = T) %>%
  pivot_wider(names_from = gen, values_from = n_gen,
              values_fn = length, #rmwarn
              values_fill = 0) %>% #noid_col,leavedups
  select(-c(ID, dist, rep:kg17)) #keepplot
#bugeats2rows...

plotVarsTbl <- rawData %>%
  mutate(entropy = spTbl %>% diversity())
  group_by(dist, plot) %>%
  mutate(richness = n_distinct(sp))
length(plotVarsTbl$richness)

  select(luz, incl, #abiotic
         dap, h, spg, kg17, #biotic
         richness) %>% #taxonomic
  get_summary_stats(type = "median_mad") %>%
  nest(varData = -variable)

treeVarsTbl <- rawData %>%
  group_by(dist, plot, fam, gen, sp) %>%
  select(dap, h, spg, kg17) %>%
  get_summary_stats(type = "median_mad") %>%
  mutate(median1 = log10(median + 1)) %>% #4statTests
  nest(varData = -variable)

#funcw/formulahard
