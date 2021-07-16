library(here);
source(
  here("analysis/clean.R"))
library(rstatix);
library(vegan) #diversity()


#4vegan
spTbl <- rawData %>%
  add_count(gen,
            plot, #group
            name = "n_gen",
            sort = T) %>%
  pivot_wider(
    id_cols = plot,
    names_from = gen,
    values_from = n_gen,
    #rmwarn
    values_fn = length,
    values_fill = 0) %>%
  mutate(
    "entropy" = diversity(.))


plotVarsTbl <- rawData %>%
  left_join(.,
            spTbl %>%
              select(plot,
                     entropy),
            by = "plot",
            copy = T) %>%
  group_by(dist, plot) %>%
  mutate(
    "richness" = n_distinct(sp)) %>%
  select(luz, incl, #abiotic
         dap, h, spg, kg17,
         richness, entropy) %>%
  get_summary_stats(
    type = "median_mad") %>%
  nest("varData" = -variable)


#rid?
treeVarsTbl <- rawData %>%
  group_by(dist, plot,
           fam, gen, sp) %>%
  select(dap, h, spg, kg17) %>%
  get_summary_stats(
    type = "median_mad") %>%
  #4statTests
  mutate(
    median1 = log10(median + 1)) %>%
  nest(varData = -variable)

#funcw/formulahard
