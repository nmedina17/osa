library(here);
source(
  here("analysis/clean.R"))
library(rstatix);
library(vegan) #diversity()


#4vegan
view(spTbl) <- rawData %>%
  #groups
  add_count(gen, plot,
            name = "n_gen",
            sort = T) %>%
  #leavedups
  mutate(
    newID = row_number()) %>%
  pivot_wider(
    id_cols = c(newID),
    names_from = gen,
    values_from = n_gen,
    #rmwarn
    #values_fn = length,
    values_fill = 0) %>%
  select(-newID)

plotVarsTbl <- rawData %>%
  mutate(
    #alreadygrouped
    "entropy" = spTbl %>%
      diversity()) %>%
  #filter(entropy > 0) #debug
  group_by(dist, plot) %>%
  mutate(
    "richness" = n_distinct(sp)) %>%

  select(luz, incl, #abiotic
         dap, h, spg, kg17,
         richness, entropy) %>%
  get_summary_stats(
    type = "median_mad") %>%
  nest(varData = -variable)

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
