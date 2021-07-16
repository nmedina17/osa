library(here) #fromprojdir
library(vroom) #loadastbl
#library(skimr) #glance
library(tidyverse);
library(BIOMASS)

rawData <- vroom(
  here("data/raw/osa.csv")) %>%
  #gen!=na,sp!=sp.?
  filter(
    !is.na(dist)) %>%
  mutate(
    "newName" = correctTaxo(genus = gen,
                            species = sp,
                            useCache = T),
    #useful
    "fam" = getTaxonomy(gen) %>%
      pull(family),
    "spg" = getWoodDensity(genus = "gen",
                           species = "sp",
                           family = "fam",
                           stand = plot) %>%
      pull(meanWD),
    "kg17" = computeAGB(D = dap,
                        WD = spg,
                        H = h) * 1000)
