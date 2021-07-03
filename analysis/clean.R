library(here) #fromprojdir
library(vroom) #loadastbl
library(tidyverse)
library(BIOMASS)

rawData <- vroom(here("data/raw/osa.csv")) %>%
  filter(!is.na(dist)) %>% #gen!=na,sp!=sp.?
  mutate(fixedNames = correctTaxo(genus = gen,
                                  species = sp,
                                  useCache = T)) %>%
  mutate(fam = getTaxonomy(gen)$family) %>% #useful
  mutate(spg = getWoodDensity(genus = gen,
                              species = sp,
                              family = fam,
                              stand = plot)$meanWD) %>%
  mutate(kg14 = 0.0673 * (spg * dap^2 * h)^0.976) %>%
  mutate(kg17 = computeAGB(D = dap, WD = spg, H = h) * 1000)

#attic--newfams
for (name in rawData$fam) {
  #if (name == "Bombacaceae" |
  #name == "Sterculiaceae") {
  #name <- "Malvaceae"
  #}
  #if (name == "Cecropiaceae") {
  #name <- "Urticaceae"
  #}
}
# both fam methods do seem equal
