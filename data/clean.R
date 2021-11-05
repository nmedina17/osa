library(here) #fromprojdir
i_am(
  "data/clean.R"
)
library(vroom) #loadastbl
#library(skimr) #glance
library(tidyverse);
library(BIOMASS)


rawData <- vroom(
  here("data/raw/osa.csv")
)

dispersal <- vroom(
  here(
    "data/dispersal.csv"
  )
) %>%
  select(
    fam,
    gen,
    sp,
    mainDispersal
  ) %>%
  filter(
    !is.na(
      gen
    )
  )


cleanData <- function(
  ...rawData
) {

  ...rawData %>%

    #gen!=na,sp!=sp.?
    filter(
      !is.na(
        dist
      )
    ) %>%

    mutate(
      "newName" = correctTaxo(
        genus = gen,
        species = sp,
        useCache = F
      ),

      #useful
      "fam" = getTaxonomy(
        gen
      ) %>%
        pull(
          family
        ),

      "spg" = getWoodDensity(
        genus = gen,
        species = sp,
        family = fam,
        stand = plot
      ) %>%
        pull(
          meanWD
        ),

      "kg17" = computeAGB(
        D = dap,
        WD = spg,
        H = h
      ) * 1000
    ) %>%

    return()
}


cleanData <- rawData %>%
  cleanData() %>%
  full_join(
    dispersal
  ) %>%
  filter(
    !is.na(
      dist
    )
  )

