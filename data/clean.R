library(here) #fromprojdir
i_am("data/clean.R")
library(vroom) #loadastbl
#library(skimr) #glance
library(tidyverse);
library(BIOMASS)


#import----

##raw----
rawData <- vroom(
  here("data/raw/osa.csv")
)

dispersal <- vroom(
  here("data/dispersal.csv")
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
    ) &
      !is.na(
        mainDispersal
      )
  )

##traits----
traits <- vroom(
  here("data/base/try/try19707.csv")
) %>% #glimpse()
  select(
    AccSpeciesName,
    TraitName,
    OrigValueStr,
    StdValue
  ) %>%

  ###qc----
  mutate(
    StdValue = ifelse(
      is.na(
        StdValue
      ),
      yes = OrigValueStr,
      no = StdValue
    ),
    "gen" = AccSpeciesName %>%
      str_split(
        " "
      ) %>% #list
      modify(
        . %>%
          first()
      ) %>%
      as_vector()
  ) %>%
  select(
    !OrigValueStr
  ) %>%

  pivot_wider(
    id_cols = gen,
    names_from = TraitName,
    values_from = StdValue
  )


##lifeHist----
lifeHist <- vroom(
  here("data/base/lifeHist.csv")
) %>%
  select(
    family,
    genus,
    dispersalMode,
    successionalStage
  ) %>%
  #qc
  filter(
    !is.na(
      successionalStage
    )
  )


##woodC----
woodC <- vroom::vroom(here::here(
  "data/base/glowcad/Doraisami_et_al._2021_Wood_C_Database.csv")) |>
  select(tissue.c, family.resolved, genus.resolved, binomial.resolved)


updateRaw <- function(
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
        stand = plot,
        verbose = F
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


#join----

cleanData <- rawData %>%
  updateRaw() %>%

  mutate(
    #perHa
    kg17 = kg17 / (
      21 * 21
    ) * (
      10000 / 1
    )
  ) %>%

  full_join(dispersal) %>%
  filter(
    !is.na(
      dist
    )
  ) %>%

  full_join(
    lifeHist,
    by = c(
      "gen" = "genus"
    )
  ) %>%

  full_join(
    traits,
    by = "gen"
  ) %>%

  full_join(woodC, by = c("gen" = "genus.resolved")) |>


  filter(
    !is.na(
      dist
    )
  ) %>%

  distinct() %>%

  ##qc----
  rename(
    "DispersalSyndrome" = "Dispersal syndrome"
  ) #%>%

#   mutate(
#     "checkDisp" = DispersalSyndrome %>%
#       str_detect(
#         mainDispersal
#       )
#   )
#
# cleanData %>%
#   filter(
#     checkDisp == F &
#       !is_null(
#         DispersalSyndrome
#       )
#   ) %>%
#   distinct(
#     DispersalSyndrome
#   ) %>%
#
#   #ignore
#   pull()


cleanData$mainDispersal <- cleanData$
  mainDispersal %>%
  as_factor()
# levels(cleanData$mainDispersal)

cleanData$successionalStage <- cleanData$
  successionalStage %>%
  as_factor()


#userStat----


#userInput--parentFile
# taxRank <- quote(
#   gen
# )
