# Refs----


here::i_am("analysis/stats.R")

#run1----

source(here::here("data/clean.R"))

# source(
#   here("analysis/statsTbl.R")
# )
library(tidyverse)
library(rstatix); library(vegan) #diversity()
library(oir) #local
library(ggbeeswarm); library(ggpmisc)



# #userInput--parentFile
# taxRank <- quote(
#   gen
# )



# Species tbl ----


#4vegan
spTbl <- cleanData %>%
  filter(
    !is.na(
      dist |
        plot
    )
  ) %>%
  add_count(
    {{
      taxRank
    }},
    plot, #group
    name = "n_tax",
    sort = T
    ) %>%
  pivot_wider(
    id_cols = plot,
    names_from = {{
      taxRank
    }},
    values_from = n_tax,
    #rmwarn
    values_fn = length,
    values_fill = 0
    ) %>%
  mutate(
    "entropy" =
      diversity(
        .
      )
  )



# centers ----

plotVarsTbl <- cleanData %>%
  left_join(
    .,
    spTbl %>%
      select(
        plot,
        entropy
      ),
    by = "plot",
    copy = T
  ) %>%

  ## plots ----

  group_by(
    dist,
    # mainDispersal,
    plot
  ) %>%
  mutate(
    "stems" = n(),
    "richness" =
      n_distinct(
        sp
      )
  ) %>%
  select(
    #abiotic
    luz,
    incl,

    #biotic
    dap,
    h,
    spg,
    kg17,

    #community
    stems,
    richness,
    entropy
  ) %>%

  get_summary_stats(
    type = "median_mad"
  ) %>%
  nest(
    "varData" = -variable
  ) %>%

  #nextlevel

  ## dists ----

  mutate(
    "varData1" = varData %>%
      modify(
        ~ .x %>%
          group_by(
            dist
            # mainDispersal
          ) %>%
          summarize(
            median = median(
              median
            ),
            mad = mad(
              median
            )
          )
      )
  ) %>%

  # hist parms
  mutate(
    "sys" = varData %>%
      modify(
        ~ .x %>%

          pull(
            median
          ) %>%
          hist(
            .,
            plot = F
          )
      )
  )



# ordinate ----

commTbl <- spTbl %>%
  select(
    -c(
      entropy,
      plot
    )
  )

distMat <- vegdist(
  commTbl
)

metaTbl <- cleanData %>%
  distinct(
    plot,
    dist
  ) %>%

  #qc
  filter(
    !is.na(
      plot |
        dist
    )
  )


ordModel <- distMat ~ dist

ordStat <- vegan::adonis2(
  ordModel,
  metaTbl,
  # 99999
  permutations = 99
)


ordTbl <- commTbl %>%
  oir::getOrdVarTbl(
    metaTbl
  )



# statsTbls----

## taxa----

#user
taxMetric <- quote(
  kg17
)


spStatTbl <- cleanData %>%
  select(
    dist,
    plot,
    {{
      taxRank
    }},
    {{
      taxMetric
    }}
  ) %>%
  add_count(
    {{
      taxRank
    }},
    plot,
  ) %>%
  #collapse
  distinct(
    {{
      taxRank
    }},
    plot,
    .keep_all = T
  ) %>%
  nest(
    "varData" = -{{
      taxRank
    }}
  ) %>%

  #QCshapiro
  mutate(
    nDist = varData %>%
      modify(
        ~ .x %>%
          pull(
            dist
          ) %>%
          n_distinct()
      ),
    varN = varData %>%
      modify(
        ~ .x %>%
          pull(
            n
          ) %>%
          var()
      )
  ) %>%
  unnest(
    c(
      nDist,
      varN
    )
  ) %>%
  filter(
    nDist >=
      3,
    varN >
      0
  ) %>%

  mutate(
    "varData1" =
      varData %>%
      modify(
        ~ .x %>%
          group_by(
            dist
          ) %>%
          summarize(
            "taxMetric" = median(
              {
                taxMetric %>%
                  eval()
              }
            )
          )
      )
  )


#ORGANIZE

## plots----

### linear----


#user
mainMetric <- quote(
  median
)


#get

mainModel <- {
  mainMetric %>%
    eval()
} ~ dist


#OGpackage
plotResultsTbl <- plotVarsTbl %>%
  getStatsTbl(
    mainModel
  )


### quad----

#get2

mainModel2 <- {
  mainMetric %>%
    eval()
} ~ poly(
  dist,
  2
)

plotResultsTbl2 <- plotVarsTbl %>%
  getStatsTbl2(
    mainModel2
  ) %>%
  addGraph2(
    mainModel
  )


### centers----


#### linear----


#get1
plotResultsTbl1 <- plotVarsTbl %>%
  getStatsTbl1(
    mainModel
  )


#### quad----


#get12
plotResultsTbl12 <- plotVarsTbl %>%
  getStatsTbl12(
    mainModel2
  ) %>%
  addGraph12(
    mainModel
  )



##TAXA----


#getTaxa

taxModel <- {
  taxMetric %>%
    eval()
} ~ dist


taxaResultsTbl <- spStatTbl %>%
  getStatsTbl(
    taxModel
  )


#getTaxa2

taxModel2 <- {
  taxMetric %>%
    eval()
} ~ poly(
  dist,
  2
)

taxaResultsTbl2 <- spStatTbl %>%
  getStatsTbl2(
    taxModel2
  ) %>%
  addGraph2(
    taxModel
  )


#getTaxa1
taxaResultsTbl1 <- spStatTbl %>%
  getStatsTbl1(
    taxModel
  )


#getTaxa12
taxaResultsTbl12 <- spStatTbl %>%
  getStatsTbl12(
    taxModel2
  ) %>%
  addGraph12(
    taxModel
  )



#DISPERSAL
#
mainModel0 <- {
  taxMetric %>%
    eval()
} ~ dist # * mainDispersal


dispResultsTbl0 <- spStatTbl %>%
  oir::getStatsTbl(
    mainModel0
  )

