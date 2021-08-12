library(here);
source(
  here("analysis/clean.R")
)
library(rstatix);
library(vegan) #diversity()



#4vegan
spTbl <- cleanData %>%
  add_count(
    gen,
    plot, #group
    name = "n_gen",
    sort = T
    ) %>%
  pivot_wider(
    id_cols = plot,
    names_from = gen,
    values_from = n_gen,
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

  group_by(
    dist,
    plot
    ) %>%
  mutate(
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
  mutate(
    "varData1" = varData %>%
      modify(
        ~ .x %>%
          group_by(
            dist
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
  )



#rid?
treeVarsTbl <- cleanData %>%
  group_by(
    dist,
    plot,

    fam,
    gen,
    sp
    ) %>%
  select(
    dap,
    h,
    spg,
    kg17
    ) %>%
  get_summary_stats(
    type = "median_mad"
    ) %>%
  #4statTests
  mutate(
    median1 =
      log10(
        median + 1
        )
    ) %>%

  nest(
    varData = -variable
    )

#funcw/formulahard



#ordinate

commTbl <- spTbl %>%
  select(
    -c(
      entropy,
      plot
    )
  )

distMat = vegdist(
  commTbl
)

metaTbl <- cleanData %>%
  distinct(
    plot,
    dist
  )


ordModel <- distMat ~ dist

ordStat = adonis(
  ordModel,
  metaTbl,
  99999
)



#taxa

# which sp only have 2 dists?
spStatTbl <- cleanData %>%
  select(
    dist,
    plot,
    gen
  ) %>%
  add_count(
    gen,
    plot,
  ) %>%
  distinct(
    gen,
    plot,
    .keep_all = T
  ) %>%
  nest(
    "varData" = -gen
  ) %>%

  #QCshapiro
  mutate(,
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
  )
