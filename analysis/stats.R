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
# taxRank <- quote(gen)



# Species tbl ----


#4vegan
spTbl <- cleanData %>% filter(!is.na(dist | plot)) %>%
  add_count(
    {{taxRank}},
    plot, #group
    name = "n_tax",
    sort = T
    ) %>%
  pivot_wider(
    id_cols = plot,
    names_from = {{taxRank}},
    values_from = n_tax,
    #rmwarn
    values_fn = length,
    values_fill = 0
    ) %>%
  # select(!`-`) %>% #sig?
  mutate("entropy" = diversity(.))



# centers ----

cleanDataPlotVars <- cleanData %>%
  left_join(spTbl %>% select(plot, entropy), by = "plot", copy = T) %>%

  ## plots ----

  group_by(
    dist,
    # mainDispersal,
    plot
  ) %>%
  mutate(
    "stems" = n(), "richness" = n_distinct(sp),
    "hmax" = max(h), "dapmax" = max(dap)
  ) %>%
  select(
    #abiotic
    luz, incl,

    #biotic
    dap, h, spg, kg17, tissue.c, hmax, dapmax,

    #community
    stems, richness, entropy
  ) #%>%


plotVarsTbl <- cleanDataPlotVars |>
  rstatix::get_summary_stats(type = "median_mad") %>%
  left_join(cleanDataPlotVars |> group_by(plot) |>
              summarize(across(.fns = median)) |>
              select(where(~ !all(is.na(.x)))),
            by = c("plot", "dist")) |>
  tidyr::nest("varData" = -variable) %>%

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
          # summarize(median = median(median), mad = mad(median)) #|>
          summarize(across(where(is.numeric), ~ median(.x)))
          # left_join(cleanDataPlotVars, by = "dist")
      )
  ) #%>%

  #hist,parms--SAVE-LATER
  # mutate(
  #   "sys" = varData %>%
  #     modify(
  #       ~ .x %>% group_by(dist) %>%
  #         pull(kg17) %>% # mainMetric, maybe make variable later
  #         hist(breaks = c(10000000, 1000000, 100000, 1000, 100, 10, 1), #log10
  #                   plot = F) %>%
  #         {.$counts + 1} #%>% as_tibble_col("g")
  #     ),
  #   varData = varData %>% modify(~.x %>% mutate(g = g))
  # ) %>%
  #
  # rename("g" = sys) #%>%
  # oir::runFits()



    # "varData0" = cleanData %>%
    #   tidyr::nest("varData0" = everything())#,
    # "sys0" = {
    #   temp <- varData0 %>% tidyr::unnest(everything()) %>% dplyr::distinct();
    #   tempHist <- hist(temp$kg17, plot = F);
    #   tempHistTbl <- dplyr::tibble(tempHist$mids, tempHist$counts)
    # } %>% tidyr::nest("sys0" = everything())
    #   # where(colnames(.x$varData0) == .x$variable))
  # )


# ordinate ----

commTbl <- spTbl %>% select(-c(entropy, plot))
distMat <- vegdist(commTbl)
metaTbl <- cleanData %>% distinct(plot, dist) %>%
  #qc
  filter(!is.na(plot | dist))


ordModel <- distMat ~ dist

ordStat <- vegan::adonis2(
  ordModel,
  metaTbl,
  # 99999
  permutations = 99
)


ordTbl <- commTbl %>% oir::getOrdVarTbl(metaTbl)



# statsTbls----

## taxa----

#user
taxMetric <- quote(kg17)


spStatTbl <- cleanData %>%
  select(dist, plot, {{taxRank}}, {{taxMetric}}) %>%
  add_count({{taxRank}}, plot) %>%
  #collapse
  distinct({{taxRank}}, plot, .keep_all = T) %>%
  nest("varData" = -{{taxRank}}) %>%

  #QCshapiro
  mutate(
    nDist = varData %>% modify(~ .x %>%pull(dist) %>% n_distinct()),
    varN = varData %>% modify(~ .x %>% pull(n) %>% var())
  ) %>% unnest(c(nDist, varN)) %>%
  filter(nDist >= 3, varN > 0) %>%

  mutate(
    "varData1" = varData %>% modify(
      ~ .x %>% group_by(dist) %>%
        summarize("taxMetric" = median({taxMetric %>% eval()}))
    )
  )


#ORGANIZE

## plots----

### linear----


#user
mainMetric <- quote(abs(median + 1)) #incl


#get

mainModel <- {mainMetric %>% eval()} ~ dist


#OGpackage
plotResultsTbl <- plotVarsTbl %>% oir::getStatsTbl(mainModel)


##woodCcheck
# ggplot2::ggplot(plotResultsTbl$varData1[[10]], aes(dist, median)) +
#   geom_point() + geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
#   stat_poly_eq(aes(label = after_stat(p.value.label)))


### quad----

#get2

mainModel2 <- {mainMetric %>% eval()} ~ poly(dist, 2)

plotResultsTbl2 <- plotVarsTbl %>%
  getStatsTbl2(mainModel2) %>%
  addGraph2(mainModel)


### centers----


#### linear----


#get1
plotResultsTbl1 <- plotVarsTbl %>% getStatsTbl1(mainModel)


#### quad----


#get12
plotResultsTbl12 <- plotVarsTbl %>%
  getStatsTbl12(mainModel2) %>%
  addGraph12(mainModel)



##TAXA----


#getTaxa

taxModel <- {taxMetric %>% eval()} ~ dist


taxaResultsTbl <- spStatTbl %>% getStatsTbl(taxModel)


#getTaxa2

taxModel2 <- {taxMetric %>% eval()} ~ poly(dist, 2)

taxaResultsTbl2 <- spStatTbl %>%
  getStatsTbl2(taxModel2) %>%
  addGraph2(taxModel)


#getTaxa1
taxaResultsTbl1 <- spStatTbl %>% getStatsTbl1(taxModel)


#getTaxa12
taxaResultsTbl12 <- spStatTbl %>%
  getStatsTbl12(taxModel2) %>%
  addGraph12(taxModel)



#DISPERSAL
#
mainModel0 <- {taxMetric %>% eval()} ~ dist # * mainDispersal


dispResultsTbl0 <- spStatTbl %>% oir::getStatsTbl(mainModel0)


#sp-biomass
covarModel <- eval(mainMetric) ~ (entropy + 1)
covarResultsTbl <- oir::getStatsTbl(plotVarsTbl, covarModel)
covarResultsTbl1 <- oir::getStatsTbl1(plotVarsTbl, covarModel)

covarModel2 <- eval(mainMetric) ~ poly((entropy + 1), 2)
covarResultsTbl12 <- plotVarsTbl %>%
  getStatsTbl12(covarModel2) %>%
  addGraph12(covarModel)

covarModel0 <- spg ~ (entropy + 1)
