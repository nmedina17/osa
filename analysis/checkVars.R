library(here);
source(
  here("analysis/stats.R"))
library(tidymodels)
library(ggbeeswarm) #quasirandom()
library(ggpmisc) #stat_poly_eq()
library(ggpubr) #ggarrange()
source(
  here("analysis/disfit.R")) #disfit()


checkVars <- function(
  .nestedVarDataTbl,
  .formula
  ) {

  .nestedVarDataTbl %>%
    mutate(
      "statTest" = varData %>%
        modify(
          ~ .x %>%
            lm(formula = .formula)
          ),
      statPrint = statTest %>%
        modify(
          ~ .x %>%
            tidy() %>%
            full_join(.,
                      .x %>%
                        glance()
                      ) %>%
            filter(
              !is.na(term)
              )
          ),

      normalTest = statTest %>%
        modify(
          ~ .x %>%
            augment() %>%
            pull(.resid) %>%
            shapiro_test()
          ),
      #OGdata
      varianceTest = varData %>%
        modify(
          ~ .x %>%
            levene_test(
              formula =
                .formula[[2]] %>%
                eval() ~
                .formula[[3]] %>%
                eval() %>%
                as_factor()
              )
          ),
      "isNormal" = normalTest %>%
        modify(
          ~ if_else(
            .x$p.value > 0.055,
            T,F)
          ),
      "isHomosced" = varianceTest %>%
        modify(
          ~ if_else(
            .x$p > 0.055,
            T, F)
          )
    ) %>%
    #list2vec
    unnest(
      c(isNormal, isHomosced)
      ) %>%
    mutate(
      "isModelOK" = if_else(
        isNormal & isHomosced,
        T, F),
      "isSignif" = statPrint %>%
        modify_if(
          isModelOK,
          ~ if_else(
            #mainterm
            .x$p.value[2] < 0.105,
            T,F),
          .else = ~ NA)
      ) %>%
    unnest(isSignif) %>%
    #nonnormalstats
    #nestedmodifyishard
    mutate(
      "statTestPois" = varData %>%
        modify_if(
          !isModelOK,
          ~ .x %>%
            glm(
              formula = mainModel,
              family = poisson()
              ) %>%
            summary(),
          .else = ~ NA),
      "statTestGamma" = varData %>%
        modify_if(
          !isModelOK,
          ~ .x %>%
            glm(
              formula = mainModel,
              family = Gamma()
              ) %>%
            summary(),
          .else = ~ NA),
      poisAIC = statTestPois %>%
        modify_if(
          !isModelOK,
          ~ .x$aic),
      gammaAIC = statTestGamma %>%
        modify_if(
          !isModelOK,
          ~ .x$aic),
      poisPval = statTestPois %>%
        modify_if(
          !isModelOK,
          ~ .x$coefficients[8]),
      gammaPval = statTestGamma %>%
        modify_if(
          !isModelOK,
          ~ .x$coefficients[8])) %>%
    unnest(
      c(poisAIC, gammaAIC,
             poisPval, gammaPval)
      ) %>%
    mutate(
      "pickAIC" = pmin(poisAIC,
                       gammaAIC)
      ) %>%
    mutate(
      graph = varData %>%
        modify(
          ~ .x %>%
            ggplot(
              aes(
                x = dist,
                y = median)
              ) +
            geom_quasirandom() +
            geom_smooth(method = "lm") +
            stat_poly_eq(
              formula = y ~ x,
              parse = T,
              aes(
                label = stat(
                  ..p.value.label..)
                )
              )
          )
    )
  }
