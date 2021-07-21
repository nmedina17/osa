library(here);
source(
  here("analysis/stats.R")
)
library(tidymodels) #glance()
library(ggbeeswarm) #quasirandom()
library(ggpmisc) #stat_poly_eq()
library(ggpubr) #ggarrange()
source(
  here("analysis/disfit.R")
) #disfit()


getStatsTbl <- function(
  ...nestedVarDataTbl,
  #user's
  ...formula
) {

  ...nestedVarDataTbl %>%
    statFitTbl(
      ...formula
    ) %>%
    addStatEval(
      ...formula
    ) %>%

    #nonnormaldistr
    addStatFitNon(
      ...formula
    ) %>%
    addStatEvalNon() %>%

    addStatGraph(
      ...formula
    ) %>%

  return()
}



statFitTbl <- function(
  ...nestedVarDataTbl,
  ....formula
  ) {

  ...nestedVarDataTbl %>%

    mutate(
      "statTest" =
        varData %>%
        modify(
          ~ .x %>%
            lm(
              formula = ....formula
              )
          ),
      statPrint =
        statTest %>%
        modify(
          ~ .x %>%
            tidy() %>%
            full_join(
              .,
              .x %>%
                glance()
              ) %>%
            filter(
              !is.na(
                term
                )
              )
          )
    ) %>%

    return()
}

#wrapper4consistency"-2"
statFitTbl2 <- function(
  ...nestedVarDataTbl,
  ....formula2
) {
  statFitTbl(
    ...nestedVarDataTbl,
    ....formula2)
}


addStatEval <- function(
  ...statFitTbl,
  ....formula
  ) {

  ...statFitTbl %>%

    mutate(
      normalTest =
        statTest %>%
        modify(
          ~ .x %>%
            residuals() %>%
            shapiro_test()
          ),
      #OGdata
      varianceTest =
        varData %>%
        modify(
          ~ .x %>%
            levene_test(
              formula =
                ....formula[[2]] %>%
                eval() ~
                ....formula[[3]] %>%
                eval() %>%
                as_factor()
            )
        ),
      "isNormal" =
        normalTest %>%
        modify(
          ~ if_else(
            .x$
              p.value >
              0.055,
            T, F
            )
          ),
      "isHomosced" =
        varianceTest %>%
        modify(
          ~ if_else(
            .x$
              p >
              0.055,
            T, F
            )
          )
    ) %>%
    #list2vec
    unnest(
      c(
        isNormal,
        isHomosced
        )
      ) %>%

    mutate(
      "isModelOK" =
        if_else(
          isNormal &
            isHomosced,
        T, F
          ),
      "isSignif" =
        statPrint %>%
        modify_if(
          isModelOK,
          ~ if_else(
            #mainterm
            .x$
              p.value[2] <
              0.105,
            T, F
            ),
          .else = ~ NA
          )
      ) %>%
    unnest(
      isSignif
      ) %>%

    return()
}


addStatEval2 <- function(
  ...statFitTbl2,
  ....formula2
) {

  ...statFitTbl2 %>%

    mutate(
      curve =
        statPrint %>%
        modify(
          ~ .x %>%
            filter(
              term !=
                "(Intercept)"
            ) %>%
            pull(
              term
            )
        ),
      "pvals" =
        statPrint %>%
        modify(
          ~ .x %>%
            filter(
              term !=
                "(Intercept)"
            ) %>%
            pull(
              p.value
            )
        ),
      "isSignif2" =
        pvals %>%
        modify(
          ~ if_else(
            .x <
              0.055,
            T, F
          )
        )
    ) %>%

    return()
}


#nonnormalstats
#nestedmodifyishard

addStatFitNon <- function(
  ...statEvalTbl,
  ....formula
) {

  ...statEvalTbl %>%

    mutate(
      "statTestPois" =
        varData %>%
        modify_if(
          !isModelOK,
          ~ .x %>%
            glm(
              formula = ....formula,
              family = poisson()
              ) %>%
            summary(),
          .else = ~ NA
          ),
      "statTestGamma" =
        varData %>%
        modify_if(
          !isModelOK,
          ~ .x %>%
            glm(
              formula =
                mainModel,
              family =
                Gamma()
              ) %>%
            summary(),
          .else = ~ NA
          )
    ) %>%

    return()
}


addStatEvalNon <- function(
  ...statFitNonTbl
) {

  ...statFitNonTbl %>%

    #getvals
    mutate(
      poisAIC =
        statTestPois %>%
        modify_if(
          !isModelOK,
          ~ .x$
            aic
        ),
      gammaAIC =
        statTestGamma %>%
        modify_if(
          !isModelOK,
          ~ .x$
            aic
        ),
      poisPval =
        statTestPois %>%
        modify_if(
          !isModelOK,
          ~ .x$
            coefficients[8]
        ),
      gammaPval =
        statTestGamma %>%
        modify_if(
          !isModelOK,
          ~ .x$
            coefficients[8]
        )
    ) %>%
    unnest(
      c(
        poisAIC,
        gammaAIC,
        poisPval,
        gammaPval
      )
    ) %>%

    #eval
    mutate(
      "pickAIC" =
        pmin(
          poisAIC,
          gammaAIC
        )
      ) %>%
    unnest(
      pickAIC
    ) %>%
    mutate(
      "pickPval" =
        if_else(
          pickAIC == poisAIC,
          poisPval,
          if_else(
            pickAIC == gammaAIC,
            gammaPval,
            9
          )
        ),
      "isSignif1" =
        if_else(
          pickPval <
            0.105,
          T, F
        )
      ) %>%

    return()
}


addStatGraph <- function(
  ...statEvalNonTbl,
  ....formula
) {

  ...statEvalNonTbl %>%

    mutate(
      graph =
        varData %>%
        modify(
          ~ .x %>%
            ggplot(
              aes(
                x = {
                  ....formula[[2]] %>%
                    eval()
                },
                y = {
                  ....formula[[3]] %>%
                    eval()
                }
              )
            ) +
            geom_quasirandom() +
            geom_smooth(
              method = "lm"
              ) +
            stat_poly_eq(
              formula = y ~ x,
              parse = T,
              aes(
                label = stat(
                  ..p.value.label..,
                  ..adj.rr.label..
                  )
                )
              ) +
            labs(
              y = deparse(
                ....formula[[2]]
              ),
              x = deparse(
                ....formula[[3]]
              ),
              title = deparse(
                variable
              )
            )
        )
    ) %>%

    return()
}


addStatGraph2 <- function(
  ...statEvalNonTbl2,
  ....formula
) {

  ...statEvalNonTbl2 %>%

    mutate(
      graph2 =
        varData %>%
        modify(
          ~ .x %>%
            ggplot(
              aes(
                x = {
                  ....formula[[2]] %>%
                    eval()
                },
                y = {
                  ....formula[[3]] %>%
                    eval()
                }
              )
            ) +
            geom_quasirandom() +
            geom_smooth(
              method = "lm"
            ) +
            stat_poly_eq(
              formula = y ~ poly(
                x,
                2
              ),
              parse = T,
              aes(
                label = stat(
                  ..p.value.label..,
                  ..adj.rr.label..
                )
              )
            ) +
            labs(
              y = deparse(
                ....formula[[2]]
              ),
              x = deparse(
                ....formula[[3]]
              ),
              #fixtitle
              title = deparse(
                curve
              )
            )
        )
    ) %>%

    return()
}

