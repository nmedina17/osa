library(here);
source(
  here("analysis/stats.R")
) #...$varData
library(tidymodels) #glance()
library(ggbeeswarm) #quasirandom()
library(ggpmisc) #stat_poly_eq()
library(ggpubr) #ggarrange()
# source(
#   here("analysis/disfit.R")
# ) #disfit()


#main

getStatsTbl <- function(
  #has"varData"col
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

    addGraph(
      ...formula
    )
}



getStatsTbl2 <- function(
  ...nestedVarDataTbl,
  #user's
  ...formula2
) {

  ...nestedVarDataTbl %>%
    statFitTbl(
      ...formula2
    ) %>%
    addStatEval2(
      ...formula2
    )
}



getStatsTbl1 <- function(
  #has"varData"col
  ...nestedVarDataTbl,
  #user's
  ...formula
) {

  ...nestedVarDataTbl %>%

    statFitTbl1(
      ...formula
    ) %>%
    addStatEval(
      ...formula
    ) %>%

    addGraph1(
      ...formula
    )
}



getStatsTbl12 <- function(
  #has"varData"col
  ...nestedVarDataTbl,
  #user's
  ...formula
) {

  ...nestedVarDataTbl %>%

    statFitTbl1(
      ...formula
    ) %>%
    addStatEval2(
      ...formula
    )
}



#sub-funcs

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
              formula =
                ....formula
            )
        ),
      "statPrint" =
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
    )
}



statFitTbl1 <- function(
  ...nestedVarDataTbl,
  ....formula
) {

  ...nestedVarDataTbl %>%

    mutate(
      "statTest" =
        varData1 %>%  #diffhere
        modify(
          ~ .x %>%
            lm(
              formula =
                ....formula
            )
        ),
      "statPrint" =
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
    )
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
      varyTest =
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
        varyTest %>%
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

    mutate(
      "R2adj" =
        statPrint %>%
        modify(
          ~ .x %>%
            filter(
              term !=
                "(Intercept)"
            ) %>%
            pull(
              adj.r.squared
            )
        )
    ) %>%
    unnest(
      R2adj
    )
}



addStatEval2 <- function(
  ...statFitTbl2,
  ....formula2
) {

  ...statFitTbl2 %>%

    mutate(
      #diffhere
      "curveName" =
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

    mutate(
      "R2adj" =
        statPrint %>%
        modify(
          ~ .x %>%
            filter(
              term !=
                "(Intercept)"
            ) %>%
            pull(
              adj.r.squared
            )
        )
    ) %>%
    unnest(
      R2adj
    )
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
    )
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
      "isSignif9" =
        if_else(
          pickPval <
            0.105,
          T, F
        )
      )
}



addGraph <- function(
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
                  ....formula[[3]] %>%
                    eval()
                },
                y = {
                  ....formula[[2]] %>%
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
              parse = F,
              aes(
                label = paste(
                  after_stat(
                    p.value.label
                    ),
                  after_stat(
                    adj.rr.label
                    )
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
    )
}


addGraph2 <- function(
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
                  ....formula[[3]] %>%
                    eval()
                },
                y = {
                  ....formula[[2]] %>%
                    eval()
                }
              )
            ) +
            geom_quasirandom() +
            geom_smooth(
              method = "lm",
              formula = y ~ poly(
                x,
                2
              )
            ) +
            stat_poly_eq(
              formula = y ~ poly(
                x,
                2
              ),
              parse = F,
              aes(
                label = paste(
                  after_stat(
                    p.value.label
                    ),
                  after_stat(
                    adj.rr.label
                  )
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
                variable
              )
            )
        )
    )
}



addGraph1 <- function(
  ...statEvalNonTbl,
  ....formula
) {

  ...statEvalNonTbl %>%

    mutate(
      graph =
        varData1 %>%  #diffhere
        modify(
          ~ .x %>%
            ggplot(
              aes(
                x = {
                  ....formula[[3]] %>%
                    eval()
                },
                y = {
                  ....formula[[2]] %>%
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
              parse = F,
              aes(
                label = paste(
                  after_stat(
                    p.value.label
                  ),
                  after_stat(
                    adj.rr.label
                  )
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
    )
}



addGraph12 <- function(
  ...statEvalNonTbl2,
  ....formula
) {

  ...statEvalNonTbl2 %>%

    mutate(
      graph2 =
        varData1 %>%  #diffhere
        modify(
          ~ .x %>%
            ggplot(
              aes(
                x = {
                  ....formula[[3]] %>%
                    eval()
                },
                y = {
                  ....formula[[2]] %>%
                    eval()
                }
              )
            ) +
            geom_quasirandom() +
            geom_smooth(
              method = "lm",
              formula = y ~ poly(
                x,
                2
              )
            ) +
            stat_poly_eq(
              formula = y ~ poly(
                x,
                2
              ),
              parse = F,
              aes(
                label = paste(
                  after_stat(
                    p.value.label
                  ),
                  after_stat(
                    adj.rr.label
                  )
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
                variable
              )
            )
        )
    )
}



#ordinate

getOrdVarTbl <- function(
  ...commTbl,
  ...metaTbl
) {


  ord = rda(
    ...commTbl
  )

  ordTbl = scores(
    ord
  )$sites %>%
    as_tibble()


  cbind(
    ...metaTbl,
    ordTbl
  ) %>%
    as_tibble()
}
