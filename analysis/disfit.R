library(poweRlaw)


disfit <- function(RealFreq) {  #freqvector
  #Clauset2009,Gillespie2015

  Pl <- displ$new(round(RealFreq))
  Pl$setPars(estimate_pars(Pl))
  PlVar <- var(bootstrap(Pl)$bootstraps$pars) #variance
  #plot(Pl)

  PlP <- bootstrap_p(Pl)$p  #>0.1passes

  Exp <- disexp$new(round(RealFreq))
  Exp$setPars(estimate_pars(Exp))
  ExpVar <- var(bootstrap(Exp)$bootstraps$pars)

  Pois <- dispois$new(round(RealFreq))
  Pois$setPars(estimate_pars(Pois))
  PoisVar <- var(bootstrap(Pois)$bootstraps$pars)

  Lognorm <- dislnorm$new(round(RealFreq))
  Lognorm$setPars(estimate_pars(Lognorm))
  LognormVar <- bootstrap(Lognorm)$bootstraps
  LognormVar1 <- var(LognormVar$pars1)
  LognormVar2 <- var(LognormVar$pars2, na.rm = T)


  #null=bothOK #1sided=arg1==arg2
  PlExpP <- compare_distributions(Pl, Exp)$p_one_sided  #<0.05=arg1better
  PlLognormP <- compare_distributions(Pl, Lognorm)$p_one_sided
  PlPoisP <- compare_distributions(Pl, Pois)$p_one_sided


  results <- data.frame("PlP" = PlP,
                        "PlExpP" = PlExpP, #pvals
                        "PlLognormP" = PlLognormP,
                        "PlPoisP" = PlPoisP,

                        #slopes/pars
                        "PlPar" = Pl$pars,
                        "ExpPar" = Exp$pars,
                        "PoisPar" = Pois$pars,
                        "LognormPar1" = Lognorm$pars[1],
                        "LognormPar2" = Lognorm$pars[2],

                        #variances
                        "PlVar" = PlVar,
                        "ExpVar" = ExpVar,
                        "PoisVar" = PoisVar,
                        "LognormVar1" = LognormVar1,
                        "LognormVar2" = LognormVar2)


  return(results)
}
