library(here) #fromprojdir
library(vroom) #loadastbl

dat <- vroom(here("data/raw/osa.csv"))


#updatetaxa2017
library(BIOMASS)

taxasp <- correctTaxo(genus = dat$gen,
                      species = dat$sp, #notneeded
                      useCache = T)

taxa <- getTaxonomy(dat$gen)  #useful
dat$fam <- taxa$family
getWD <- getWoodDensity(genus = dat$gen,
                        species = dat$sp,
                        family = dat$fam,
                        stand = dat$plot)
dat$spg <- getWD$meanWD
dat <- subset(dat, dist != "NA")#&gen!="-")#&sp!="sp.")
dat$kg17 <- computeAGB(D = dat$dap, WD = dat$spg,
                       H = dat$h) * 1000


#chave2014

attach(dat)
dat$kg14 <- 0.0673 * (spg * dap^2 * h)^0.976

#updatefams--old
for (name in dat$fam) {
  #if (name == "Bombacaceae" |
  #name == "Sterculiaceae") {
  #name <- "Malvaceae"
  #}
  #if (name == "Cecropiaceae") {
  #name <- "Urticaceae"
  #}
}
detach(dat)

# both methods do seem equal
