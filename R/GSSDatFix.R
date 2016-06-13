## Open and clean all neccessities for using the GSS Data
rm(list = ls())

library(foreign)

dat <- read.dta("/Users/bjr/Dropbox/R_Projects/GSSThing/GSS2014merged_R3.DTA")
## dat <- read.dta("/Users/bjr/GitHub/BeSiVaImp/Data/GSS2006.dta")
varlabs <- attr(dat, "var.labels")
## ncol(dat)
## head(dat)

colnames(dat)[grep("Vote", colnames(dat), ignore.case = TRUE)]

unique(dat$vote12)
dat$vote12bin <- as.character(dat$vote12)
unique(dat$vote12bin)

dat$vote12bin[dat$vote12bin %in% "did not vote"] <- "0"
dat$vote12bin[dat$vote12bin %in% "voted"] <- "1"
dat$vote12bin[dat$vote12bin %in% "ineligible"] <- NA

dat$vote12bin <- as.numeric(dat$vote12bin)

table(dat$vote12, dat$vote12bin)


unique(dat$occ10)

## Recode trtcops
apply(dat, 2, function(x) grep("cop",x))
colnames(dat)[grep("cop",colnames(dat))]


## recode dat$vote08
dat$vote08

dat$vote08bin <- as.character(dat$vote08)
unique(as.character(dat$vote08))
dat$vote08bin[dat$vote08bin %in% "did not vote"] <- "0"
dat$vote08bin[dat$vote08bin %in% "voted"] <- "1"
dat$vote08bin[dat$vote08bin %in% "ineligible"] <- NA

dat$vote08bin <- as.numeric(dat$vote08bin)
## table(dat$vote08, dat$vote08bin, useNA= "ifany")
