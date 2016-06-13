## Here's the script where we open the american national election survey data

loc <- "/Users/bjr/Dropbox/R_Projects/GSSThing/anes_timeseries_cdf_sav/"

library(foreign)

anes <- read.spss(paste0(loc,"anes_timseries_cdf.sav"), to.data.frame = T)
dim(anes)

anes48 <-anes[ anes$VCF0004 == 1948,]

dim(anes48)

## A better way of determining whether there's missing data
notmissing <- colSums(!is.na(anes48)) > 0


anes48 <- anes48[,notmissing]
