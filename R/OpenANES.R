## Here's the script where we open the american national election survey data
rm(list = ls()[!ls() %in% "anes"])
loc <- "/Users/bjr/Dropbox/R_Projects/GSSThing/anes_timeseries_cdf_sav/"

## library(foreign)
library(memisc)

## anes <- read.spss(paste0(loc,"anes_timseries_cdf.sav"), to.data.frame = T)
if(!exists("anes")){
    anes <- as.data.set(spss.system.file(paste0(loc,"anes_timseries_cdf.sav")))
    anes <- as.data.frame(anes)
}

str(anes)


anes48 <- anes[ anes$vcf0004 == 1948,]
anes48$vcf0006a

anes52 <- anes[anes$vcf0004 == 1952,]

dim(anes48)

## A better way of determining whether there's missing data
notmissing48 <- colSums(!is.na(anes48)) > 0


anes48 <- anes48[,notmissing48]
colnames(anes48)

notmissing52 <- colSums(!is.na(anes52)) > 0
anes52 <- anes52[,notmissing52]
