## Here's the script where we open the american national election
## survey data, and subset it to the year 2000.
rm(list = ls()[!ls() %in% "anes"])
loc <- "/Users/bjr/GitHub/BeSiVaImp/Data/"

library(foreign)
library(memisc)
library(plyr)

if(!exists("anes")){
    anes <- read.spss(paste0(loc,"anes_timseries_cdf.sav"))
    anes <- as.data.frame(anes)
    colnames(anes) <- tolower(colnames(anes))
}


## Work on dealing with missing data, according to the codebook

source("rmMissings.R")

str(anes)
data.frame(table(anes$vcf0004))

anes2000 <- anes[anes$vcf0004 == 2000, ]


notmissing2000 <- colSums(!is.na(anes2000)) > 0
anes2000 <- anes2000[, notmissing2000]
