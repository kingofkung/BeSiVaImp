## Here's the script where we open the american national election survey data
rm(list = ls()[!ls() %in% "anes"])
loc <- "/Users/bjr/GitHub/BeSiVaImp/Data/"

library(foreign)
## library(readstata13)
library(memisc)
library(plyr)

if(!exists("anes")){
    anes <- read.spss(paste0(loc,"anes_timseries_cdf.sav"))
    anes <- as.data.frame(anes)
    colnames(anes) <- tolower(colnames(anes))
}
length(anes[[1]])
lapply(anes, length)
## View(head(anes))


## if(!exists("anes")){
##     anes <- as.data.set(spss.system.file(paste0(loc,"anes_timseries_cdf.sav")))
##     ## anes <- as.data.frame(anes)
## }



## anes$vcf0120[!is.na(anes$vcf0120)]

table(as.vector(anes$vcf0202[!is.na(anes$vcf0202)]))
## anesdf <- as.data.frame(anes)
## table(as.vector(anesdf$vcf0202[!is.na(anesdf$vcf0202)]))







## anes2 <- read.dta13("/Users/bjr/GitHub/BeSiVaImp/Data/anes_timeseries_cdf_dta/anes_timeseries_cdf.dta")



## Work on dealing with missing data, according to the codebook

source("rmMissings.R")

str(anes)
data.frame(table(anes$vcf0004))

anes48 <- anes[ anes$vcf0004 == 1948,]
anes48$vcf0006a

anes52 <- anes[anes$vcf0004 == 1952,]

anes2000 <- anes[anes$vcf0004 == 2000, ]



dim(anes48)

## A better way of determining whether there's missing data
notmissing48 <- colSums(!is.na(anes48)) > 0


anes48 <- anes48[,notmissing48]
colnames(anes48)

notmissing52 <- colSums(!is.na(anes52)) > 0
anes52 <- anes52[,notmissing52]


notmissing2000 <- colSums(!is.na(anes2000)) > 0
anes2000 <- anes2000[, notmissing2000]
