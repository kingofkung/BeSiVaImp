## So the plan here is to
## A. Try and get the data that Alba, Rumbaut,
## and Marotz (2005) use to predict racial attitudes in the 2000 GSS MAUS Module
## B. Replicate some of their analyses.
## C. Try BeSiVa lm on the analyses

rm(list = ls())
datloc <- "/Users/bjr/GitHub/BeSiVaImp/Data/GSS_stata/"

library(foreign)
gss <- read.spss(paste0(datloc, "GSS2000.sav"), to.data.frame = T)

head(gss)
## gss$uswht
## gss$usblk
## gss$ushisp

gss$armdv1 <- log((gss$usblk + gss$ushisp)/gss$uswht)
gss$armdv1[which(gss$armdv == Inf)] <- NA


library(caret)

gssdv <- gss[complete.cases(gss$armdv1), ]
nrow(gssdv)



#get rid of values that are all NAs
gssdv <- gssdv[, sapply(gssdv, function(x) !all(is.na(x)))]


## Get rid of values with near Zero Variance
gssdv <- gssdv[, -nearZeroVar(gssdv)]

## find variables with excess levels
CatBin <- lapply(gssdv, function(x) {
    if(is.factor(x)) length(levels(x)) > 50
})

## get their names
tooManyCats <- names(which(unlist(CatBin)))


## Remove DV, and variables used to create it
varsToUse <- colnames(gssdv)[!colnames(gssdv) %in% c("armdv1", "uswht", "usblk", "ushisp", "usasn", tooManyCats)]


source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctionslm.R")

## Rprof()
rnd1 <- besivalm("armdv1", varsToUse, gssdv, sampseed = 12345, hc = .5)
## Rprof(NULL)
## summaryRprof()

sort(rnd1$pclps)

gssdv$found
lm(armdv1 ~ prestg10, gssdv)

