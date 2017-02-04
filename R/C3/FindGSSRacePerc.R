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

dv <- "ushisp"

gssdv <- gss[complete.cases(gss[, dv]), ]
nrow(gssdv)



#get rid of values that are all NAs
gssdv <- gssdv[, sapply(gssdv, function(x) !all(is.na(x)))]

## and mostly NA's
sum(is.na(gssdv[,dv]))
gssdv <- gssdv[, sapply(gssdv, function(x) sum(is.na(x))) < nrow(gssdv) * .85]


## Get rid of values with near Zero Variance
gssdv <- gssdv[, -nearZeroVar(gssdv)]

## find variables with excess levels
CatBin <- lapply(gssdv, function(x) {
    if(is.factor(x)) length(levels(x)) > 50
})

## get their names
tooManyCats <- names(which(unlist(CatBin)))


## Remove DV, and variables used to create it
varsToUse <- colnames(gssdv)[!colnames(gssdv) %in% c("armdv1", "uswht", "usblk", "ushisp", "usasn", "usamind", "usjews", "respnum", "ballot", "issp", "version", tooManyCats)]
varsToUse <- sort(varsToUse)

source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctionslm.R")

## Rprof()
multirnd <- lapply(1:100, function(i){
    rnd1 <- besivalm(dv, varsToUse, gssdv, sampseed = i, hc = 10, showforms = F)
})

## Grab pclps, intvars
mrpclps <- unlist(lapply(multirnd, function(x) max(x$pclps)))
mrintvars <- lapply(multirnd, function(x) x$intvars)

## prep intvars for inclusion in dataframe
mrintForDf <- unlist(lapply(mrintvars, paste, collapse = ", "))

## create dataframe, and write to a file
infoDF <- data.frame(mrpclps, mrintForDf)
writeloc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/C3/"
write.csv(infoDF, paste0(writeloc, dv, ".csv"),  row.names = F)


## Rprof(NULL)
## summaryRprof()


## lm(ushisp ~ where4 + mobile16 + teens + wrkstat, gssdv)

sort(rnd1$pclps)

gssdv$found
lm(armdv1 ~ prestg10, gssdv)

