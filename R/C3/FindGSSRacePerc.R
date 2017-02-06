## So the plan here is to
## A. Try and get the data that Alba, Rumbaut,
## and Marotz (2005) use to predict racial attitudes in the 2000 GSS MAUS Module
## B. Replicate some of their analyses.
## C. Try BeSiVa lm on the analyses

rm(list = ls())
datloc <- "/Users/bjr/GitHub/BeSiVaImp/Data/GSS_stata/"
writeloc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/C3/"
note <- ""

library(foreign)
gss <- read.spss(paste0(datloc, "GSS2000.sav"), to.data.frame = T)

head(gss)
## gss$uswht
## gss$usblk
## gss$ushisp

gss$armdv1 <- log((gss$usblk + gss$ushisp)/gss$uswht)
gss$armdv1[which(gss$armdv == Inf)] <- NA


library(caret)

dv <- "usblk"

gssdv <- gss[complete.cases(gss[, dv]), ]
nrow(gssdv)



#get rid of values that are all NAs
gssdv <- gssdv[, sapply(gssdv, function(x) !all(is.na(x)))]

## and mostly NA's
sum(is.na(gssdv[,dv]))
missthresh <- round(nrow(gssdv) * .5)
## Only allow columns whose proportion of NAs is less than missthresh
gssdv <- gssdv[, sapply(gssdv, function(x) sum(is.na(x))) < missthresh]


## Get rid of values with near Zero Variance
gssdv <- gssdv[, -nearZeroVar(gssdv)]

## find variables with excess levels
CatBin <- lapply(gssdv, function(x) {
    if(is.factor(x)) length(levels(x)) > 20
})

## get their names
tooManyCats <- names(which(unlist(CatBin)))


## Remove DV, and variables used to create it
varsToUse <- colnames(gssdv)[!colnames(gssdv) %in% c("armdv1", "uswht", "usblk", "ushisp", "usasn", "usamind", "usjews", "respnum", "ballot", "issp", "version", tooManyCats)]
varsToUse <- sort(varsToUse)

source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctionslm.R")

## Rprof()
seeds <- 1:100
multirnd <- lapply(seeds, function(i){
    print(paste("iteration", i))
    rnd1 <- besivalm(dv, varsToUse, gssdv, sampseed = i, hc = 10, showforms = F, showoutput = F)
})


## Grab pclps, intvars
mrpclps <- unlist(lapply(multirnd, function(x) max(x$pclps, na.rm = TRUE)))
mrintvars <- lapply(multirnd, function(x) x$intvars)


## prep intvars for inclusion in dataframe
mrintForDf <- unlist(lapply(mrintvars, paste, collapse = ", "))
## create dataframe, and write to a file
infoDF <- data.frame(seeds, mrpclps, mrintForDf)
write.csv(infoDF, paste0(writeloc, note, dv, min(seeds),"to", max(seeds),"missthresh of", missthresh, ".csv"),  row.names = F)

myMessage <- "Ding! Simulation is done!"
number <- readLines("~/Dropbox/myno.txt")
command <- paste0("osascript -e 'tell application \"Messages\" to send \"", myMessage, "\" to buddy \"+",number,"\" of service \"SMS\"'")
system(command)




