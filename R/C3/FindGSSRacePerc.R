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
## Fun fact: This gives you the difference of the range divided by 10!
armdv1Hc <- range(gss$armdv1, na.rm = TRUE)/10
armdv1Hc <- diff(armdv1Hc)

library(caret)
library(rpart)

dv <- "armdv1"

gssdv <- gss[complete.cases(gss[, dv]), ]
nrow(gssdv)

set.seed(27)
trueHoldRows <- sample(1:nrow(gssdv), round(nrow(gssdv)* .1))
trueholdout <-  gssdv[trueHoldRows, ]
gssdv <- gssdv[-trueHoldRows, ]

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

cor(gss[, c("uswht", "usblk", "ushisp", "usasn", "usamind", "usjews", "usmixed", "armdv1")], use = "pairwise")

## Remove DV, and variables used to create it
varsToUse <- colnames(gssdv)[!colnames(gssdv) %in% c("armdv1", "uswht", "usblk", "ushisp", "usasn", "usamind", "usjews", "respnum", "ballot", "issp", "version", "sampcode", "id", "usmixed", "vstrat" , tooManyCats)]
varsToUse <- sort(varsToUse)

source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctionslm.R")

makeForm <- as.formula(paste(dv, "~", paste(varsToUse, collapse = " + ")))

## Rprof()
myThresh <- 10
if(dv == "armdv1") myThresh <- armdv1Hc
seeds <- 1:100
multirnd <- lapply(seeds, function(i){
    print(paste("iteration", i))
    rnd1 <- besivalm(dv, varsToUse, gssdv, sampseed = i, hc = myThresh, showforms = F, showoutput = F)
})

## Revised getBestCP so it wouldn't print no matter what
getBestCP <- function(myCart, printTheCP = FALSE){
    cpLs <- myCart$cptable
    ## plotcp(myCart)
    ## if(printTheCP) print(cpLs[order(cpLs[,"xerror"]),])
    bestCP <- cpLs[order(cpLs[,"xerror"])[1],"CP"]
}

multiRpart <- lapply(seeds, function(i){
        print(paste("rpart iteration", i))
        set.seed(i)
        tst <- sample(1:nrow(gssdv), size = round(nrow(gssdv) * .2))
        myRpart <- rpart(makeForm, data = gssdv[-tst, ])
        ## Must prune the tree
        myRpart <- prune(myRpart, cp = getBestCP(myRpart))
        ##
        ##
        myPreds <- predict(myRpart, newdata = gssdv[tst, ])
        ##
        ##
        myPclp <- makepclp(NULL, gssdv[tst, dv], myPreds, myThresh)
        ##
        list("pclp" = myPclp, "varImp" = myRpart$variable.importance, "myRpart" = myRpart)
})

summary(multiRpart[[1]]$myRpart)




## Grab pclps, intvars
mrpclps <- unlist(lapply(multirnd, function(x) max(x$pclps, na.rm = TRUE)))
rpartPclps <- unlist(lapply(multiRpart, function(x) x$pclp))


## prep intvars for inclusion in dataframe
mrintvars <- lapply(multirnd, function(x) x$intvars)
mrintForDf <- unlist(lapply(mrintvars, paste, collapse = ", "))
rpartIntVars <- unlist(lapply(multiRpart, function(x) paste(names(x$varImp), collapse = ", ")))

##
trueHldPrds <- sapply(mrintvars, function(x, devee = dv, tho = trueholdout, dat = gssdv, thresh = myThresh){
    form <- paste(devee, "~", paste(x, collapse = " + ") )
    if(length(x) == 0) form <- paste0(form, "1")
    form
    ##
    myMod <- lm(form, dat)
    myPreds <- predict(myMod, newdata = tho)
    makepclp(myMod, tho[,devee], myPreds, thresh)
})

trueHldPrdsRP <- sapply(multiRpart, function(x, tho = trueholdout, devee = dv, thresh = myThresh){
    myPredsRP <- predict(x$myRpart, newdata = trueholdout)
    makepclp(NULL, tho[, dv], myPredsRP, thresh)
##
})

plot(density(trueHldPrdsRP), col = "red", xlim = c(0.35,0.65))
lines(density(trueHldPrds))

## create dataframe, and write to a file
infoDF <- data.frame(seeds, mrpclps, mrintForDf, rpartPclps, rpartIntVars, trueHldPrds, trueHldPrdsRP)
write.csv(infoDF, paste0(writeloc, note, dv, min(seeds),"to", max(seeds),"missthresh of", missthresh, ".csv"),  row.names = F)

myMessage <- "Ding! Simulation is done!"
number <- readLines("~/Dropbox/myno.txt")
command <- paste0("osascript -e 'tell application \"Messages\" to send \"", myMessage, "\" to buddy \"+",number,"\" of service \"SMS\"'")
system(command)




