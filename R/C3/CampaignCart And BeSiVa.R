rm(list = ls())

library(rpart)
source("~/Github/BeSiVaImp/R/BeSiVaFunctionslm.R")
anes <- read.csv("~/Dropbox/Lab POLS 306_Fall 2016/Lab Materials/DataHuntFindings/anes_pilot_2016recoded.csv")


## Assemble the formula
colnames(anes)
## Write the dependent variable
devee <- "fttrump"
!is.null(anes[,devee])

if(is.numeric(anes[, devee])) myThresh <- diff(range(anes[, devee], na.rm = TRUE))/10

anesSub <- anes[complete.cases(anes[,devee]),]
anesSub[, devee]

set.seed(1)
valRows <- sample(1:nrow(anesSub), nrow(anesSub) * .1)
valSet <- anesSub[valRows,]
anesSub <- anesSub[-valRows,]


## Determine the means CART will use to predict the devee
if(is.numeric(anesSub[, devee])){
    bestMethod <- "anova"
} else
{
    bestMethod <- "class"
}


catAnes <- anesSub[, sapply(anesSub, is.factor)]
numAnes <- names(anesSub)[sapply(anesSub, is.numeric)]
nCats <- sapply(catAnes, function(x) length(unique(x)))


varsToCheck <- unique(c("rr1", "rr2", "rr3", "rr4", "race", "gender", "birthyr", "educ", "marstat", "faminc", grep("ft", colnames(anesSub), value = TRUE),
                 grep("lazy", colnames(anesSub), value = TRUE),
                 grep("violent", colnames(anesSub), value = TRUE),
                        "ladder", "getahead", "finwell",
                        names(nCats)[nCats < 20],
                        numAnes
                        ))

mostlyMissing <- sapply(varsToCheck, function(i, dataset = anesSub) {
    sum(is.na(dataset[,i]))/nrow(dataset) > .5
})
mostlyMissing <- names(which(mostlyMissing))

varsToCheck <- varsToCheck[!varsToCheck %in% c(devee, mostlyMissing)]

form <- paste(devee, "~", paste(varsToCheck, collapse = " + "))

## pull out the


seeds <- 1:10
myCarts <- lapply(seeds, function(i, theform = form, dat = anesSub, myMethod = bestMethod, prop = .2){
    set.seed(i)
    print(paste0("rpart iteration = ", i))
    tr <- sample(1:nrow(dat), round(nrow(dat)*prop))
    myCart <- rpart(theform, data = dat[-tr,], method = myMethod)
    return(myCart)
})

besivas <- lapply(seeds, function(i, deev = devee, ivees = varsToCheck, data = anesSub, prop = .2, mt = myThresh){
    print(paste0("besiva iteration = ", i))
    besivalm(devee = deev, ivs = ivees, data, perc = prop, sampseed = i, showoutput = FALSE, showforms = FALSE, hc = mt)
})



getBestCP <- function(myCart){
    cpLs <- myCart$cptable
    bestCP <- cpLs[order(cpLs[,"xerror"])[1],"CP"]
}

bestCPs <- lapply(myCarts, getBestCP)


prunedCarts <- lapply(seq_along(bestCPs), function(x, mCs = myCarts, bCPs = bestCPs){
    prune(mCs[[x]], bCPs[[x]])
})


getImpVars <- function(myCart){
    tvI <- caret::varImp(myCart)
    tvI <- tvI[order(tvI$Overall, decreasing = TRUE), , drop = FALSE]
    tvI <- tvI[tvI$Overall>0, , drop = FALSE]
    tvI
}

impVarList <- lapply(myCarts, getImpVars)
impVars <- table(unlist(lapply(impVarList, rownames)))
sort(impVars)


trs <- lapply(seeds, function(x, dat = anes, prop = .2) tr <- sample(1:nrow(dat), round(nrow(dat)*prop)))

getErrorMetrics <- function(i, pCs = prunedCarts, mCs = myCarts, testRows = valRows, rdig = 2, dv = devee, dat = anes){
    prunedCart <- pCs[[i]]
    myCart <- mCs[[i]]
    theobs <- dat[testRows, dv]
    ##
    if(is.numeric(theobs)){
        thepreds <- predict(prunedCart, newdata = dat[testRows, ])
        ##
        prunePCLP <- makepclp(NULL, theobs, thepreds, howclose = 10)
        ##
        return(c("prunePCLP" = prunePCLP))
        ##
    }else if(is.factor(obs)){
        preds <- predict(prunedCart, newdata = dat[testRows, ], type = "class")
        ##
        prunePCP <- sum(preds == obs, na.rm = TRUE)/length(na.omit(obs))
        ##
        dvTab <- table(dat[,dv])
        modalCat <- prop.table(dvTab)[order(dvTab, decreasing = TRUE)[1]]
        names(modalCat) = NULL
        return(c("modalCat" = modalCat, "prunePCP" = prunePCP))
    }
}


lapply(seq_along(myCarts), getErrorMetrics)

