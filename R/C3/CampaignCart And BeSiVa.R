rm(list = ls()[!ls() %in% "anes"])

library(rpart)
source("~/Github/BeSiVaImp/R/BeSiVaFunctionslm.R")
anes <- read.csv("/Users/bjr/Dropbox/Lab POLS 306_Fall 2016/Lab Materials/DataHuntFindings/anes_pilot_2016recoded.csv")


## Assemble the formula
colnames(anes)
## Write the dependent variable
devee <- "fttrump"
!is.null(anes[,devee])

## Determine the means CART will use to predict the devee
if(is.numeric(anes[, devee])){
    bestMethod <- "anova"
} else
{
    bestMethod <- "class"
}


catAnes <- anes[, sapply(anes, is.factor)]
numAnes <- names(anes)[sapply(anes, is.numeric)]
nCats <- sapply(catAnes, function(x) length(unique(x)))

varsToCheck <- unique(c("rr1", "rr2", "rr3", "rr4", "race", "gender", "birthyr", "educ", "marstat", "faminc", grep("ft", colnames(anes), value = TRUE),
                 grep("lazy", colnames(anes), value = TRUE),
                 grep("violent", colnames(anes), value = TRUE),
                        "ladder", "getahead", "finwell",
                        names(nCats)[nCats < 40],
                        numAnes
                        ))
varsToCheck <- varsToCheck[!varsToCheck %in% devee]
form <- paste(devee, "~", paste(varsToCheck, collapse = " + "))

## pull out the

anes$repcand[anes$repcand %in% "None"] <- NA
anes$repcand <- factor(anes$repcand)

seeds <- 1:20
myCarts <- lapply(seeds, function(i, theform = form, dat = anes, myMethod = bestMethod, prop = .2){
    set.seed(i)
    tr <- sample(1:nrow(dat), round(nrow(dat)*prop))
    myCart <- rpart(theform, data = dat[-i,], method = myMethod)
    return(myCart)
})

besivas <- lapply(seeds, function(i, deev = devee, ivees = varsToCheck, data = anes, prop = .2){
    besivalm(devee = deev, ivs = ivees, data, perc = prop, sampseed = i, showoutput = F, showforms = F)
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



getErrorMetrics <- function(i, pCs = prunedCarts, mCs = myCarts, testRows = trs, rdig = 2, dv = devee, dat = anes){
    prunedCart <- pCs[[i]]
    myCart <- mCs[[i]]
    tr <- testRows[[i]]
    obs <- dat[tr, dv]
##
    if(is.numeric(obs)){
        preds <- predict(prunedCart, newdata = dat[tr, ])
        fullPreds <- predict(myCart, newdata = dat[tr,])
##
        pruneRMSE <- sqrt(mean((obs - preds)^2, na.rm = TRUE))
        fullRMSE <- sqrt(mean((obs - fullPreds)^2, na.rm = TRUE))
    ##
        ## datRange <- range(dat[,dv], na.rm = TRUE)
        return(c("pruneRMSE" = pruneRMSE, "fullRMSE" = fullRMSE))
##
    }else if(is.factor(obs)){
        preds <- predict(prunedCart, newdata = dat[tr, ], type = "class")
        fullPreds <- predict(myCart, newdata = dat[tr,], type = "class")
    ##
        prunePCP <- sum(preds == obs, na.rm = TRUE)/length(na.omit(obs))
        fullPCP <- sum(fullPreds == obs, na.rm = TRUE)/length(na.omit(obs))
    ##
        dvTab <- table(dat[,dv])
        modalCat <- prop.table(dvTab)[order(dvTab, decreasing = TRUE)[1]]
        names(modalCat) = NULL
        return(c("modalCat" = modalCat, "prunePCP" = prunePCP, "fullPCP" = fullPCP ))
    }
}


lapply(seq_along(myCarts), getErrorMetrics)

