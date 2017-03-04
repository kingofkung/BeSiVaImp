rm(list = ls())

library(rpart)
writeloc <- "~/Dropbox/Dissertation Stuff/DatOutPut/C3/"
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

similarToDV <- c("repcand")

varsToCheck <- varsToCheck[!varsToCheck %in% c(devee, mostlyMissing, similarToDV)]

form <- paste(devee, "~", paste(varsToCheck, collapse = " + "))


seeds <- 1:100
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

impVarList <- lapply(prunedCarts, getImpVars)
impVars <- table(unlist(lapply(impVarList, rownames)))
sort(impVars)



getErrorMetrics <- function(i, pCs = prunedCarts, mCs = myCarts, testRows = valRows, rdig = 2, dv = devee, dat = anes){
    prunedCart <- pCs[[i]]
    theobs <- dat[testRows, dv]
    ##
    if(is.numeric(theobs)){
        thepreds <- predict(prunedCart, newdata = dat[testRows, ])
        ##
        prunePCLP <- makepclp(NULL, theobs, thepreds, howclose = 10)
        ##
        return(prunePCLP)
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

## When you get the data, you need the test rows' pclps from CART, to compare to BeSiVa
trs <- lapply(seeds, function(x, dat = anes, prop = .2) tr <- sample(1:nrow(dat), round(nrow(dat)*prop)))
myCartPClPs <- sapply(seq_along(myCarts), function(x) getErrorMetrics(x, testRows = trs[[x]]))
##
besivamrPClPs <- sapply(besivas, function(x) max(x$pclps))

## Get vars, and
besivaVars <- lapply(besivas, function(x) x$intvars)
## Get them ready for output
mrintForDf <- sapply(besivaVars, function(x) paste(x, collapse = ", "))
rpartIntVars <- sapply(impVarList, function(x) paste(rownames(x), collapse = ", "))

besivaforms <- lapply(besivaVars, function(x, dv = devee) paste(dv, "~", paste(x, collapse = " + ")))
besivaforms[besivaforms %in% "fttrump ~ "] <- paste0(devee, " ~ 1")

besivaMods <- lapply(besivaforms, function(x, dat = anesSub) lm(x, data = dat))
besivaPreds <- lapply(besivaMods, predict, newdata = anes[valRows,])


besivaValPClP <- sapply(besivaPreds, function(x) makepclp(NULL, anes[valRows, devee], x, 10))
cartValPClP <- sapply(seq_along(myCarts), getErrorMetrics)

plot(density(besivaValPClP))
lines(density(cartValPClP), lty = 2)



oneModVal <- lapply(trs, function(x, dat = anesSub, deev = devee, vs = valSet){
    mod <- lm(fttrump ~ rr1 + gender + race + pid7 + ideo5 + birthyr, data = dat[-x, ])
    ## Get validation PClPs
    modPredsVal <- predict.lm(mod, newdata = fixbadlevels(vs, mod, dat[-x,]))
    valPclp <- makepclp(NULL, vs[, deev], modPredsVal, 10)
    ## Get test PClPs
    modPredsTest <- predict.lm(mod, newdata = fixbadlevels(dat[x,], mod, dat[-x,]))
    testPclp <- makepclp(NULL, dat[x, deev], modPredsTest, 10)
    return(c(testPclp, valPclp))
})
oneModVal <- do.call(rbind, oneModVal)
oneModVal <- as.data.frame(oneModVal)
colnames(oneModVal) <- c("testPclp", "valPclp")
rockchalk::summarize(cbind(outDF[, -1]), alphaSort = F)$numerics

dfName <- paste0(devee, min(seeds),"to", max(seeds), "missthresh of", round(nrow(anesSub) * .5), ".csv")
outDF <- data.frame("seeds" = seeds, "mrpclps" = besivamrPClPs, "mrintForDf" = mrintForDf,
                    "rpartPclps" = myCartPClPs, "rpartIntVars" = rpartIntVars,
                    "trueHldPrds" = besivaValPClP, "trueHldPrdsRP" = cartValPClP,
                    "empTestPclp" = oneModVal$testPclp, "empValPclp" = oneModVal$valPclp)
write.csv(outDF, paste0(writeloc, dfName), row.names = FALSE)
