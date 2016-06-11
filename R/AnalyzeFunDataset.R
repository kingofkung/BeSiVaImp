## So now we've made a fun dataset. Let's analyze it.
rm(list = ls())
## sampsize <- 1500
source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/MakeFunDataset.R")

print(sampsize)

colsofint <- colnames(daf)[!colnames(daf) %in% "yvar"]

## sample( nrow(daf), round(nrow(daf)/10) )


## for(i in 1:10)

besiva('yvar', colsofint, daf, iters = 5, perc = .25, thresh = 0.0, sampseed = 54321)

## corrmod <- glm(yvar ~ X6.1 + cont1, data = daf, family = "binomial")
## summary(corrmod)


## look at any dataset
## get a dataset and impute
