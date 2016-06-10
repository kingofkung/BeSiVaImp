## So now we've made a fun dataset. Let's analyze it.
rm(list = ls())
## sampsize <- 1500
source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/MakeFunDataset.R")

print(sampsize)

colsofint <- colnames(daf)[!colnames(daf) %in% "yvar"]

## sample( nrow(daf), round(nrow(daf)/10) )


for(i in 1:10) print(besiva('yvar', colsofint, daf, iters = 5, perc = .25, sampseed = i, thresh = 0.001 ))
