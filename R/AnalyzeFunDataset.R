## So now we've made a fun dataset. Let's analyze it.

source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/MakeFunDataset.R")

colsofint <- colnames(daf)[!colnames(daf) %in% "yvar"]

## sample( nrow(daf), round(nrow(daf)/10) )

besiva('yvar', colsofint, daf)
