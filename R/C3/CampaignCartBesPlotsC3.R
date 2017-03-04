## In this script, we'll take what we did with CART and BeSiVa for the Trump Question, and we'll make a series of plots/analyses for our Chapter 3 results

setwd("~/Dropbox/Dissertation Stuff/DatOutPut/C3")
ftDat <- read.csv("fttrump1to100missthresh of539.csv")

str(ftDat)

## Turn column into vector
colToTable <- function(x, dec = TRUE){
    ##
    ## browser()
    myIVs <- paste(as.character(x), collapse = ",")
    myIVs <- gsub(",+", " ", myIVs)
    myIVs <- unlist(strsplit(myIVs, " "))
    sort(table(myIVs[!myIVs %in% ""]), decreasing = dec)
}

colToTable(ftDat$mrintForDf)
colToTable(ftDat$rpartIntVars)
