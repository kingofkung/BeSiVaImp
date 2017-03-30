## In this script, we'll take what we did with CART and BeSiVa for the Trump Question, and we'll make a series of plots/analyses for our Chapter 3 results

library(rockchalk)

mainLoc <- "~/Dropbox/Dissertation Stuff/DatOutPut/C3"
setwd(mainLoc)
ftDat <- read.csv("fttrump1to100missthresh of539.csv")

str(ftDat)

summarize(ftDat, alphaSort = F)$numerics

## Turn column into vector
colToTable <- function(x, dec = TRUE){
    ##
    ## browser()
    myIVs <- paste(as.character(x), collapse = ",")
    myIVs <- gsub(",+", " ", myIVs)
    myIVs <- unlist(strsplit(myIVs, " "))
    sort(table(myIVs[!myIVs %in% ""]), decreasing = dec)
}

## Take the table, and repeat each name according to its number in the table
repNames <- function(i, vb) rep(names(vb[i]), vb[i])

minSelected <- 5

## Convert column of values in df to table, and remove any variable
## appearing less than minSelected times.
trumpBesiva <- colToTable(ftDat$mrintForDf, dec = FALSE)
trumpBesiva <- trumpBesiva[trumpBesiva >= minSelected]
tbLvls <- names(trumpBesiva)
## GetTrumpBesiva Ready for ggplot2 by turning it into an appropriate data frame
trumpBesiva <- lapply(seq(trumpBesiva), repNames, vb = trumpBesiva)
trumpBesiva <- data.frame("Var" = unlist(trumpBesiva))
trumpBesiva$Var <- factor(trumpBesiva$Var, levels = tbLvls)


## and repeat for the Cart example
trumpCart <- colToTable(ftDat$rpartIntVars, dec = FALSE)
trumpCart <- trumpCart[trumpCart >= minSelected]
tcLvls <- names(trumpCart)
## Making trumpCart into a Data Frame
trumpCart <- lapply(seq(trumpCart), repNames, vb = trumpCart)
trumpCart <- data.frame("Var" = unlist(trumpCart))
trumpCart$Var <- factor(trumpCart$Var, levels = tcLvls)



library(ggplot2)
VarPlotFunc <- function(dat, title){
    ggplot(data = dat) +
        geom_bar(aes(x = Var), fill = "darkgreen") +
            theme_classic(10) +
                theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", face = "bold"),
                      panel.grid.major.x = element_line(color = "gray")) +
                          xlab("Variable Names") +
                              ylab("Count") +
                                  ggtitle(title) +
                                  coord_flip()
}

outLoc <- paste0(mainLoc, "/TrumpFigs/")

dev.new()
pdf(file = paste0(outLoc,"BeSiVaTrumpVarsSelected", minSelected, "Times.pdf"))
VarPlotFunc(trumpBesiva, paste("Variables Selected by BeSiVa For Predicting Trump Support\n", minSelected, "or More Times"))
graphics.off()

dev.new()
pdf(file = paste0(outLoc,"CARTTrumpVarsSelected", minSelected, "Times.pdf"))
VarPlotFunc(trumpCart, paste("Variables Selected by CART For Predicting Trump Support\n", minSelected, "or More Times"))
graphics.off()

dev.new()
pdf(file = paste0(outLoc,"TestPClPsTrump.pdf"))
plot(density(ftDat$rpartPclps * 100), lty = 2, ylim = c(0, .09),
     main = "A Comparison of Trump Support PClPs on the Test set",
     xlab = "Percent Closely Predicted")
lines(density(ftDat$mrpclps * 100))
legend("topright", c("BeSiVa", "CART"), lty = c(1, 2))
graphics.off()

dev.new()
pdf(file = paste0(outLoc,"ValPClPsTrump.pdf"))
plot(density(ftDat$trueHldPrdsRP * 100), lty = 2, ylim = c(0, .11),
     main = "A Comparison of Trump Support PClPs on the Validation set",
     xlab = "Percent Closely Predicted")
lines(density(ftDat$trueHldPrds * 100))
legend("topright", c("BeSiVa", "CART"), lty = c(1, 2))
graphics.off()

dev.new()
pdf(file = paste0(outLoc,"EmpiricalPClPsTrump.pdf"))
plot(density(ftDat$empValPclp * 100),
     main = "PClPs from the Empirical Test of Trump Support",
     xlab = "Percent Closely Predicted")
graphics.off()
