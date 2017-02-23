## Make some plots from the data I got from CART/BeSiVa for Comparison Purposes
rm(list = ls())
myPath <- "~/Dropbox/Dissertation Stuff/DatOutPut/C3/"
csvOut <- read.csv(paste0(myPath, "armdv11to100missthresh of573.csv"))

head(csvOut)

## Turn column into vector
colToTable <- function(x, dec = TRUE){
    ##
    ## browser()
    myIVs <- paste(as.character(x), collapse = ",")
    myIVs <- gsub(",+", " ", myIVs)
    myIVs <- unlist(strsplit(myIVs, " "))
    sort(table(myIVs[!myIVs %in% ""]), decreasing = dec)
}

varBesiva <- colToTable(csvOut$mrintForDf, dec = FALSE)
varRpart <- colToTable(csvOut$rpartIntVars, dec = FALSE)

minSelected <- 10
varBesiva <- varBesiva[varBesiva >= minSelected]
vBesLvls <- names(varBesiva)

varRpart <- varRpart[varRpart >= minSelected]
vRpartLvls <- names(varRpart)


rockchalk::summarizeNumerics(csvOut[, -1], F)


repNames <- function(i, vb) rep(names(vb[i]), vb[i])

varBesiva <- lapply(seq(varBesiva), repNames, vb = varBesiva)
varBesiva <- data.frame("Var" = unlist(varBesiva))
varBesiva$Var <- factor(varBesiva$Var, levels = vBesLvls)

varRpart <- unlist(lapply(seq(varRpart), repNames, vb = varRpart))
varRpart <- data.frame("Var" = unlist(varRpart))
varRpart$Var <- factor(varRpart$Var, levels = vRpartLvls)

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

dev.new()
pdf(paste0(myPath, "BeSivaSelected", minSelected, "TimesC3.pdf"))
VarPlotFunc(varBesiva, paste("Variables Selected by BeSiVa", minSelected, "or More Times"))
graphics.off()

dev.new()
pdf(paste0(myPath, "CARTSelected", minSelected, "TimesC3.pdf"))
VarPlotFunc(varRpart,  paste("Variables Selected by CART", minSelected, "or More Times"))
graphics.off()

dev.new()
pdf(paste0(myPath, "TestPClPs.pdf"))
plot(density(csvOut$mrpclps * 100), main = "A comparison of PClPs on the Test Set", xlab = "Percent Closely Predicted")
lines(density(csvOut$rpartPclps * 100), lty = 2)
legend("topright", c("BeSiVa", "CART"), lty = c(1, 2))
graphics.off()

dev.new()
pdf(paste0(myPath, "ValidationPClPs.pdf"))
plot(density(csvOut$trueHldPrds * 100), main = "A comparison of PClPs on the Validation Set", xlab = "Percent Closely Predicted")
lines(density(csvOut$trueHldPrdsRP * 100), lty = 2)
legend("topright", c("BeSiVa", "CART"), lty = c(1, 2))
graphics.off()
