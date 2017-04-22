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


renamer <- c('size' = "Population",
             'comhisp' = "Estimated Percent of Hispanics In Community",
             'degree' = "Last Degree Attained",
             'finrela' = "Relative Income",
             'educ' = "Education in Years",
             'relig16' = "Religion at Age 16",
             'sex' = "Sex",
             'comblk' = "Estimated Percent of Blacks In Community",
             'dateintv' = "Date Interviewed",
             'family16' = "Family at 16",
             'relig' = "Religion",
             'sexsex' = 'Number of Sex Partners',
             'comamind'= "Estimated Percent of Indians In Community",
             'sibs' = "Number of Siblings",
             'hhtype1' = 'Condensed Household Type',
             'region' = "Region",
             'gender1' = "Gender of First Family Member",
             'sexsex5' = "Genders of Sex Partners",
             'nummen' = "Number of Male Partners",
             'numwomen' = "Number of Female Partners",
             'isco681' = "Occupation",
             'prestg105plus' = "Occupational Prestige, new measure",
             'comasn' = "Estimated Percent of Asians In Community",
             'prestg10' = "Occupational Prestige in 2010",
             'prestg80'= "Occupational Prestige in 1980",
             'sei' = "Socioeconomic Index",
             'hhrace' = "Race of Household",
             'srcbelt' = "Population Density SRC Code",
             'comwht' = "Estimated Percent of Whites In Community",
             'race' = "Race 3 Categories",
             'racecen1' = "Race Census Categories")




varBesiva <- colToTable(csvOut$mrintForDf, dec = FALSE)
varRpart <- colToTable(csvOut$rpartIntVars, dec = FALSE)

minSelected <- 5
varBesiva <- varBesiva[varBesiva >= minSelected]

for(i in seq_along(renamer)){
    names(varBesiva)[names(varBesiva) %in% names(renamer)[i]] <- renamer[i]
}


vBesLvls <- names(varBesiva)

minCartSelected <- minSelected + 20
varRpart <- varRpart[varRpart >= minCartSelected]

for(i in seq_along(renamer)){
    names(varRpart)[names(varRpart) %in% names(renamer)[i]] <- renamer[i]
}


vRpartLvls <- names(varRpart)
names(varRpart)

rpartnames <- names(varRpart)[!names(varRpart) %in% names(renamer)]
paste(rpartnames, collapse = "', '")

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

outPath <- paste0(myPath, "/RacePercFigs/")

dev.new()
pdf(paste0(outPath, "BeSivaSelected", minSelected, "TimesC3.pdf"))
VarPlotFunc(varBesiva, paste("Variables Selected by BeSiVa", minSelected, "or More Times"))
graphics.off()

dev.new()
pdf(paste0(outPath, "CARTSelected", minCartSelected, "TimesC3.pdf"))
VarPlotFunc(varRpart,  paste("Variables Selected by CART", minCartSelected, "or More Times"))
graphics.off()

dev.new()
pdf(paste0(outPath, "TestPClPs.pdf"))
plot(density(csvOut$mrpclps * 100), main = "A comparison of PClPs on the Test Set", xlab = "Percent Closely Predicted")
lines(density(csvOut$rpartPclps * 100), lty = 2)
legend("topright", c("BeSiVa", "CART"), lty = c(1, 2))
graphics.off()

dev.new()
pdf(paste0(outPath, "ValidationPClPs.pdf"))
plot(density(csvOut$trueHldPrds * 100), main = "A comparison of PClPs on the Validation Set", xlab = "Percent Closely Predicted")
lines(density(csvOut$trueHldPrdsRP * 100), lty = 2)
legend("topright", c("BeSiVa", "CART"), lty = c(1, 2))
graphics.off()
