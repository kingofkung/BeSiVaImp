## Make some plots from the data I got from CART/BeSiVa for Comparison Purposes
myPath <- "~/Dropbox/Dissertation Stuff/DatOutPut/C3/"
csvOut <- read.csv(paste0(myPath, "armdv11to100missthresh of573.csv"))

head(csvOut)

## Turn column into vector
colToTable <- function(x){
    ##
    ## browser()
    myIVs <- paste(as.character(x), collapse = ",")
    myIVs <- gsub(",+", " ", myIVs)
    myIVs <- unlist(strsplit(myIVs, " "))
    sort(table(myIVs[!myIVs %in% ""]), TRUE)
}

varBesiva <- colToTable(csvOut$mrintForDf)
varRpart <- colToTable(csvOut$rpartIntVars)

varBesiva[varBesiva > 1]
varRpart[varRpart > 1]

rockchalk::summarizeNumerics(csvOut[, -1], F)

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

