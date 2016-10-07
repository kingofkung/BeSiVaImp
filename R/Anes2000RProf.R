## This is where we'll store our analyses of the ANES data
## getwd()
## source("/Users/bjr/GitHub/BeSiVaImp/R/OpenANES2000.R")
rm(list = ls())
anes2000 <- read.csv("/Users/bjr/GitHub/BeSiVaImp/Data/anes2000.csv")
source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/RecodeANES2000.R")


writeloc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/"
note <- ""

## head(anes2000)



varstoreallyuse <- c("ednum" = "ednum", "pidstr" = "pidstr", "agesq" = "agesq", "age" = "age", "minority" = "minority", "sex" = "sex", "incNum" = "incNum", "houseTimeNum" = "houseTimeNum", "south" = "south", "divorced" = "divorced", "churchBin" = "churchBin", "daysreadpaper" = "daysreadpaper", "polEff" = "polEff", "partyContact" = "partyContact", "demContact" = "demContact", "repContact" = "repContact", "otherContact" = "otherContact")

Rprof(file1 <- paste0(writeloc, "testRprof.txt"))
testbes <- besiva("bindep", names(varstoreallyuse), anes2000[],
                      iters = 5, sampseed = 100000,
                      showoutput = F, showforms = F)
Rprof()
summaryRprof(file1)

library(prof.tree)
tree1 <- prof.tree(file1)
str(tree1)
print(tree1, limit = NULL)
plot(tree1)


library(microbenchmark)
micbench <- microbenchmark(
    testbes2 <- besiva("bindep", names(varstoreallyuse), anes2000[],
                      iters = 5, sampseed = 100000,
                       showoutput = F, showforms = F)
)
