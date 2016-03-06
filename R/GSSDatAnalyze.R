## Begin analyzing GSS Data

source("GSSDatFix.R")
source("BeSiVaFunctions.R")


devee <- "vote12bin"
colnames(dat)


avoidcols <- c("year", "id", "ballot", "version", "issp", "formwt", "sampcode", "sample", "phase", "spanself", "spanint", "spaneng", "vote12")

## Keep vote12, and the sample/weight info out of the data
colstouse <- colnames(dat)[!colnames(dat) %in% avoidcols]


