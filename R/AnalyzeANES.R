## This is where we'll store our analyses of the ANES data
getwd()
source("/Users/bjr/GitHub/BeSiVaImp/R/OpenANES.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")

## make dependent binary variable
head(anes48)
anes48$bindep <- ifelse(anes48$vcf0702 %in% "2. Yes, voted", 1, 0)
anes52$bindep <- ifelse(anes52$vcf0702 %in% "2. Yes, voted", 1, 0)

multiunique <- unlist(lapply(anes48[, ],function(x) length(unique(x))>1))

anes48 <- anes48[, multiunique]

sapply(anes48, function(x) length(unique(x)))

avoidcols <- c("vcf0702", "bindep", "vcf0706", "vcf0704", "vcf0704a", "vcf0705", "vcf0734", "vcf0716", "vcf0015a")
colstouse <- colnames(anes48)[ !colnames(anes48) %in% avoidcols]
length(colstouse)
## View(anes48)

bes1 <- besiva("bindep", colstouse, dat = anes48, iters = 3, perc = .1)
bes1$pcps

ncol(anes52)

str(anes52)

nrow(anes52)

fewcats <- names(which(!sapply(anes52, function(x) length(unique(na.omit(x))))>1 ))
anes52$vcf0009x

avoidcols2 <- c(avoidcols, "vcf0703", fewcats)
colstouse <- colnames(anes52)[!colnames(anes52) %in%  avoidcols2]
bes2 <- besiva("bindep", colstouse, dat = anes52, perc = .25)
sort(bes2$pcps)
data.frame(colstouse, bes2$pcps)[bes2$pcps==0,]

