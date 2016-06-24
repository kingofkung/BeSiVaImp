## This is where we'll store our analyses of the ANES data
getwd()
source("/Users/bjr/GitHub/BeSiVaImp/R/OpenANES.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")

## make dependent binary variable
head(anes48)
anes48$bindep <- ifelse(anes48$vcf0702 %in% "2. Yes, voted", 1, 0)

multiunique <- unlist(lapply(anes48[, ],function(x) length(unique(x))>1))

anes48 <- anes48[, multiunique]


colstouse <- colnames(anes48)
colstouse <- colstouse[ !colstouse %in% c("vcf0702", "bindep", "vcf0704", "vcf0704a", "vcf0705", "vcf0706", "vcf0734")]

bes1 <- besiva("bindep", colstouse, dat = anes48, iters = 3, perc = .33)
bes1$pcps

glm(bindep ~ vcf0737, data = anes48)
