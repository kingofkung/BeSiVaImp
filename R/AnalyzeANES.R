## This is where we'll store our analyses of the ANES data
getwd()
source("/Users/bjr/GitHub/BeSiVaImp/R/OpenANES.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")

## make dependent binary variable
head(anes48)
anes48$bindep <- ifelse(anes48$vcf0702 %in% "2. Yes, voted", 1, 0)

multiunique <- unlist(lapply(anes48[, ],function(x) length(unique(x))>1))

anes48 <- anes48[, multiunique]

sapply(anes48, function(x) length(unique(x)))

## internally disputing on whether to avoid vcf0734 (intended v actual
## vote). On the one hand, it tends towards perfect separation after
## the second iteration (with vcf0110, a measure of education). On the other hand, it's not
## exactly obvious what else to do about the separation.
table(anes48$bindep, anes48$vcf0734)
table(anes48$vcf0102, anes48$vcf0103)

avoidcols <- c("vcf0702", "bindep", "vcf0706", "vcf0704", "vcf0704a", "vcf0705", "vcf0716", "vcf0015a", "vcf0712", "vcf0737" , "vcf0734", "vcf0103")
colstouse <- colnames(anes48)[ !colnames(anes48) %in% avoidcols]
length(colstouse)
## View(anes48)

bes1 <- besiva("bindep", colstouse, dat = anes48, iters = 2, perc = .15, thresh = .001, sampseed = i, showoutput = T)

data.frame("pcps" = bes1$pcps, "formulae" = as.character(bes1$forms))[order(bes1$pcps), ]
model.frame(bindep ~ vcf0014 + vcf0713, data = anes48[-bes1$tstrows, ])

bm <- glm(bindep ~ vcf0713, data = anes48, binomial)
summary(bm)

## names(bes1)
bes1$tieforms




anes52$bindep <- ifelse(anes52$vcf0702 %in% "2. Yes, voted", 1, 0)

## eliminate any variable with fewer than 1 category, making sure to
## remove NAs as a unique option

sapply(anes52, function(x) length(unique(na.omit(x))) > 1 )

onecat <- names(which(sapply(anes52, function(x) length(unique(na.omit(x)))) == 1))

fewcats <- names(which(!sapply(anes52, function(x) length(unique(na.omit(x)))) > 1))
## anes52$vcf0009x

avoidcols2 <- c(avoidcols, "vcf0703", fewcats, "vcf9023", "vcf0901", "vcf0701")
colstouse2 <- colnames(anes52)[!colnames(anes52) %in%  avoidcols2]
bes2 <- besiva("bindep", colstouse2, dat = anes52, perc = .25, thresh =.01, iters = 4, sampseed = 12345)
bes2$pcps
data.frame(as.character(bes2$forms), bes2$pcps)[   order(bes2$pcps),]

ivside <- paste(unlist(bes2$intvars), collapse = " + ")
bm2 <- glm(as.formula(paste0("bindep ~", ivside )), data = anes52, family = "binomial")
summary(bm2)

