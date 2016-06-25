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

avoidcols <- c("vcf0702", "bindep", "vcf0706", "vcf0704", "vcf0704a", "vcf0705", "vcf0716", "vcf0015a", "vcf0712", "vcf0737" , "vcf0734")
colstouse <- colnames(anes48)[ !colnames(anes48) %in% avoidcols]
length(colstouse)
## View(anes48)

bes1 <- besiva("bindep", colstouse, dat = anes48, iters = 5, perc = .25)
names(bes1)
bes1$tieforms

glm(bindep ~ vcf0127 + vcf9027 + vcf0713, data = anes48)



anes52$bindep <- ifelse(anes52$vcf0702 %in% "2. Yes, voted", 1, 0)

## eliminate any variable with fewer than 1 category, making sure to
## remove NAs as a unique option
fewcats <- names(which(!sapply(anes52, function(x) length(unique(na.omit(x)))) > 1))
anes52$vcf0009x

avoidcols2 <- c(avoidcols, "vcf0703", fewcats)
colstouse2 <- colnames(anes52)[!colnames(anes52) %in%  avoidcols2]
bes2 <- besiva("bindep", colstouse2, dat = anes52, perc = .25)





## The problem, illustrated
mod <- glm(bindep ~ vcf0378d + vcf0127, "binomial", data = anes52[-bes2$tstrows,])
## if you run the line below, it'll return an error instead of
## predictions due to the new categories in vcf0378d's test set


## predictr(mod, anes52, bes2$tstrows)

## working on a fix
ivsused <- as.character(formula(mod)[[3]][-1])

testuniques <- sapply(anes52[bes2$tstrows, ivsused ], unique )
moduniques <- sapply(model.frame(mod)[ , -1], unique)


findnew <- function(x, testvals = testuniques , modvals = moduniques){
    jn <- testvals[[x]][ !testvals[[x]] %in% modvals[[x]] ]
    jn <- jn[!is.na(jn)]
    jn
    }

newlvls <- lapply(1:2, findnew)


lapply(newlvls, function(x) as.character(x))

testuniques[ !testuniques[i] %in% moduniques[i]])


predictr( jnk, data = anes52, bes2$tstrows)
