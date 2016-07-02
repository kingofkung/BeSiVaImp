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

bes1 <- besiva("bindep", colstouse, dat = anes48, iters = 5, perc = .1)
## names(bes1)
bes1$tieforms

glm(bindep ~ vcf0127 + vcf9027 + vcf0713, data = anes48)



anes52$bindep <- ifelse(anes52$vcf0702 %in% "2. Yes, voted", 1, 0)

## eliminate any variable with fewer than 1 category, making sure to
## remove NAs as a unique option
fewcats <- names(which(!sapply(anes52, function(x) length(unique(na.omit(x)))) > 1))
anes52$vcf0009x

avoidcols2 <- c(avoidcols, "vcf0703", fewcats)
colstouse2 <- colnames(anes52)[!colnames(anes52) %in%  avoidcols2]
bes2 <- besiva("bindep", colstouse2, dat = anes52, perc = .1, iters = 4, sampseed = 12345)
bes2$glms
bes2$pcps
sort(bes2$pcps)
bes2$tieforms

## The problem, illustrated
## two variables that have the issue: vcf0396d, vcf0498d
## three variables that do not: vcf0711, vcf0701, vcf0411
u <- glm(bindep ~ vcf0006 + vcf0013, "binomial", data = anes52[-bes2$tstrows,])
u <- glm(bindep ~ vcf0498d, "binomial", data = anes52[-bes2$tstrows,])

model.frame(u)

## if you run the line predictr() below, it'll return an error instead of
## predictions due to the new categories in vcf0378d's test set
## predictr(u, anes52, bes2$tstrows)
ndat <- catprobfinder(u, anes52, bes2$tstrows)
str(ndat)
ndat$muniques
ndat$tuniques

findnew(1, ndat$tuniques, ndat$muniques)



model.frame(u)
na.omit(ndat$tstdatnu)
predictr(u, ndat$tstdatnu, bes2$tstrows)

##################################################################
## It is at this point that we go from simply detecting whether levels
## are new to removing those new levels.
## Need to provide: The test data and the new levels

## tdatnu <- as.data.frame(sapply(seq_along(newlvls), function(i){
##     tdat[tdat[,i] %in%   newlvls[[i]], i] <- NA
##     tdat[,i]}
##     ))
## colnames(tdatnu) <- ivsused
## na.omit(tdatnu)
## ## table(factor(tdat[,1]), factor(tdatnu[,1]))

## predout <- predictr( u, data = tdatnu, rowstouse= seq_along(rownames(tdat)))
## print(predout)
## ## getpcp(predout, anes52$bindep[examrw])
