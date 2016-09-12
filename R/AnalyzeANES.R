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

for(i in 1:100){
    bes1 <- besiva("bindep", colstouse, dat = anes48, iters = 4, perc = .15, sampseed = i, showoutput = F)
    ifelse(i == 1, savvar <- unique(bes1$intvars), savvar <- c(savvar,unique(bes1$intvars)))
}
savvar

which(lapply(bes1$glms, class) == "try-error")

sort(table(unlist(savvar)))

data.frame("pcps" = bes1$pcps, "formulae" = as.character(bes1$forms))[order(bes1$pcps), ]
pmf <- model.frame(bindep ~ vcf0014 + vcf0713, data = anes48[-bes1$tstrows, ])
table(pmf[,2], pmf[,3])

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

avoidcols2 <- c(avoidcols, "vcf0703", fewcats, "vcf9023", "vcf0901", "vcf0701", "vcf0715")
colstouse2 <- colnames(anes52)[!colnames(anes52) %in%  avoidcols2]
bes2 <- besiva("bindep", colstouse2, dat = anes52, perc = .25, thresh =.01, iters = 4, sampseed = 12345)
bes2$pcps
data.frame(as.character(bes2$forms), bes2$pcps)[   order(bes2$pcps),]

ivside <- paste(unlist(bes2$intvars), collapse = " + ")
bm2 <- glm(as.formula(paste0("bindep ~", ivside )), data = anes52, family = "binomial")
summary(bm2)

## Meet w/Dr. Joslyn
## Move onto 2000... is vote intent still king?

## Take some theories: so
## Michigan: determinants
## sociological: determinants
## R + H: determinants
## Retrospective(FIorina)
## etc...
## and use just those variables across all years.

## may have to leave out vote intent

## Try to standardize across years...
## Family/personal income
## Education
## Age, but be consistent!

head(anes2000)

anes2000$bindep <- ifelse(anes2000$vcf0702 %in% "2. Yes, voted", 1, 0)

anes2000$vcf9030a
## Efficacy? Personal efficacy == VCF0609


varstouse <- c("pid7" = "vcf0301", "daysreadpaper" = "vcf9033", "polEff" =  "vcf0609", "ed" = "vcf0140a", "incGroup" = "vcf0114", "marital" = "vcf0147", "race7" = "vcf0105a", "region" = "vcf0112", "sex" = "vcf0104", "partyContact" = "vcf9030a", "demContact" = "vcf9030b", "repContact" = "vcf9030c", "otherContact" = "vcf9031", "work7" = "vcf0116" , "church" = "vcf0130")
vtunn <- varstouse
names(vtunn) <- NULL

for(i in 1:20) {
    print(paste0("i = ", i))
    bes2000 <- besiva("bindep", vtunn, anes2000, iters = 5, sampseed = i, showoutput = F, showforms = F, thresh = .00001)
    ifelse(i == 1, savvars <-  bes2000$intvars, savvars <- c(savvars, bes2000$intvars))
    ifelse(i == 1, savpcp <- max(bes2000$intpcps, na.rm = T), savpcp <- c(savpcp, max(bes2000$intpcps, na.rm = T)))
}
savvarsU <- unlist(savvars)
savvartab <- sort(table(savvarsU), decreasing = T)

table(unlist(lapply(savvars, length)))

## swap useless names for useful ones
names(varstouse)[ match(names(savvartab) , varstouse)]

sort(savvartab, T)

table(savpcp)
hist(savpcp)

## str(bes2000)
