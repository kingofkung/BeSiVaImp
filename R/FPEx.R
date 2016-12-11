## Supporting the Populist: The Sanders Campaign and the 2016
## Democratic Primary.
rm(list = ls()[!ls()%in% "anes"] )
source("BeSiVaFunctionslm.R")
library(rockchalk)
library(ggplot2)
library(caret)

writeloc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/C3/"

## The hashtags at the start comment a line out, meaning I can write
## whatever I want and R will ignore it. It's handy for leaving
## notes/thourghts/what I was trying to do.

## Read in and attach the data
anes <- read.csv("/Users/bjr/Dropbox/Lab POLS 306_Fall 2016/Lab Materials/DataHuntFindings/anes_pilot_2016recoded.csv")
## Get rid of variables with low variance and high proportions of NA's
anes <- anes[, -nearZeroVar(anes)]
mostlynas <- sapply(anes, function(x) sum(is.na(x))/length(x))
anes <- anes[,mostlynas<.7 ]

## Get the test rows separately
set.seed(45)
tr <- sample(nrow(anes), round(nrow(anes)* .2))
sort(tr)

anes$age <- 2016 - anes$birthyr

varstouse <- colnames(anes)

realvarstouse <- varstouse[!varstouse %in% c("fttrump", "repcand", 'starttime', "count", "optintimestamp", "ua", "os", "browser")]

## fvars <- names(unlist(lapply(anes[, c("ftsanders", realvarstouse)], is.factor)))


## Make a few preliminary plots:
dev.new()
pdf(paste0(writeloc, "OverallTrumpFeelings.pdf"))
hist(anes[ , "fttrump"], freq = F,
     main = "Trump Support Across ANES Respondents",
     xlab = "Trump Feeling Thermometer Response")
lines(density(anes$fttrump, na.rm = TRUE))
graphics.off()

dev.new()
pdf(paste0(writeloc, "OverallTrumpFeelingsLogged.pdf"))
hist(log(anes[ , "fttrump"]), freq = F,
     main = "Trump Support Across ANES Respondents",
     xlab = "Logarithm of Trump Feeling Thermometer Response")
lines(density(log(anes$fttrump), na.rm = TRUE))
graphics.off()


dev.new()
pdf(paste0(writeloc, "RepublicanTrumpFeelings.pdf"))
hist(anes[grepl("republican", anes$pid7, TRUE) , "fttrump"],
     main = "Republican Affect Towards Donald Trump",
     xlab = "Feeling Thermometer of Donald Trump")
graphics.off()







## Recode race to minority, in conjunction with
anes$minority <- ifelse(anes$race %in% "White", 0, 1)
anes$minority[is.na(anes$race)] <- NA
table(anes$race, anes$minority, useNA = "always")

myform <- fttrump ~ age + minority + gender + pid7num + ideo5num
## myform <- fttrump ~ rr1  + violenth + birthright_b
## myform <- fttrump ~ rr1 + violenth + birthright_b + age + minority + gender + pid7num + ideo5num
trsupprmses <- unlist(lapply(1:100, function(x){
    print(x)
    set.seed(x)
    tr <- sample(1:nrow(anes), round(nrow(anes) * .2))
    modo <- lm(myform, anes[-tr,])
    ## pr <- predict(modo, newdata = fixbadlevels(anes[tr,], modo))
    getrmses(modo, anes, "fttrump", tr)
}
))
summarize(trsupprmses)
##
## Plot RMSES on a histogram
dev.new()
pdf(paste0(writeloc, "trumpSupportRMSE.pdf"))
hist(trsupprmses, freq = F)
lines(density(trsupprmses, na.rm = T))
graphics.off()

## Make full model
fullmodo <- lm(myform, anes)
## outreg(fullmodo, type = "html", tight = FALSE)
summary(fullmodo)





closeness <- 20
varls <- lapply(1:5, function(i){
    print(paste("iter = ", i))
    tst <- besivalm("fttrump", sort(realvarstouse), anes,
                    iters = 5, thresh = 1E-5, sampseed = i, hc = closeness, showoutput = FALSE, showforms = FALSE)
    c("intvars" = list(tst$intvars), "maxpclp" = max(tst$pclps))
})
varlintvar <- unlist( lapply(varls, function(x) x$intvars))
varlpclp <- unlist( lapply(varls, function(x) x$maxpclp))
##
dev.new()
pdf(paste0(writeloc, "pclp on fttrump; closeness = ", closeness, ".pdf"))
hist(varlpclp, main = paste0("PCLP on Trump Feeling Thermometer\n","closeness = ", closeness))
graphics.off()

## Bernie, Bernie
lapply(1:5, function(i){
    bern <- besivalm("ftsanders", c("fttrump", realvarstouse), dat = anes, sampseed = i, perc = .2, hc = 10, showforms = F, showoutput = F)
    max(bern$pclps)
})

summarize(varlpclp)

sort(table(varlintvar))

smallrangevar <- c(realvarstouse[!realvarstouse %in% grep("pid", colnames(anes), value = T)], 'fttrump')
pideotry <- lapply(1:10, function(x){
    print(paste("iter =", x))
##
    junker <- besivalm("pid7num", smallrangevar, anes, hc = 1, sampseed = x, showforms = FALSE, showoutput = FALSE)
    junker
}
                  )

ideopclps <- unlist(lapply(pideotry, function(x) max(x$pclps)))
summarize(ideopclps)
ideointvars <- lapply(pideotry, function(x) max(x$intvars))

range(mtcars$mpg)
mpgMods <- lapply(1:100, function(i) besivalm("mpg", colnames(mtcars)[!colnames(mtcars) %in% "mpg"], mtcars, perc = .5, hc = 2, thresh = .001, sampseed = i))

mpgMods[[1]]$tstrows
mpgPCLPs <- unlist(lapply(mpgMods, function(x) max(x$pclps)))
table(mpgPCLPs)
hist(mpgPCLPs, breaks = 5)

source("BeSiVaFunctions.R")

## anes$trumpFans <- ifelse(anes$fttrump > 50, 1, 0)
## ## table(anes$fttrump, anes$trumpFans)
## prop.table(table(anes$trumpFans))


## tf <- lapply(1:10, function(i){
##     tfbin <- besiva("trumpFans", sort(realvarstouse), anes, sampseed = i, showforms = FALSE)
##     ## anes$trumpFans[unlist(tfbin$tstrows)]
##     tfbin})
## selectvars <- unlist(lapply(tf, function(x) x$intvars))
## sort(table(selectvars), decreasing = TRUE)
## pcpVals <- unlist(lapply(tf, function(x) max(x$pcps)))
## hist(pcpVals)


## install.packages(c("GGally", "VGAM"))
library(VGAM)


## predict( modmakertobit(myform, anes[-tr,]))[, 'mu']


closethresh <- 10
##
tobcriters <- lapply(1:100, function(i){
    print(i)
    set.seed(i)
    testingrows <- sample(1:nrow(anes), nrow(anes)*.2)
    tsttobit <- modmakertobit(myform, anes[-testingrows,])
##
    ##
    ## browser()
    tobpreds <- predict(tsttobit, newdata = anes[testingrows,])[,'mu']
    tobobs <- anes$fttrump[testingrows[testingrows %in% names(tobpreds)]]
    c(RMSE(tobpreds, tobobs, TRUE),
    ## Note: The NA rows are taken out, requiring the correction seen below
    makepclp(tsttobit, tobobs, tobpreds, closethresh)*length(tobobs)/length(testingrows))
})
tobcriters <- as.data.frame(do.call(rbind, tobcriters))
colnames(tobcriters) <- c("rmse", "pclp")
summarizeNumerics(tobcriters)

hist(tobcriters$pclp)
source("BeSiVaFunctionslm.R")
library(VGAM)
trumptobit <- besivatobit('fttrump', sort(realvarstouse), anes, hc = 10)
summarize(trumptobit$pclps)


anes$trumpBin <- ifelse( anes$repcand == "Donald Trump" ,1, 0)
anes$trumpBin[anes$repcand %in% "None"] <- NA
## table(anes$trumpBin, anes$repcand, useNA = "always")
## prop.table(table(anes$trumpBin))

PCPTrump <- lapply(1:200,function(i){
    print(paste("iter =", i))
##
    trumpAlg <- besiva("trumpBin", sort(realvarstouse), sampseed = i,
                       perc = .2, anes, showforms = F, showoutput = F)
    max(trumpAlg$pcps)})
PCPTrump <- unlist(PCPTrump)
write.csv((PCPTrump), paste0(writeloc,"trumpBeSiVaLogitPCPs.csv"))

dev.new()
pdf(paste0(writeloc, "BeSiVaLogitTrump.pdf"))
hist(PCPTrump)
graphics.off()

anes$bernieBin <- ifelse(anes$demcand == "Bernie Sanders", 1, 0)
anes$bernieBin[anes$demcand %in% "None"] <- NA
table(anes$bernieBin, anes$demcand, useNA = "always")
prop.table(table(anes$bernieBin))


varsftb <- realvarstouse[!realvarstouse %in% c("ftsanders", "demcand")]
varsftb <- c(varsftb, "fttrump")

besivaBern <- lapply(1:100, function(i){
    print(paste("iter =", i))
        junker <- besiva("bernieBin", sort(varsftb), sampseed = i,
                       perc = .2, anes, showforms = F, showoutput = F)
}
                     )
bernVars <- unlist(lapply(besivaBern, function(x) x$intvars))
PCPBern <- unlist(lapply(besivaBern, function(x) max(x$pcps)))
summarize(PCPBern)

dev.new()
pdf(paste0(writeloc, "BeSiVaLogitBernie.pdf"))
hist(PCPBern)
graphics.off()
