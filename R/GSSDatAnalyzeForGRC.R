## Begin analyzing GSS Data
rm(list = ls())
source("GSSDatFix.R")
source("BeSiVaFunctions.R")

## recode DV
dat$acqcopsbin <- as.character(dat$trtcops)
dat$acqcopsbin[ dat$acqcopsbin %in% c("1", "2-5", "6-10", "More than 10")] <- "1"
dat$acqcopsbin <- as.numeric(dat$acqcopsbin)


devee <- "acqcopsbin"
## colnames(dat)


## Keep some data held out
## First, make sure our DV Is included in all cases rows

dat2 <- dat[complete.cases(dat[, devee]),]




## get the sample of rows
set.seed(12345)
test <- sample(seq_along(dat2$acqcopsbin), size = round(nrow(dat2)/5))


## Figure out which rows have few categories, so we can eliminate them later.
ncats <- lapply(colnames(dat2), function(x) length(unique(dat2[-test,x])))



## Keep some columns from being used. Specifically, columns that have
## either 1 or over 100 values, and those that
avoidcols <- c("year", "id", "ballot", "version", "issp", "formwt", "sampcode", "sample", "phase", "spanself", "spanint", "spaneng", "vote12","wtss", "wtssnr", "wtssall", "vrstrat", "vpsu", "vote12bin","wtcomb", "form", "mode",   "vote08", "acqcopsbin", "trtcops",
               colnames(dat)[ which(ncats>50)], colnames(dat)[ which(ncats==1)] )


## How can I figure out if a column is now all na's?

## all(is.na(dat2[,"othpolw"]))

whichcols <- lapply(colnames(dat2), function(x) all(is.na(dat2[-test ,x]))) == TRUE

allnas <- colnames(dat2)[ whichcols ]

## Now, how can I figure out if a column is majority nas?
library(caret)
mostlynas <- colnames(dat2)[nearZeroVar(dat2[-test,])]


## dat2[-test, 3]



napercs <- lapply(colnames(dat2), function(x)  sum(is.na(dat2[-test, x]))/2137  )


varstoinc <- "" ## c("trtwhite", "rellife")  ##c("partyid", "degree", "sex", "race")
avoidcols <- c(avoidcols, allnas, mostlynas, colnames(dat2)[which(napercs>.8)], varstoinc)


## Keep vote12, and the sample/weight info out of the data
colstouse <- colnames(dat2)[!colnames(dat2) %in% avoidcols]

length(colnames(dat2)) -  length(unique(avoidcols))

length(unique(colstouse))




## Placein will be where we store the variables we want included

ifelse(varstoinc == "", placein <- "", placein <- paste("+", paste(varstoinc, collapse = " + ")))

formulae <- lapply(colstouse, function(x) as.formula(paste0(devee, " ~ ",x, placein  )) )

if(exists("glms")) rm(glms)
glms <- lapply(formulae, function(x){
    cat("\n\n")
     print(x)
    try(
        print(y <- glm(x, family = "binomial", data = dat2[-test, ]))
    )
}
               )

## glm(vote12bin ~ pres08 + vote08, family = "binomial", data = dat2[-test,])

## model.matrix(vote12bin ~ pres08 + vote08, data = dat2[-test,])

## formulae[[733]]
## dat2$relhhd8


## print(glms)

head(glms)

if(exists("predictions")) rm(predictions)
predictions <- lapply(glms, function(x)  try( predictr( x  , data = dat2, rowstouse = test)) )

lapply(predictions, function(x) length(na.omit(x)))

pcps <- unlist(lapply(predictions, function(x)  getpcp(x, dat2[test, devee], TRUE) ))

IVs <- unlist(lapply(formulae, function(x) as.character(x)[3]))

nobserv <- unlist(lapply(predictions,function(x) length(na.omit(x)) ))
ncorr <- round(as.numeric(pcps)*nobserv)


roundoutput <- data.frame(IVs,
                          "pcps" =  as.numeric(pcps),
                          "ncorr" = ncorr,
                          "nobserv" = nobserv
                          ) [order(pcps, decreasing = FALSE), ]

print(roundoutput)

roundoutput[roundoutput$IVs == "zodiac",]
prop.table(table(dat2[, devee]))

## hist(roundoutput$pcps)

## badvars <- as.character(round1output[(nrow(round1output)-3):nrow(round1output),"IVs"])

## head(dat2[, badvars])
## Now it tells us when it's done!
## system("say done")
## I got tired of it telling us it's done.
## /System/Library/PrivateFrameworks/ScreenReader.framework/Versions/A/Resources/Sounds

convmod <- glm(get(devee) ~ partyid + degree + race + age + income, data = dat2[-test,])

convpreds <- predictr(convmod, dat2[], test)
getpcp(convpreds, dat2[test, devee], FALSE)


system("afplay /System/Library/Sounds/Hero.aiff")
