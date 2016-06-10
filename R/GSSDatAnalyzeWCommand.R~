## Begin analyzing GSS Data
rm(list = ls())
source("GSSDatFix.R")
source("BeSiVaFunctions.R")


devee <- "vote12bin"
## colnames(dat)


## Keep some data held out
## First, make sure our DV Is included in all cases rows
dat2 <- dat[complete.cases(dat$vote12bin),]
dat2$vote12bin



## get the sample of rows
set.seed(12345)
test <- sample(seq_along(dat2$vote12bin), size = round(nrow(dat2)/10))


## Figure out which rows have few categories, so we can eliminate them later.
ncats <- lapply(colnames(dat2), function(x) length(unique(dat2[-test,x])))



## Keep some columns from being used. Specifically, columns that have
## either 1 or over 100 values, and those that
avoidcols <- c("year", "id", "ballot", "version", "issp", "formwt", "sampcode", "sample", "phase", "spanself", "spanint", "spaneng", "vote12","wtss", "wtssnr", "wtssall", "vrstrat", "vpsu", "vote12bin", "pres12", "if12who","wtcomb",
               colnames(dat)[ which(ncats>50)], colnames(dat)[ which(ncats==1)] )


## How can I figure out if a column is now all na's?


whichcols <- lapply(colnames(dat2), function(x) all(is.na(dat2[-test ,x]))) == TRUE

allnas <- colnames(dat2)[ whichcols ]

## Now, how can I figure out if a column is majority nas?
library(caret)
mostlynas <- colnames(dat2)[nearZeroVar(dat2[-test,])]


## dat2[-test, 3]



napercs <- lapply(colnames(dat2), function(x)  sum(is.na(dat2[-test, x]))/2137  )


varstoinc <-"" ##c("partyid","degree")  ##c("partyid", "degree", "sex", "race")
noVote08 <- "vote08"
avoidcols <- c(avoidcols, allnas, mostlynas, colnames(dat2)[which(napercs>.8)], varstoinc)##, noVote08)


## Keep vote12, and the sample/weight info out of the data
colstouse <- colnames(dat2)[!colnames(dat2) %in% avoidcols]

length(colnames(dat2)) -  length(unique(avoidcols))

length(unique(colstouse))

colstoreallyuse <- colstouse[1:30]

besiva("vote12bin", colstoreallyuse, dat2, iters = 1, perc = .1, thresh = 0)



## Placein will be where we store the variables we want included

## ifelse(varstoinc == "", placein <- "", placein <- paste("+", paste(varstoinc, collapse = " + ")))

## formulae <- lapply(colstouse, function(x) as.formula(paste0(devee, " ~ ",x, placein  )) )

## if(exists("glms")) rm(glms)
## glms <- lapply(formulae, function(x){
##     ## cat("\n\n")
##     ## print(x)

##     try(
##         y <- glm(x, family = "binomial", data = dat2[-test, ])
##         ## y <- eval(bquote(glm(.(x), data = dat2[-test,])))
##         ## y <- glm(eval(bquote(.(as.formula(charform)))) , data = dat2[-test,])

##     )
##     ## try(print(y))
## }
##                )

## ## glm(vote12bin ~ pres08 + vote08, family = "binomial", data = dat2[-test,])

## ## model.matrix(vote12bin ~ pres08 + vote08, data = dat2[-test,])

## ## formulae[[733]]
## ## dat2$relhhd8


## ## print(glms)

## head(glms)


## ## lapply(glms, function(x) try(coef(x)))


## if(exists("predictions")) rm(predictions)
## predictions <- lapply(glms, function(x)  try( predictr( x  , data = dat2, rowstouse = test)) )


## pcps <- unlist(lapply(predictions, function(x)  getpcp(x, dat2$vote12bin[test], fullpreds = TRUE) ))

## IVs <- unlist(lapply(formulae, function(x) as.character(x)[3]))

## ## ncorr <- as.numeric(pcps)*length(dat2$vote12bin[test])

## nobserv <- unlist(lapply(predictions,function(x) length(na.omit(x)) ))
## ncorr <- as.numeric(pcps) * length(test)
## altpcps <- ncorr/nobserv

## roundoutput <- data.frame(IVs,
##                           "pcps" =  as.numeric(pcps),
##                           "ncorr" = ncorr,
##                           "nobserv" = nobserv,
##                           "altpcps" = altpcps
##                           ) [order(pcps, decreasing = FALSE), ]

## print(roundoutput[order(roundoutput$altpcps),])

## prop.table(table(dat2$vote12bin[test]))

## probchildren <- roundoutput[roundoutput$ncorr == 0,]
## ## print(probchildren)




## system("afplay /System/Library/Sounds/Hero.aiff")
## ## Create mobilization model based on likely contact
## ## actassoc lets us discern how important being in an organization is to a person, although it might need some recodes
## ## clsetown might let us say that someone is more central in their network, as feeling close to their town suggests association, more so than actassoc
## ## volmonth, which is all about if a person volunteered and not for pay, useful for associating with
## ## grpwork,
## colnames(dat2)[ grep("act", colnames(dat2), T)]
## varlabs
## rnhmod <- glm(vote12bin ~ wrkstat + grpsprts + grprelig + actassoc + degree + rincome, data = dat2[-test,], family = "binomial")
## summary(rnhmod)

## limrnhmod <- glm(vote12bin ~ actassoc+ degree, data = dat2[-test,], family = "binomial")
## bessiesmod <- glm(vote12bin ~ educ + vote08, data = dat2[-test,], family = "binomial")

## ## Get pcp for the rosenthal and Hansen model
##  getpcp( predictr(bessiesmod, data = dat2, test), dat2[test, devee], T )
## table(predictr(rnhmod, dat2, test))

## summary(glms[[56]])
## pcps[[56]]

## library(rockchalk)
## outreg(list("party ID" = glms[[56]]), title = "this is a test")
## outreg(list(rnhmod), title = "this is a test")
## varlabs[grep("comm", varlabs, F)]
## colnames(dat2)

## table(dat2$vote12bin[-test], dat2$grprelig[-test])
