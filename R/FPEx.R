## Supporting the Populist: The Sanders Campaign and the 2016
## Democratic Primary.
rm(list = ls())
source("BeSiVaFunctionslm.R")
library(rockchalk)
library(ggplot2)
library(caret)

## The hashtags at the start comment a line out, meaning I can write
## whatever I want and R will ignore it. It's handy for leaving
## notes/thourghts/what I was trying to do.

## Read in and attach the data
anes <- read.csv("/Users/bjr/Dropbox/Lab POLS 306_Fall 2016/Lab Materials/DataHuntFindings/anes_pilot_2016recoded.csv")
## Get rid of variables with low variance and high proportions of NA's
anes <- anes[, -nearZeroVar(anes)]
mostlynas <- sapply(anes, function(x) sum(is.na(x))/length(x))
anes <- anes[,mostlynas<.5 ]

## Get the test rows separately
set.seed(45)
tr <- sample(nrow(anes), round(nrow(anes)* .2))
sort(tr)



varstouse <- colnames(anes)

realvarstouse <- varstouse[!varstouse %in% c("fttrump", "ua", "os", "browser", "repcand")]

fvars <- names(unlist(lapply(anes[, c("ftsanders", realvarstouse)], is.factor)))


## sort(tst$rmses)

hist(anes[ , "fttrump"], main = "Affect Towards Donald Trump")
hist(anes[grepl("republican", anes$pid7, TRUE) , "fttrump"], main = "Republican Affect Towards Donald Trump", xlab = "Feeling Thermometer of Donald Trump")

mod <- modmakerlm(fttrump ~ syrians_b + pid3, anes[-tr,])
summary(mod)


##' remove levels that are not useful fory
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @param facnames names of factor variables in the dataset
##' @param trdat the training data frame
##' @param tesdat the test data frame
##' @return the test dataframe column, with levels that are being a problem set to NA
##' @author Benjamin Rogers
lvlrm <- function(x, facnames = facvarnames, trdat, tesdat){
        ## Get the levels for the factor of choice
        fac <- facnames[x]
        trlvls <- unique(factor(trdat[,fac]))
        teslvls <- unique(factor(tesdat[,fac]))
        ## Figure out which levels are/aren't in the training data.
        ## These are bad levels, as they screw with our ability to predict
        badlvlbool <- !teslvls %in% trlvls
        badlvls <- teslvls[badlvlbool]
        ## Remove the bad levels, and return the column without the bad levels
        tesdat[tesdat[, fac] %in% badlvls, fac] <- NA
        tesdat[ , fac]
    }

set.seed(5)
tr1 <- sample(1:nrow(anes), round(nrow(anes) * .2))
## New Tr
tr <- c(tr1, which(anes$race %in% "Other"))
table(as.character(anes[-tr, "race"] ))
table(anes[tr, "race"])
table(lvlrm(3, c("pid7", "rr1", "race", "gender", "syrians_b", "os"), anes[-tr,], anes[tr, ]))

bettercpf(anes[, c("pid7", "rr1", "race", "gender", "syrians_b", "os")], tr, c("pid7", "rr1", "race", "gender", "syrians_b", "os"))



m <- lm(fttrump ~ pid7 + rr1 + gender + race + syrians_b, anes[,])
summary(m)
trsupp <- unlist(lapply(1:100, function(x){
    print(x)
    set.seed(x)
    tr <- sample(1:nrow(anes), round(nrow(anes) * .2))
    modo <- lm(fttrump ~ pid7 + rr1 + gender + race + syrians_b, anes[-tr,])
    predict(modo, newdata = anes[tr, ])

}
))
rockchalk::summarize(trsupp)
hist(trsupp)

