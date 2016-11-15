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
set.seed(12345)
tr <- sample(nrow(anes), round(nrow(anes)* .2))
sort(tr)



varstouse <- colnames(anes)

realvarstouse <- varstouse[!varstouse %in% c("fttrump", "ua", "os", "browser", "repcand")]

fvars <- names(unlist(lapply(anes[, c("ftsanders", realvarstouse)], is.factor)))

tst <- besivalm("fttrump", realvarstouse, anes, iters = 5, showforms = T, sampseed = 45, thresh = .01 )

## sort(tst$rmses)

vars <- c(tst$intvars[[1]][2:5], "religpew")

bsform1 <- as.formula(paste("fttrump ~", paste(vars, collapse = " + ")))

mod <- modmakerlm(fttrump ~ syrians_b + pid3, anes[-tr,])
summary(mod)

tstfun <- function(model, mydat = NULL){
    model.frame(model, data = mydat)
}
tstfun(mod, anes[-tr,])

## facstat <- lapply(model.frame(model), is.factor)
## facnames <- names(facstat[unlist(facstat)])

## rownums <- as.numeric(rownames(model.frame(modpull)))

## smalldf <- anes[c(rownums, tr),]
## smalldf <- bettercpf(smalldf, seq_along(tr) + length(rownums), facnames)

getrmses(mod, anes, "fttrump", tr)

RMSE(predict(mod, newdata = anes[tr,]), anes[tr, "fttrump"], TRUE)
