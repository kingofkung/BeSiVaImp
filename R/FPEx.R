## Supporting the Populist: The Sanders Campaign and the 2016
## Democratic Primary.

library(rockchalk)
library(ggplot2)
library(caret)

## The hashtags at the start comment a line out, meaning I can write
## whatever I want and R will ignore it. It's handy for leaving
## notes/thourghts/what I was trying to do.

## Read in and attach the data
anes <- read.csv("/Users/bjr/Dropbox/Lab POLS 306_Fall 2016/Lab Materials/DataHuntFindings/anes_pilot_2016recoded.csv")
anes <- anes[, -nearZeroVar(anes)]
anes <- anes[, ]

levels(anes$ua)
levels(anes$os)


set.seed(12345)
tr <- sample(nrow(anes), round(nrow(anes)* .2))
sort(tr)


facs <- unlist(lapply(anes, is.factor))
debug(bettercpf)
bettercpf(anes[,c("ua", "os")], tr, names(facs[facs %in% c("ua", "os")]))

varstouse <- colnames(anes)


source("besivafunctionslm.R")
realvarstouse <- varstouse[!varstouse %in% "fttrump"]

fvars <- names(unlist(lapply(anes[, c("ftsanders", realvarstouse)], is.factor)))

tst <- besivalm("fttrump", realvarstouse, anes, iters = 5, showforms = T, sampseed = 45 )

tstmod <- lm(ftsanders ~ skintone + skintone_mob + ideo5num, data = anes[-tr,])


## besivalm("mpg", colnames(mtcars)[-1], mtcars, sampseed = 2)


## Outreg is part of the rockchalk package, and it makes it easy to
## copy and paste the table into Word.
## outreg(lmod, type = 'html')
