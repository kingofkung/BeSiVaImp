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

## So I'll be going with a question concerning the sanders Campaign in
## the 2016 presidential primary, as it's something no student did and
## should illustrate what we're trying to show them.

anes$ftsanders ## The DV. A feeling thermometer of Bernie Sanders
anes$ladder ## A categorical IV. They actually need one categorical or continuous IV, but we'll do everything for each.
## This was just because I was curious
anes$ladder <- factor(anes$ladder, levels = levels(anes$ladder)[c(1, 5, 3, 7, 4, 6, 2)])
anes$birthyr ## A continuous IV. We'll use this in the place of one of the controls.
anes$pid7num ## And one control. We've used PID as a continuous and control variable




varstoavoid <- c("ftsanders")
varstouse <- colnames(anes)[!colnames(anes) %in% varstoavoid]

theform <- formula(ftsanders ~ ladder + birthyr + pid7num)

fullmod <- lm(theform, data = anes)
summary(fullmod)

rmses <- lapply(1:40, function(i, myform = theform){
    set.seed(i)
    anesSub <- sample(1:nrow(anes), size = round(nrow(anes) * .2))
    lmod <- lm(theform, data = anes[-anesSub,] )
    RMSE(predict(lmod, newdata = anes[anesSub,]), anes[anesSub, "ftsanders"], TRUE)
})
rmses <- unlist(rmses)
summarize(rmses)
hist(rmses)



## For when we do it and don't want to rely on caret
tstrmse <- sqrt(mean(
    (anes[anesSub, "ftsanders"] - predict(lmod, newdata = anes[anesSub,]))^2,
    na.rm = TRUE))

## Outreg is part of the rockchalk package, and it makes it easy to
## copy and paste the table into Word.
## outreg(lmod, type = 'html')
