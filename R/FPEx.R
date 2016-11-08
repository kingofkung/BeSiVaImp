## Supporting the Populist: The Sanders Campaign and the 2016
## Democratic Primary.

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


##########################################################################
## get summary statistics for each variable. I like summarize, as it    ##
## gets the data in a form that people will understand easier, even if  ##
## they have to load the rockchalk package using library(rockchalk) to  ##
## get it.                                                              ##
##########################################################################

library(rockchalk)
## To get the mode on a categorical variable, make a table, save it to a variable, and sort

## Subset the data with the square brackets and c() command.
## datsub is a dataset with the 3 variables we need.
datsub <- anes[, c("ftsanders", "birthyr", "pid7num")]
summarize(datsub)

## The dollar sign pops out only the variable we need from the
## dataset. This is optional since we attached it, but it'll help if
## you ever need a variable from a dataset specifically.
sort(table(anes$ladder), decreasing = T)

##########################################################################
## Now I need to make some univariate plots for each of the variables.  ##
## The DV is continuous, so we'll make a histogram                      ##
##########################################################################
hist(anes$ftsanders)
## So's one of our IVs. I'm adding a few graphical options on this one
## They're also allowed to use kernel density plots, so here's how
## you'd add that in R
hist(anes$birthyr, main = "A Histogram of Age", xlab = "Year Respondent Was Born", freq = F)
lines(density(anes$birthyr, na.rm = T))

hist(anes$pid7num)

## And now I'll make a barplot. This requries the ggplot2 package, so
## I'll load that using:
library(ggplot2)
## and make the plot with qplot
qplot(anes$ladder)



##########################################################################
## They only need to make one bivariate plot, But I'll get them both    ##
## made here.                                                           ##
##########################################################################

## Here's how you plot if you've got 2 continuous variables.
plot(ftsanders ~ birthyr, data = anes)

## This gets the feeling thermometer into groups, aggregated by how
## people feel about climbing the economic ladder.
SandersByLadder <- aggregate(anes$ftsanders, by = list(anes$ladder), mean, na.rm = T)
barplot(SandersByLadder$x, names.arg = SandersByLadder$Group.1)
## And put a horizontal line on the chart for fun
abline(h = 50)

##########################################################################
## And now, here are how to conduct the 2 bivariate tests.              ##
##########################################################################

cor.test(anes$ftsanders, anes$birthyr)

## t.test for difference of means. Pick the 2 categories that matter
## most and compare them.
anes$ladderbin <- rep(NA, times = length(anes$ladder))
anes$ladderbin[anes$ladder == "A great deal easier"] <- 0
anes$ladderbin[anes$ladder == "A great deal harder"] <- 1
t.test(ftsanders ~ ladderbin, data = anes)

##########################################################################
## And finally, a brief multiple regression.                            ##
##########################################################################
library(caret)

varstoavoid <- c("ftsanders")
varstouse <- colnames(anes)[!colnames(anes) %in% varstoavoid]

theform <- formula(ftsanders ~ ladder + birthyr + pid7num)
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
