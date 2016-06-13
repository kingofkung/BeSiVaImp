## Make a file that displays single analyses instead of the full output of BeSiVa

## This is where we'll also store any code needed to make outreg tables and charts.

if(!exists("dat2")) source("GSSDatAnalyze.R")

hist(roundoutput$pcps)


convmod <- glm(vote12bin ~ partyid + degree + race + age + income, data = dat[,])

mod08 <- glm(vote08bin ~ hhtype1 + race + wrkstat + educ, data = dat)

mod12 <- glm(vote12bin ~ othlang1, family = "binomial", data = dat2[-test,])
getpcp(predictr(mod12, dat2, test), dat2$vote12bin[test])


## The problem:  other relative-great-aunt,grandniece,etc.- is included in the test, but not the training set...
data.frame(table(dat2$relhhd2[-test]),table( dat2$relhhd2[test]))[,c(1,2,4)]

id <- levels(factor(dat2$relhhd2[test]))[!levels(factor(dat2[test, "relhhd2"])) %in%
                                       levels(factor(dat2[-test, "relhhd2"]))
                                       ]
dat2$relhhd2[ dat2$relhhd2 %in% id] <- NA

predictr(mod12, dat2[,], test)


## Best thing to do would be create a function called KillNewLevels, which can be used to do exactly what it says in the test set.



convpreds <- predictr(convmod, dat2[], test)
getpcp(convpreds, dat2[test,"vote12bin"])

m1 <- glm(vote12bin ~ vote08, data = dat2[-test,])
m2 <- update(m1, .~. + educ)

m1.1 <- update(m1, .~. -vote08 + partyid)
summary(m1.1)
m2.1 <- update(m1.1, .~. + degree)
m3.1 <- update(m2.1, .~. +sex + race)
m4.1 <- update(m3.1, .~. + rplace)
library(rockchalk)
outreg(list("Iteration 1" = m1, "Iteration 2" = m2), title = "this is a test")
outreg(list("Iteration 1" = m1.1, "Iteration 2" = m2.1, "Iteration 3" = m3.1, "Iteration 4" = m4.1), title = "Algorithm predictions, sans prior vote"   )
