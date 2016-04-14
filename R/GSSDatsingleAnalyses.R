hist(roundoutput$pcps)

badvars <- as.character(round1output[(nrow(round1output)-3):nrow(round1output),"IVs"])

head(dat2[, badvars])

convmod <- glm(vote12bin ~ partyid + degree + race + age + income, data = dat2[-test,])

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
