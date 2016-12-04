## Supporting the Populist: The Sanders Campaign and the 2016
## Democratic Primary.
rm(list = ls()[!ls()%in% "anes"] )
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
anes <- anes[,mostlynas<.7 ]

## Get the test rows separately
set.seed(45)
tr <- sample(nrow(anes), round(nrow(anes)* .2))
sort(tr)

anes$age <- 2016 - anes$birthyr

varstouse <- colnames(anes)

realvarstouse <- varstouse[!varstouse %in% c("fttrump", "repcand", 'starttime', "count")]

fvars <- names(unlist(lapply(anes[, c("ftsanders", realvarstouse)], is.factor)))


## sort(tst$rmses)

hist(anes[ , "fttrump"], main = "Affect Towards Donald Trump")
hist(anes[grepl("republican", anes$pid7, TRUE) , "fttrump"], main = "Republican Affect Towards Donald Trump", xlab = "Feeling Thermometer of Donald Trump")

mod <- modmakerlm(fttrump ~ syrians_b + pid3, anes[-tr,])
summary(mod)




set.seed(5)
tr1 <- sample(1:nrow(anes), round(nrow(anes) * .2))
## New Tr
tr <- c(tr1, which(anes$race %in% "Other"))
table(as.character(anes[-tr, "race"] ))
table(anes[tr, "race"])
table(lvlrm(3, c("pid7", "rr1", "race", "gender", "syrians_b", "os"), anes[-tr,], anes[tr, ]))

anes$minority <- ifelse(anes$race %in% "White", 0, 1)
anes$minority[is.na(anes$race)] <- NA
table(anes$race, anes$minority, useNA = "always")

myform <- fttrump ~ age + minority + gender + pid7num + ideo5num
## myform <- fttrump ~ rr1  + violenth + birthright_b
## myform <- fttrump ~ rr1 + violenth + birthright_b + age + minority + gender + pid7num + ideo5num
trsupp <- unlist(lapply(1:100, function(x){
    print(x)
    set.seed(x)
    tr <- sample(1:nrow(anes), round(nrow(anes) * .2))
    modo <- lm(myform, anes[-tr,])
    ## pr <- predict(modo, newdata = fixbadlevels(anes[tr,], modo))
    getrmses(modo, anes, "fttrump", tr)
}
))
summarize(trsupp)
hist(trsupp)
fullmodo <- lm(myform, anes)
## outreg(fullmodo, type = "html")
summary(fullmodo)


set.seed(2)
tr <- sample(1:nrow(anes), round(nrow(anes) * .2))
modo <- lm(fttrump ~ pid7 + rr1 + gender + race + syrians_b + birthyr, anes[-tr,])
summary(modo)
levels(anes[-tr,]$rr1)

model.frame(modo)$race %in% "Middle Eastern"
mododat <- rockchalk::model.data(modo)

## ## Start thinking about PCP
## thresh <- 25
## mod <- modo
## obs <- anes[tr, 'fttrump']
## preds <- predict(modo, newdata = fixbadlevels(anes[tr,], modo))
## PCP <- sum(abs(obs - preds) < thresh, na.rm = T)/length(obs)
## print(PCP)


tste <- besivalm("fttrump", sort(realvarstouse), anes,
                iters = 5, thresh = 1E-5, sampseed = 1, hc = 15, showoutput = FALSE, showforms = FALSE)
tste$intvars
max(tste$pclps)

varls <- lapply(1:25, function(i){
    tst <- besivalm("fttrump", sort(realvarstouse), anes,
                    iters = 5, thresh = 1E-5, sampseed = i, hc = 10, showoutput = FALSE, showforms = FALSE)
    tst$intvars
})
sort(table(unlist(varls)))


lapply(tst$lms, function(x) {
    ifelse(class(x) == "lm", {
        mypreds <- predict(x, newdata = fixbadlevels(anes[tst$tstrows,], x))
        makepclp(x,anes[tst$tstrows, 'fttrump'],mypreds,5)
    },
    NA)
    })

summary( lm(fttrump ~ lcself, anes))

sort(tst$rmses, na.last = F)
bigform <- paste("ftsanders ~", paste(unlist(tst$intvars), collapse = " + "))
summary(lm(bigform, data = anes))
