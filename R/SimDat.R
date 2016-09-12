## Attempt to simulate the data using the method recommended by Paul Johnson

## dat <- read.csv("../Data/GSS_stata/rmMissingsVotebin12.csv")
dat <- read.csv("../Data/GSS_stata/simdat.csv")

## pretty obvious that if we try to use all columns, it's going to take forever. Let's get a selection that's theoretically driven.
## Education, educ and degree
## Wealth: rincome, wealth
## Efficacy? poleff11, poleff18, poleff19, poleff20, polinter
## Contact? ## remains to be seen

## Texeira 1987

## Links to media: news, newsfrom, polnews
## links to party: partyid, grpparty
## links to state: (See Efficacy)
## SES: See education and wealth
## residential mobility: mobile16
## marital status: marital
## race: racecen1, race
## region: region
## sex: sex

grep("bin", colnames(dat), value = T)




colnames(dturm)[sample(1:ncol(dturm), 5)]

## So let's say I wanted to make a dv from these variables
lapply(dtu1rm, unique)

library(dummies)
set.seed(1234)
## constDVlat <- -100 + 1 * dtu1rm$age + 2.5 * dtu1rm$educ

dummy("racecen1", dtu1rm)
head(dummy("poleff11", data = dtu1rm))
head(dummy("poleff19", data = dtu1rm))
head(dummy("marital", data = dtu1rm))


somecats <- grep("^married|divorced", levels(dtu1rm$marital))
head(dummy("marital", data = dtu1rm)[,somecats])

morecats <- grep("strongly\\sagree|^agree", levels(dtu1rm$poleff11))
rowSums(dummy("poleff11", data = dtu1rm)[,morecats])


lapply(dtu1rm, function(x){
    if(is.factor(x)==T){
        somecols <- sample(1:length(levels(x)), size = 3, replace = T)
        if(length(unique(somecols)) > 1) rowSums(dummy(x)[, unique(somecols)])
    }
})

constDVlat <-

pr <- 1/(1 + exp(-constDVlat))
plot(sort(pr))
table(round(pr, 1))
dtu1rm$constDV <- rbinom(nrow(dtu1rm), 1, pr)
table(dattouse$constDV)
summary(glm(constDV ~ age + educ, family = binomial, dtu1rm))



source("BeSiVaFunctions.R")

for(i in 1:100){
    g <- besiva("constDV", varsofint, dattouse, sampseed = i, showoutput = F, thresh = .025)
    ifelse(i == 1, savr <- g$intvars, savr <- c(savr, g$intvars))
}
table(unlist(savr))




savrpaste <- lapply(savr, function(x) paste(sort(x), collapse = ""))
sum(savrpaste %in% "ageeduc")

table(unlist(truevars), unlist(savrpaste))
