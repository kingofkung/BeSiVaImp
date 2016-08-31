## Attempt to simulate the data using the method recommended by Paul Johnson

dat <- read.csv("../Data/GSS_stata/rmMissingsVotebin12.csv")


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

varsofint <- c("educ", "degree", "rincome", "wealth", "poleff11", "poleff18", "poleff19", "poleff20", "polinter", "news", "newsfrom", "polnews", "partyid", "grpparty", "mobile16", "marital", "racecen1","race", "region", "sex", "age" )

dattouse <- dat[, c(varsofint, "vote12bin")]

percMissing <- lapply(dattouse, function(x) sum(is.na(x))/nrow(dat))


## So let's say I wanted to make a dv from these variables
set.seed(1234)
constDVlat <- 1 + .1 * dattouse$age + dummies::dummy(dattouse$sex)[, 1]
pr <- 1/(1 + exp(-constDVlat))
hist(round(pr, 2))
dattouse$constDV <- rbinom(nrow(dattouse), 1, pr)
summary(glm(constDV ~ age + sex, family = binomial, dattouse))



