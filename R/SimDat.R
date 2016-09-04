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


library(mice)

for(i in 1:10){
## if it's the first iteration, I want dattouse to be what's iterated. Otherwise, I want recursion.
    ifelse(i == 1, dturm <- dattouse, dturm <- dtu1rm)
    dtu <- mice(dturm, m = 1, MaxNWts = 2000)
    ## grab the dataset from dtu
    dtu1 <- complete(dtu, 1)

    ## proof of concept of the replacement with missing data
    ## set.seed(1234)
    ## mrows <- sample(1:nrow(dtu1), round(percMissing[["rincome"]] * nrow(dtu1)), replace = T)
    ## length(mrows)
    ## dtu1[mrows, "rincome"] <- NA

    ## take the names from percent missing.
    dtu1rm <- as.data.frame(lapply(names(percMissing), function(x){
        ## Then, get a sample of the rows of the dataset
        mrows <- sample(1:nrow(dtu1), round(percMissing[[x]] * nrow(dtu1)), replace = T)
        ## Put them as NA
        dtu1[mrows, x] <- NA
        ## and output that column of data as the result of this little lapply function
        dtu1[,x]
    }))
    ## fix the names
    colnames(dtu1rm) <- colnames(dtu1)


}

write.csv(dtu1rm, "../Data/GSS_stata/simdat.csv")

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
