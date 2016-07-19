## Begin analyzing GSS Data
rm(list = ls())
source("GSSDatFix.R")
source("BeSiVaFunctions.R")


devee <- "vote12bin"
## colnames(dat)


## Keep some data held out
## First, make sure our DV Is included in all cases rows
dat2 <- dat[complete.cases(dat[,devee]),]
## dat2$vote08bin



## get the sample of rows
set.seed(12345)
test <- sample(seq_along(dat2$vote12bin), size = round(nrow(dat2)/10))

## Figure out which rows have few categories, so we can eliminate them later.
ncats <- lapply(colnames(dat2), function(x) length(unique(dat2[-test,x])))



## Keep some columns from being used. Specifically, columns that have
## either 1 or over 100 values, and those that
avoidcols <- c("year", "id", "ballot", "version", "issp", "formwt", "sampcode", "sample", "phase", "spanself", "spanint", "spaneng", "vote12","wtss", "wtssnr", "wtssall", "vrstrat", "vpsu", "pres08", "vote08bin", "vote12bin", "pres12", "if12who","wtcomb",  colnames(dat)[ which(ncats>50)], colnames(dat)[ which(ncats==1)] )


## How can I figure out if a column is now all na's?


whichcols <- lapply(colnames(dat2), function(x) all(is.na(dat2[-test, x]))) == TRUE

allnas <- colnames(dat2)[whichcols]

## Now, how can I figure out if a column is majority nas?
library(caret)
mostlynas <- colnames(dat2)[nearZeroVar(dat2[-test,])]

probvars <- read.csv("/Users/bjr/Dropbox/R_Projects/GSSThing/probkids.csv", stringsAsFactors = F)


dat2[, probvars$x] <- apply(probvars, 1, function(x) rmnewCats(dat2[,x], test))

## dat2[-test, 3]



napercs <- lapply(colnames(dat2), function(x)  sum(is.na(dat2[-test, x]))/nrow(dat2))


varstoinc <-"" ##c("partyid","degree")  ##c("partyid", "degree", "sex", "race")
noVote08 <- "vote08"
avoidcols <- c(avoidcols, allnas, mostlynas, colnames(dat2)[which(napercs>.8)], varstoinc) #, noVote08)


## Keep vote12, and the sample/weight info out of the data
colstouse <- colnames(dat2)[!colnames(dat2) %in% avoidcols]

length(colnames(dat2)) -  length(unique(avoidcols))

length(unique(colstouse))

colstoreallyuse <- colstouse

mods <- besiva(devee, colstoreallyuse, dat2, iters = 5, perc = .1, thresh = 0)

## str(mods)
names(mods)
mods$intvars
mods$tieforms
mods$pcps
## find the columns that are giving us grief
probkids <- colstouse[ which(lapply(mods$predvals, class) == "try-error")]
print(probkids)
## write.csv(probkids, paste0("/Users/bjr/Dropbox/R_Projects/GSSThing/","probkids.csv"), row.names = F, append = T)

## I understand why pres08 and othlang1 might give us trouble. relhhd2, relhh2, and relsp3 aren't so clear


