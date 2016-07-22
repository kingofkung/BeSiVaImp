## Begin analyzing GSS Data
rm(list = ls())
## source("GSSDatFix.R")
dat72kp <- read.csv("/Users/bjr/GitHub/BeSiVaImp/Data/datpres72kp.csv")
source("BeSiVaFunctions.R")
library(caret)


devee <- "voterep"
## colnames(dat)


## Keep some data held out
## First, make sure our DV Is included in all cases rows
dat2 <- dat72kp[complete.cases(dat72kp[,devee]),]
## dat2$vote08bin




## Figure out which rows have few categories, so we can eliminate them later.
ncats <- lapply(colnames(dat2), function(x) length(unique(dat2[,x])))



## Keep some columns from being used. Specifically, columns that have
## either 1 or over 100 values, and those that
avoidcols <- c("year", "id", "ballot", "version", "issp", "formwt", "sampcode", "sample", "phase", "spanself", "spanint", "spaneng", "vote12","wtss", "wtssnr", "wtssall", "vrstrat", "vpsu", "pres08", "pres72","wtcomb",  colnames(dat2)[ which(ncats>50)], colnames(dat2)[ which(ncats==1)], devee )


## How can I figure out if a column is now all na's?


whichcols <- lapply(colnames(dat2), function(x) all(is.na(dat2[, x]))) == TRUE

allnas <- colnames(dat2)[whichcols]

## Now, how can I figure out if a column is majority nas?
mostlynas <- colnames(dat2)[nearZeroVar(dat2[,])]



napercs <- lapply(colnames(dat2), function(x)  sum(is.na(dat2[, x]))/nrow(dat2))


## varstoinc <-"" ##c("partyid","degree")  ##c("partyid", "degree", "sex", "race")
avoidcols <- c(avoidcols, allnas, mostlynas, colnames(dat2)[which(napercs>.8)]) #, noVote08)


## Keep vote12, and the sample/weight info out of the data
colstouse <- colnames(dat2)[!colnames(dat2) %in% avoidcols]

length(unique(colnames(dat2))) -  length(unique(avoidcols))

length(unique(colstouse))

colstoreallyuse <- colstouse

mods <- besiva(devee, colstoreallyuse, dat2, iters = 10, perc = .1, thresh = 0.001)
names(mods)

# W/5 iters, thresh = 0, perc = .1, intvars = "satfrnd" "confed"  "degree"  "satfin"  "partyid"
# W/10 iters, thresh = 0.01, perc = .1, intvars = "satfin"  "partyid"
# W/10 iters, thresh = 0.001, perc = .1, intvars = "family16" "satfrnd"  "confed"   "degree"   "satfin"   "partyid"



mods$intvars
mods$tieforms
mods$pcps



## find the columns that are giving us grief
probkids <- colstouse[ which(lapply(mods$predvals, class) == "try-error")]
print(probkids)
## write.csv(probkids, paste0("/Users/bjr/Dropbox/R_Projects/GSSThing/","probkids.csv"), row.names = F, append = T)

## I understand why pres08 and othlang1 might give us trouble. relhhd2, relhh2, and relsp3 aren't so clear


