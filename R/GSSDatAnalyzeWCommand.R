## Begin analyzing GSS Data
rm(list = ls()[!ls() %in% "dat2"])
## source("GSSDatFix.R")
datloc <- "/Users/bjr/GitHub/BeSiVaImp/Data/GSS_stata/"
if(!exists("dat2")) dat2 <- read.csv(paste0(datloc, "vote12bindat.csv"))
source("BeSiVaFunctions.R")


devee <- "vote12bin"
## colnames(dat)

percdat <- prop.table(table(dat2$vote12)) * 100

## Keep some data held out
## First, make sure our DV Is included in all cases rows
## dat2 <- dat[complete.cases(dat[,devee]),]
## dat2$vote08bin



## get the sample of rows
set.seed(12345)
test <- sample(seq_along(dat2$vote12bin), size = round(nrow(dat2)/10))

## Figure out which rows have few categories, so we can eliminate them later.
ncats <- lapply(colnames(dat2), function(x) length(unique(dat2[,x])))



## Keep some columns from being used. Specifically, columns that have
## either 1 or over 100 values, and those that
avoidcols <- c("year", "id", "ballot", "version", "issp", "formwt", "sampcode", "sample", "phase", "spanself", "spanint", "spaneng", "vote12","wtss", "wtssnr", "wtssall", "vrstrat", "vpsu", "pres08", "vote08bin", "vote12bin", "pres12", "if12who","wtcomb",  colnames(dat2)[ which(ncats>50)], colnames(dat2)[ which(ncats==1)] )


## How can I figure out if a column is now all na's?


whichcols <- lapply(colnames(dat2), function(x) all(is.na(dat2[, x]))) == TRUE

allnas <- colnames(dat2)[whichcols]

## Now, how can I figure out if a column is majority nas?
library(caret)
mostlynas <- colnames(dat2)[nearZeroVar(dat2[,])]

## probvars <- read.csv("/Users/bjr/Dropbox/R_Projects/GSSThing/probkids.csv", stringsAsFactors = F)



## dat2[-test, 3]



napercs <- lapply(colnames(dat2), function(x)  sum(is.na(dat2[-test, x]))/nrow(dat2))


varstoinc <-"" ##c("partyid","degree")  ##c("partyid", "degree", "sex", "race")
noVote08 <- "vote08"
avoidcols <- c(avoidcols, allnas, mostlynas, colnames(dat2)[which(napercs>.8)], varstoinc, noVote08)


## Keep vote12, and the sample/weight info out of the data
colstouse <- colnames(dat2)[!colnames(dat2) %in% avoidcols]

length(colnames(dat2)) -  length(unique(avoidcols))

length(unique(colstouse))

colstouse <- sort(colstouse)
write.csv(colstouse, "/Users/bjr/GitHub/BeSiVaImp/Output/ColsC1ForAppendixA.csv", row.names = F)

mods <- besiva(devee, colstouse, dat2, iters = 5, perc = .1, thresh = .001)
max(mods$pcps)

modColl <- lapply(1:100, function(x){
    print(paste("iter =", x))
    junker <- besiva(devee, colstouse, dat2, perc = .1, thresh = .001, sampseed = x, showforms = F)
    junker
})
mcPCPs <- unlist(lapply(modColl, function(x) max(x$pcps, na.rm = T)))
write.csv(mcPCPs, "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/C1/C1PCPs.csv")
mcIntVars <- unlist(lapply(modColl, function(x) unlist(x$intvars)))
write.csv(mcIntVars, "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/C1/C1IntVars.csv")

## tstmod <- glm(vote12bin ~ sector, data = dat2[-test,], family = binomial)
## tstcpf <- catprobfinder(tstmod, dat2, test)
## unique(model.frame(tstmod)$sector)
## unique(dat2[test, 'sector'])
## unique(tstcpf$tstdatnu)

## predictr(tstmod, data = dat2, rowstouse = mods$tstrows)

data.frame(as.character(mods$forms), mods$pcps)[order(mods$pcps),]

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


