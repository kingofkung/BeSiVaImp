## Open and clean all neccessities for using the GSS Data
rm(list = ls())

library(foreign)
datloc <- "/Users/bjr/GitHub/BeSiVaImp/Data/GSS_stata/"
## dat <- read.dta("/Users/bjr/Dropbox/R_Projects/GSSThing/GSS2014merged_R3.DTA")
## ## dat <- read.dta("/Users/bjr/GitHub/BeSiVaImp/Data/GSS2006.dta")
## dat <- read.dta(paste0(datloc, "GSS7214_R4.DTA"))
## write.csv(dat, file = paste0(datloc, "GSS7214_R5.csv"))

library(data.table)
dat <- fread(paste0(datloc, "GSS7214_R5.csv"))
dat <- as.data.frame(dat)
## varlabs <- attr(dat, "var.labels")
## ncol(dat)
## head(dat)

grep("Vote", colnames(dat), ignore.case = TRUE, value = T)

unique(dat$vote12)
dat$vote12bin <- as.character(dat$vote12)
unique(dat$vote12bin)

dat$vote12bin[dat$vote12bin %in% "did not vote"] <- "0"
dat$vote12bin[dat$vote12bin %in% "voted"] <- "1"
dat$vote12bin[dat$vote12bin %in% "ineligible"] <- NA

dat$vote12bin <- as.numeric(dat$vote12bin)

table(dat$vote12, dat$vote12bin)

## Reconsider how we make our data
## Get complete.cases, as before
dvote12 <- dat[complete.cases(dat$vote12bin), ]
## but also, find which columns are NOT all na's...
somedatahere <- unlist(lapply(dvote12, function(x) !all(is.na(x))))
## ...and remove them from consideration.
dvote12 <- dvote12[, somedatahere]

## That still leaves us with a lot of data, and allows us to write a
## file that's smaller than before.
dim(dvote12)
write.csv(dvote12, paste0(datloc, "rmMissingsVotebin12.csv"), row.names = F)


unique(dat$occ10)

## Recode trtcops
## apply(dat, 2, function(x) grep("cop",x))
colnames(dat)[grep("cop",colnames(dat))]


## recode dat$vote08
dat$vote08

dat$vote08bin <- as.character(dat$vote08)
unique(as.character(dat$vote08))
dat$vote08bin[dat$vote08bin %in% "did not vote"] <- "0"
dat$vote08bin[dat$vote08bin %in% "voted"] <- "1"
dat$vote08bin[dat$vote08bin %in% "ineligible"] <- NA

dat$vote08bin <- as.numeric(dat$vote08bin)
## table(dat$vote08, dat$vote08bin, useNA= "ifany")


grep("pres", colnames(dat), value = T)
dat72 <- dat[!is.na( dat$pres72), ]

notallnas72 <- unlist(lapply(dat72, function(x) !all(is.na(x))))

keepcols <- colnames(dat72)[notallnas72]
dat72kp <- dat72[ , colnames(dat72) %in% keepcols]
dim(dat72kp)

dat72kp[dat72kp$pres72 %in% "", "pres72"] <- NA
dat72kp$voterep <- ifelse(dat72kp$pres72 == "nixon", 1, 0)
dat72kp$voterep[!dat72$pres72 %in% c("mcgovern", "nixon")] <- NA
table(dat72kp$voterep, dat72kp$pres72)

 ## write.csv(dat72kp, paste0(datloc, "datpres72kp.csv"))


dat76 <- dat[!is.na(dat$pres76) & !dat$pres76 %in% "", ]
notallnas76 <- unlist(lapply(dat76, function(x) !all(is.na(x))))
dat76kp <- dat76[ , notallnas76]
dim(dat76kp)

dat76kp$voterep <- ifelse(dat76kp$pres76 == "ford", 1, 0)
dat76kp$voterep[ dat76kp$pres76 %in% c("no pres. vote", "other", "refused")] <- NA

table(dat76kp$voterep, dat76kp$pres76)
## write.csv(dat76kp, paste0(datloc, "datpres76kp.csv"))
