## Move the recodes to another file, to make everything slightly more managable


anes92$bindep <- ifelse(anes92$vcf0702 %in% "2. Yes, voted", 1, 0)
anes92$bindep[is.na(anes92$vcf0702)] <- NA

varstochange <- c("pid7" = "vcf0301", "daysreadpaper" = "vcf9033", "polEff" =  "vcf0609", "ed" = "vcf0140a", "incGroup" = "vcf0114", "marital" = "vcf0147", "race7" = "vcf0105a", "region" = "vcf0112", "sex" = "vcf0104", "partyContact" = "vcf9030a", "demContact" = "vcf9030b", "repContact" = "vcf9030c", "otherContact" = "vcf9031", "work7" = "vcf0116" , "church" = "vcf0130", "union" = "vcf0127", "timeinHouse" = "vcf9002", "age" = "vcf0101", "pidstr" = "pidstr")
vtc <- as.data.frame(varstochange)
rownames(vtc)

nucolnames <- colnames(anes92)

for(i in 1:nrow(vtc)) nucolnames[colnames(anes92) %in% vtc[i,1]] <- rownames(vtc)[i]

checkdf <- data.frame("orig" = colnames(anes92), "new" = nucolnames)
checkdf <- checkdf[!grepl("vcf", checkdf$new)& !checkdf$new %in% c("version", "bindep"),]
checkdf[order(checkdf$new),]
sort(rownames(vtc))

colnames(anes92) <- nucolnames



grep("partyContact", colnames(anes92))

## make a measure of the overall strength of party ID
pidstr <- as.character(anes92$pid7)
sort(unique(pidstr))
pidstr[grep("Strong", pidstr, ignore.case = T)] <- "3"
pidstr[grep("Weak", pidstr, ignore.case = T)] <- "2"
pidstr[grep("Independent - Independent", pidstr, ignore.case = T)] <- "0"
pidstr[grep("Independent - Democrat|Republican", pidstr, ignore.case = T)] <- "1"
table(anes92$pid7, pidstr)
anes92$pidstr <- as.numeric(pidstr)


## Create numeric education variable.
anes92$ednum <- as.character(anes92$ed)
anes92$ednum <- substr(anes92$ednum, 1, 1)
anes92$ednum <- as.numeric(anes92$ednum)
## table(anes92$ed, anes92$ednum)

## Craft age squared variable
anes92$agesq <- anes92$age**2
## plot(anes92$age, anes92$agesq)

## Get racial data together
table(anes92$race7)
anes92$minority <- as.character(anes92$race7)
anes92$minority[anes92$race7 %in% "1. White non-Hispanic (1948-2012)" ] <- "0"
anes92$minority[!anes92$race7 %in% "1. White non-Hispanic (1948-2012)" & !is.na(anes92$race7) ] <- "1"
anes92$minority <- as.numeric(anes92$minority)
table(anes92$race7, anes92$minority, useNA = "always")

## numeric income
anes92$incNum <- substr(as.character(anes92$incGroup), 1, 1)
anes92$incNum <- as.numeric(anes92$incNum)

## house time

anes92$houseTimeNum <- substr(anes92$timeinHouse, 1, 1)
anes92$houseTimeNum <- as.numeric(anes92$houseTimeNum)
table(anes92$houseTimeNum, anes92$timeinHouse)

## region dichotomize
anes92$south <- as.character(anes92$region)
anes92$south[grepl("South", anes92$region)] <- 1
anes92$south[!grepl("South", anes92$region)] <- 0
table(anes92$south, anes92$region)

## divorced dichotomous
anes92$divorced <- as.character(anes92$marital)
anes92$divorced[grepl("Divorced", anes92$marital)] <- 1
anes92$divorced[!grepl("Divorced", anes92$marital) & !is.na(anes92$marital)] <- 0
anes92$divorced <- as.numeric(anes92$divorced)
table(anes92$divorced, anes92$marital)

## married dichotomous
anes92$married <- as.character(anes92$marital)
anes92$married[grepl("Married", anes92$marital)] <- "1"
anes92$married[!grepl("Married", anes92$marital) & !is.na(anes92$marital)] <- "0"
anes92$married <- as.numeric(anes92$married)
table(anes92$married, anes92$marital)


## Goto church dichotomous
sort(unique(anes92$church))
anes92$churchBin <- as.character(anes92$church)
anes92$churchBin[grepl("Never", anes92$church)] <- 0
anes92$churchBin[!grepl("Never", anes92$church) & !is.na(anes92$church)] <- 1
table(anes92$church, anes92$churchBin, useNA = "always")

## dichotomize Party and other Contact (all)
contactVars <- grep("Contact", names(varstochange), value = T)
lapply(contactVars, function(x) sort(unique(anes92[,x])))
## Looks like we're already there. NVM.

##looks like poleff wasn't properly recoded. Let's fix that.
effrec <- as.character(anes92[, grep("Eff", colnames(anes92))])
## View(sort(unique(effrec)))
effrec[grep("Agree", effrec)] <- 2
effrec[grep("Disagree", effrec)] <- 0
effrec[grep("Neither", effrec)] <- 1
effrec[grep("DK", effrec)] <- NA
anes92$polEff <- as.character(anes92$polEff)
anes92$polEff <- as.numeric(effrec)
