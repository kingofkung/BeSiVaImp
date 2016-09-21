## Move the recodes to another file, to make everything slightly more managable

anes2000$bindep <- ifelse(anes2000$vcf0702 %in% "2. Yes, voted", 1, 0)


varstochange <- c("pid7" = "vcf0301", "daysreadpaper" = "vcf9033", "polEff" =  "vcf0609", "ed" = "vcf0140a", "incGroup" = "vcf0114", "marital" = "vcf0147", "race7" = "vcf0105a", "region" = "vcf0112", "sex" = "vcf0104", "partyContact" = "vcf9030a", "demContact" = "vcf9030b", "repContact" = "vcf9030c", "otherContact" = "vcf9031", "work7" = "vcf0116" , "church" = "vcf0130", "union" = "vcf0127", "timeinHouse" = "vcf9002", "age" = "vcf0101", "pidstr" = "pidstr")
nucolnames <- colnames(anes2000)
nucolnames[na.omit(match(varstochange, nucolnames))] <- names(varstochange)[!names(varstochange) %in% "pidstr"]

colnames(anes2000) <- nucolnames



grep("partyContact", colnames(anes2000))

## make a measure of the overall strength of party ID
pidstr <- as.character(anes2000$pid7)
sort(unique(pidstr))
pidstr[grep("Strong", pidstr, ignore.case = T)] <- "3"
pidstr[grep("Weak", pidstr, ignore.case = T)] <- "2"
pidstr[grep("Independent - Independent", pidstr, ignore.case = T)] <- "0"
pidstr[grep("Independent - Democrat|Republican", pidstr, ignore.case = T)] <- "1"
table(anes2000$pid7, pidstr)
anes2000$pidstr <- as.numeric(pidstr)


## Create numeric education variable.
anes2000$ednum <- as.character(anes2000$ed)
anes2000$ednum <- substr(anes2000$ednum, 1, 1)
anes2000$ednum <- as.numeric(anes2000$ednum)
## table(anes2000$ed, anes2000$ednum)

## Craft age squared variable
anes2000$agesq <- anes2000$age**2
## plot(anes2000$age, anes2000$agesq)

## Get racial data together
table(anes2000$race7)
anes2000$minority <- as.character(anes2000$race7)
anes2000$minority[anes2000$race7 %in% "1. White non-Hispanic (1948-2012)" ] <- "0"
anes2000$minority[!anes2000$race7 %in% "1. White non-Hispanic (1948-2012)" & !is.na(anes2000$race7) ] <- "1"
anes2000$minority <- as.numeric(anes2000$minority)
table(anes2000$race7, anes2000$minority, useNA = "always")

## numeric income
anes2000$incNum <- substr(as.character(anes2000$incGroup), 1, 1)
anes2000$incNum <- as.numeric(anes2000$incNum)

## house time

anes2000$houseTimeNum <- substr(anes2000$timeinHouse, 1, 1)
anes2000$houseTimeNum <- as.numeric(anes2000$houseTimeNum)
table(anes2000$houseTimeNum, anes2000$timeinHouse)

## region dichotomize
anes2000$south <- as.character(anes2000$region)
anes2000$south[grepl("South", anes2000$region)] <- 1
anes2000$south[!grepl("South", anes2000$region)] <- 0
table(anes2000$south, anes2000$region)

## divorced dichotomous
anes2000$divorced <- as.character(anes2000$marital)
anes2000$divorced[grepl("Divorced", anes2000$marital)] <- 1
anes2000$divorced[!grepl("Divorced", anes2000$marital) & !is.na(anes2000$marital)] <- 0
anes2000$divorced <- as.numeric(anes2000$divorced)
table(anes2000$divorced, anes2000$marital)

## married dichotomous
anes2000$married <- as.character(anes2000$marital)
anes2000$married[grepl("Married", anes2000$marital)] <- "1"
anes2000$married[!grepl("Married", anes2000$marital) & !is.na(anes2000$marital)] <- "0"
anes2000$married <- as.numeric(anes2000$married)
table(anes2000$married, anes2000$marital)


## Goto church dichotomous
sort(unique(anes2000$church))
anes2000$churchBin <- as.character(anes2000$church)
anes2000$churchBin[grepl("Never", anes2000$church)] <- 0
anes2000$churchBin[!grepl("Never", anes2000$church) & !is.na(anes2000$church)] <- 1
table(anes2000$church, anes2000$churchBin, useNA = "always")

## dichotomize Party and other Contact (all)
contactVars <- grep("Contact", names(varstochange), value = T)
lapply(contactVars, function(x) sort(unique(anes2000[,x])))
## Looks like we're already there. NVM.
