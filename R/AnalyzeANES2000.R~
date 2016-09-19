## This is where we'll store our analyses of the ANES data
## getwd()
source("/Users/bjr/GitHub/BeSiVaImp/R/OpenANES.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")

library(ggplot2)
library(rockchalk)

writeloc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/"
note <- ""

head(anes2000)

anes2000$bindep <- ifelse(anes2000$vcf0702 %in% "2. Yes, voted", 1, 0)

anes2000$vcf9030a
## Efficacy? Personal efficacy == VCF0609
lastknownages <- anes2000[,grep("vcf0101", colnames(anes2000))]

varstouse <- c("pid7" = "vcf0301", "daysreadpaper" = "vcf9033", "polEff" =  "vcf0609", "ed" = "vcf0140a", "incGroup" = "vcf0114", "marital" = "vcf0147", "race7" = "vcf0105a", "region" = "vcf0112", "sex" = "vcf0104", "partyContact" = "vcf9030a", "demContact" = "vcf9030b", "repContact" = "vcf9030c", "otherContact" = "vcf9031", "work7" = "vcf0116" , "church" = "vcf0130", "union" = "vcf0127", "timeinHouse" = "vcf9002", "age" = "vcf0101", "pidstr" = "pidstr")
nucolnames <- colnames(anes2000)
nucolnames[na.omit(match(varstouse, nucolnames))] <- names(varstouse)[!names(varstouse) %in% "pidstr"]
## table(nucolnames, colnames(anes2000))[names(varstouse), varstouse]

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
## anes2000$pidstr




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
contactVars <- grep("Contact", names(varstouse), value = T)
lapply(contactVars, function(x) sort(unique(anes2000[,x])))

varstoreallyuse <- c("ednum" = "ednum", "pidstr" = "pidstr", "agesq" = "agesq", "age" = "age", "minority" = "minority", "sex" = "sex", "incNum" = "incNum", "houseTimeNum" = "houseTimeNum", "south" = "south", "divorced" = "divorced", "churchBin" = "churchBin", "daysreadpaper" = "daysreadpaper", "polEff" = "polEff", "partyContact" = "partyContact", "demContact" = "demContact", "repContact" = "repContact", "otherContact" = "otherContact")

    bes2000 <- besiva("bindep", names(varstoreallyuse), anes2000, iters = 5, sampseed = 5, showoutput = F, showforms = F, thresh = .001)


for(i in 1:100) {
    print(paste0("i = ", i))
    bes2000 <- besiva("bindep", names(varstoreallyuse), anes2000, iters = 5, sampseed = i, showoutput = F, showforms = F, thresh = .001)
    ifelse(i == 1, savvars <-  bes2000$intvars, savvars <- c(savvars, bes2000$intvars))
    ifelse(i == 1, savpcp <- max(bes2000$intpcps, na.rm = T), savpcp <- c(savpcp, max(bes2000$intpcps, na.rm = T)))
}
 savvarsU <- unlist(savvars)
savvartab <- sort(table(savvarsU), decreasing = T)

table(unlist(lapply(savvars, length)))

## swap useless names for useful ones
## usefulnames <- names(varstouse)[ match(names(savvartab) , varstouse)]

## names(savvartab) <- usefulnames

svtabdf <- data.frame("Var" = unlist(lapply(seq_along(savvartab), function(x) rep(names(savvartab)[x], savvartab[x]))))
uniqueVar <- unique(svtabdf$Var)
svtabdf$Var <- factor(svtabdf$Var, levels = uniqueVar[length(uniqueVar):1])


dev.new()
pdf(paste0(writeloc,"ANES2000",i,"runs",note, ".pdf"))
ggplot(data = svtabdf) +
    geom_bar(aes(x = Var, stat = "identity"), fill = "darkgreen") +
    theme_classic(10) +
    xlab("Variable Names") + ylab("Count") +
    ggtitle(paste("Number of times BeSiVa Selected a Variable Out of", i, "Runs\n 2000 Election ANES Data")) +
    coord_flip()
graphics.off()

str(savpcp)
table(savpcp)

hist(rnorm(100), freq = F)
hist(savpcp, freq = F)

anesh <- hist(savpcp, prob = T)
## anesh$counts <- anesh$counts/sum(anesh$counts)
dev.new()
pdf(paste0(writeloc,"ANES2000",i,"runs pcpHist",note, ".pdf"))
plot(anesh, main = "Histogram of 2000 ANES PCPS")
##
##
## plot normal distribution
savseq <- seq(min(savpcp), max(savpcp), length.out = length(savpcp))
normedsavseq <- dnorm(x = savseq, mean = mean(savpcp), sd = sd(savpcp))
lines(x = savseq, y = normedsavseq, lty = 1)
##
##
## Plot kernel density
lines(density(savpcp, na.rm = T), lty = 2)
##
##
## Add a legend
legend("topright", legend = c("Normal Distribution", "Kernel Density"), lty = c(1, 2) )
graphics.off()







besforms <- lapply(seq_along(uniqueVar), function(x){
    ivtxt <- paste(uniqueVar[1:x], collapse = " + ")
    wdv <- paste0("bindep ~ ", ivtxt)
    as.formula(wdv)
})


ftex <- formula(bindep ~ daysreadpaper + pidstr + polEff + ednum + incNum + age + houseTimeNum + divorced + minority + south + sex)
michigan <- formula(bindep ~ pid7)
RnH <- formula(bindep ~ polEff + ed + incGroup + partyContact + otherContact + churchBin)
##Rosenstone and Hansen 1993
model.frame(glm(RnH, data = anes2000, family = binomial()))

besforms <- c(besforms, ftex, michigan, RnH)
## Maximum iterations

maxIT <- 1000
sampsize <- round(nrow(anes2000) * .2)
set.seed(10101)
for(u in seq_along(besforms)){
    ##
    print(paste("iteration", u))
    ## A loop designed to replicate the monte Carlo simulations of
    ## BeSiVa, but with a single model instead of many.
    thepcps <- unlist(lapply(1:maxIT, function(i, maxiter = maxIT){
        ## print progress
        ## print(paste0("progress = ", round(i/maxiter * 100), "%" ))
        ## sample the rows
        subsamp <- sample(1:nrow(anes2000), size = sampsize )
        ## create the model, making sure to pull out some of the data
        mod <- glm(besforms[[u]], family = binomial, data = anes2000[-subsamp,])
        ## get the predictions and the pcps
        predsb <- predictr(mod, anes2000, subsamp, loud = F)
        junker <- getpcp(predsb, anes2000$bindep[subsamp])
        ## save the pcps
        junker
    }))
    ifelse(u == 1, finalout <- data.frame(thepcps), finalout <- cbind(finalout, thepcps))
    }
##
colnames(finalout) <- paste0("iteration", seq_along(besforms))
## Make sure we have teixeira's model somewhere.
ivlist <- lapply(besforms, function(x) as.character(x)[[3]])
teixeiraloc <- ivlist %in% as.character(ftex)[[3]]
michiganloc <- ivlist %in% as.character(michigan)[[3]]
RnHloc <- ivlist %in% as.character(RnH)[[3]]
##
colnames(finalout)[teixeiraloc] <- "teixeira1987ish"
colnames(finalout)[michiganloc] <- "CCMS1960ish"
colnames(finalout)[RnHloc] <- "RnH1993ish"
##
finaloutdf <- as.data.frame(sapply(finalout, summarizeNumerics))
## get bootstrapped confidence intervals
btstpCI <- sapply(finalout, quantile, probs = c(.025, .975), na.rm = T)
rownames(finaloutdf) <- rownames(summarizeNumerics(finalout[,1]))
##
write.csv(finaloutdf, paste0(writeloc, "pcpsum", "maxIter", maxIT, note, ".csv"))
##
finaloutmeans <- apply(finalout, 2, mean)
##
##
## Possible to add error bars to each point/ confidence bands on lines?
algIters <- grep("iteration", names(finaloutmeans))
dev.new()
pdf(file = paste0(writeloc,"maxIter", maxIT,"numPts",note, ".pdf"))
plot(
    seq_along(finaloutmeans[algIters]),
    finaloutmeans[algIters],
    ylab = "PCPs",
    xlab = "Number of Selected Independent Variables\n Included in the Model",
    type = "p", ylim = c(0.45, 0.75))
lapply(seq_along(algIters), function(x) segments(x, btstpCI[2,x], x, btstpCI[1,x]))
lapply(seq_along(algIters), function(g) segments(x0 = g - hbar, y0 = btstpCI[1,g], x1 = g + hbar , y1 = btstpCI[1,g]))
lapply(seq_along(algIters), function(g) segments(x0 = g - hbar, y0 = btstpCI[2,g], x1 = g + hbar , y1 = btstpCI[2,g]))
abline(h = finaloutmeans["teixeira1987ish"],col = "red")
abline(h = finaloutmeans["CCMS1960ish"],col = "green")
abline(h = finaloutmeans["RnH1993ish"], col = "purple")
abline(h = prop.table(table(anes2000$bindep)), col = "blue")
## Which one is largest?
points(x = which(finaloutmeans == max(finaloutmeans)), max(finaloutmeans), pch = 4, lwd = 3, col = "red")
legend("bottomright",
       c("BeSiVa", "Teixeira 1987", "Party ID", "RnH",  "Mode for all"),
       lty = c(-1, 1, 1, 1, 1), pch = c(1, -1, -1, -1, -1),
       col = c("black", "red", "green", "purple", "blue"))
graphics.off()


## Start working on a latex table featuring the best models
mods <- lapply(besforms, function(x) glm(x, binomial, anes2000))
lyxout <- outreg(mods[1:14], "latex", showAIC = T)
## But look, there's a line with way too many *'s, and -2LLR twice, right here.
badlineloc <- grep("[*]{5}", lyxout, T)
badline <- lyxout[badlineloc]
## Get rid of excessive asterisks.
badline <- gsub("[*]+", "", badline)
## deal with that pesky second -2LLR
neg2LLR <- "[$]-2LLR\\s[(]Model\\schi[:^:]\\d[:):][:$:]"
start2LLR2 <- gregexpr(neg2LLR, badline)[[1]][2]
## Turns out we can't just use the length of the above thing, as it's
## got a bunch of extra characters we don't need. So we'll use the
## length of the thing that's bugging us.
n2llrlen <- nchar("$-2LLR (Model chi^2)$")
## And then we cut it out like so
fixline <- paste(substr(badline, 1, start2LLR2-1), substr(badline, start2LLR2 + n2llrlen , nchar(badline))   )
## Possible to get it so we actually have the symbol chi^2, instead of
## what we do have?
## Think I'll talk with PJ.
gsub("chi", "\\chi", fixline)
##
lyxout[badlineloc] <- fixline
##
##
write.table(lyxout, file = paste0("/Users/bjr/GitHub/BeSiVaImp/Output/", "convMods", note,".txt"), row.names = F, col.names = F, quote = FALSE)


## str(bes2000)
