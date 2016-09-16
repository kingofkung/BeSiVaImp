## This is where we'll store our analyses of the ANES data
getwd()
source("/Users/bjr/GitHub/BeSiVaImp/R/OpenANES.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")

writeloc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/"


head(anes2000)

anes2000$bindep <- ifelse(anes2000$vcf0702 %in% "2. Yes, voted", 1, 0)

anes2000$vcf9030a
## Efficacy? Personal efficacy == VCF0609

## make a measure of the overall strength of party ID
pidstr <- as.character(anes2000$vcf0301)
sort(unique(pidstr))
pidstr[grep("Strong", pidstr, ignore.case = T)] <- "3"
pidstr[grep("Weak", pidstr, ignore.case = T)] <- "2"
pidstr[grep("Independent - Independent", pidstr, ignore.case = T)] <- "0"
pidstr[grep("Independent - Democrat|Republican", pidstr, ignore.case = T)] <- "1"
table(anes2000$vcf0301, pidstr)
anes2000$pidstr <- as.numeric(pidstr)
anes2000$pidstr

## Note that regional mobility may not be testable, but we do have where respondent grew up
anes2000$vcf0132
anes2000$vcf9002


varstouse <- c("pid7" = "vcf0301", "daysreadpaper" = "vcf9033", "polEff" =  "vcf0609", "ed" = "vcf0140a", "incGroup" = "vcf0114", "marital" = "vcf0147", "race7" = "vcf0105a", "region" = "vcf0112", "sex" = "vcf0104", "partyContact" = "vcf9030a", "demContact" = "vcf9030b", "repContact" = "vcf9030c", "otherContact" = "vcf9031", "work7" = "vcf0116" , "church" = "vcf0130", "timeinHouse" = "vcf9002", "age" = "vcf0101" )#, "pidstr" = "pidstr")

nucolnames <- colnames(anes2000)
nucolnames[match(varstouse, nucolnames)] <- names(varstouse)
## table(nucolnames, colnames(anes2000))[names(varstouse), varstouse]
colnames(anes2000) <- nucolnames


vtunn <- varstouse
names(vtunn) <- NULL

for(i in 1:100) {
    print(paste0("i = ", i))
    bes2000 <- besiva("bindep", names(varstouse), anes2000, iters = 5, sampseed = i, showoutput = F, showforms = F, thresh = .001)
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

library(ggplot2)

dev.new()
pdf(paste0(writeloc,"ANES2000",i,"runs.pdf"))
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
pdf(paste0(writeloc,"ANES2000",i,"runs pcpHist.pdf"))
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





library(rockchalk)


besforms <- lapply(seq_along(uniqueVar), function(x){
    ivtxt <- paste(uniqueVar[1:x], collapse = " + ")
    wdv <- paste0("bindep ~ ", ivtxt)
    as.formula(wdv)
})


ftex <- formula(bindep ~ daysreadpaper + pid7 + polEff + ed + incGroup + age + timeinHouse + marital + race7 + region + sex)
michigan <- formula(bindep ~ pid7)

besforms <- c(besforms, ftex, michigan)
## Maximum iterations

maxIT <- 100
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

colnames(finalout) <- paste0("iteration", seq_along(besforms))
## Make sure we have teixeira's model somewhere.
ivlist <- lapply(besforms, function(x) as.character(x)[[3]])
teixeiraloc <- ivlist %in% as.character(ftex)[[3]]
michiganloc <- ivlist %in% as.character(michigan)[[3]]

colnames(finalout)[teixeiraloc] <- "teixeira1987ish"
colnames(finalout)[michiganloc] <- "CCMS1960ish"

write.csv(finalout, paste0(writeloc, "pcps.csv"))

finaloutmeans <- apply(finalout, 2, mean)

which(finaloutmeans == max(finaloutmeans))

## Possible to add error bars to each point/ confidence bands on lines?
algIters <- grep("iteration", names(finaloutmeans))
dev.new()
pdf(file = paste(writeloc,"numPts.pdf"))
plot(
    seq_along(finaloutmeans[algIters]),
    finaloutmeans[algIters],
    ylab = "PCPs",
    xlab = "Number of Selected Independent Variables\n Included in the Model",
    type = "p", ylim = c(0.45, 0.75))
lines(x = c(0, seq_along(finaloutmeans)),
      y = rep(finaloutmeans["teixeira1987ish"], 1 + length(finaloutmeans)),
      col = "red")
lines(x = c(0, seq_along(finaloutmeans)),
      y = rep(finaloutmeans["CCMS1960ish"], 1 + length(finaloutmeans)),
      col = "green")
legend("bottomright",
       c("BeSiVa", "Teixeira 1987", "CCMS 1960"),
       lty = c(-1, 1, 1), pch = c(1, -1, -1),
       col = c("black", "red", "green"))
graphics.off()

hist(thepcps)
summarize(thepcps)

## Start working on a latex table featuring the best models
mods <- lapply(besforms, function(x) glm(x, binomial, anes2000))

write.table(outreg(mods[1:6], "latex"), file = paste0("/Users/bjr/GitHub/BeSiVaImp/Output/", "convMods.txt"), row.names = F, col.names = F, quote = FALSE)


## str(bes2000)
