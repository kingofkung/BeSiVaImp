## This is where we'll store our analyses of the ANES data
## getwd()
rm(list = ls())
yr <- 2008
anes92 <- read.csv(paste0("/Users/bjr/GitHub/BeSiVaImp/Data/anes", yr,".csv"))
source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/RecodeANES92.R")

library(ggplot2)
library(rockchalk)
library(parallel)
no_cores <- detectCores() - 4
print(no_cores)

writeloc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/C3/"
note <- paste0("ANES, ", yr, "ForC3")

varstoreallyuse <- c("ednum" = "ednum", "pidstr" = "pidstr", "agesq" = "agesq", "age" = "age", "minority" = "minority", "sex" = "sex", "incNum" = "incNum", "houseTimeNum" = "houseTimeNum", "south" = "south", "divorced" = "divorced", "churchBin" = "churchBin", "daysreadpaper" = "daysreadpaper", "polEff" = "polEff", "partyContact" = "partyContact", "demContact" = "demContact", "repContact" = "repContact", "otherContact" = "otherContact")

varstoreallyuse[!varstoreallyuse %in% colnames(anes92)]

tst <- besiva("bindep", varstoreallyuse, anes92, perc = .25, sampseed = 950, loud = FALSE, showforms = F, showoutput = T)
tst$glms

lapply(tst$glms, function(x){
    ##
    thepreds <- predict(x, newdat = anes92[ tst$tstrows,], type = "response")
    predictr(x, anes92, tst$tstrows)
    ## obs <- anes92[tst$tstrows, "bindep"]
    ## table(thepreds, obs)
##
})


## First effort at parallel programming:
cl <- makeCluster(no_cores, type = "FORK")
MCIter <- 1000
clusterExport(cl, c("varstoreallyuse", "MCIter", "anes92"))
clusterExport(cl, c("fixbadlevels", "getgoodlevels", "bettercpf", "modmaker",  "besiva", "getpcp", "predictr", "predictr2"))
pti <- proc.time()
besresults <- parLapply(cl, 1:MCIter, function(i){
    ## print(paste0("MC Progress = ", round(i/MCIter * 100), "%"))
    bes92 <- besiva("bindep", names(varstoreallyuse), anes92,
                      iters = 5, sampseed = i,
                      showoutput = F, showforms = F, thresh = .001)
    bes92}
                        )
ptf <- proc.time() - pti
stopCluster(cl)
##
savvars <- lapply(besresults, function(x) unlist(x$intvars))
savpcp <- unlist(lapply(besresults, function(x) unlist(max(x$pcps, na.rm = T))))
savvarsU <- unlist(savvars)
savvartab <- sort(table(savvarsU), decreasing = T)



besforms <- lapply(seq_along(savvartab), function(x){
    ivtxt <- paste(names(savvartab)[1:x], collapse = " + ")
    wdv <- paste0("bindep ~ ", ivtxt)
    as.formula(wdv)
})
##
ftex <- formula(bindep ~ daysreadpaper + pidstr + polEff + ednum + incNum + age + houseTimeNum + divorced + minority + south + sex)
michigan <- formula(bindep ~ pidstr)
RnH <- formula(bindep ~ polEff + ednum + incNum + partyContact + otherContact + churchBin)
besforms <- c(besforms, ftex, michigan, RnH)

## Maximum iterations
cl <- makeCluster(no_cores)
clusterExport(cl, c("fixbadlevels", "getgoodlevels", "bettercpf", "modmaker",  "besiva", "getpcp", "predictr", "predictr2"))
maxIT <- 100
sampsize <- round(nrow(anes92) * .2)
clusterExport(cl, c("besforms", "maxIT"))
clusterExport(cl, c("anes92"))
clusterExport(cl, "sampsize")
pti2 <- proc.time()
finalout <- lapply(seq_along(besforms), function(u){
    ##
    print(paste("iteration", u))
    ## A loop designed to replicate the monte Carlo simulations of
    ## BeSiVa, but with a single model instead of many.
    thepcps <- unlist(parLapply(cl, 1:maxIT, function(i, usub = u, maxiter = maxIT, sampsz = sampsize, dat = anes92){
        ## print(usub)
        set.seed(i)
        ## sample the rows
        subsamp <- sample(1:nrow(dat), size = sampsz)
        mod <- glm(besforms[[usub]], family = binomial(logit), data = dat[-subsamp,], model = F, y = F)
        ## get the predictions and the pcps
        predsb <- predictr2(mod, data = dat, subsamp, loud = F)
        junker <- getpcp(predsb, dat$bindep[subsamp])
        ## save the pcps
        junker
    }))
})
stopCluster(cl)
ptf3 <- proc.time() - pti2
##
finalout <- do.call(cbind, finalout)



## Make plot to explain number of variables selected
VarSelect <- table(unlist(lapply(savvars, length)))
## make table and convert it to a dataframe
proptab <- prop.table(VarSelect)*100
proptabdf <- data.frame(t(proptab))[, 2:3]
## plot table using ggplot2. It needs a dataframe.
dev.new()
pdf(paste0(writeloc,"AlgVarNumSelect", MCIter, "runs.pdf"))
ggplot(data = proptabdf, aes(x = Var2, y =  Freq)) +
    geom_bar( stat = "identity") +
    xlab("Number of Variables") + ylab("Percentage of Runs") +
    ggtitle(paste0("Number of Variables Selected by BeSiVa Over ", MCIter, " Runs")) +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.ticks.x = element_blank())
    graphics.off()

svtabdf <- data.frame("Var" = unlist(lapply(seq_along(savvartab), function(x) rep(names(savvartab)[x], savvartab[x]))))
svtabdf$Var <- as.character(svtabdf$Var)
datrec <- read.csv("/Users/bjr/GitHub/BeSiVaImp/Data/uniquevarSels.csv")

for(x in seq_along(datrec[,1])){
    svtabdf$Var[svtabdf$Var %in% datrec$Var[x]] <- as.character(datrec$Recode[x])
    svtabdf$Var[svtabdf$Var %in% datrec$Recode[x]]
}

uniqueVar <- unique(svtabdf$Var)
svtabdf$Var <- factor(svtabdf$Var, levels = uniqueVar[length(uniqueVar):1])


dev.new()
pdf(paste0(writeloc, "ANES2000", MCIter, "runs", note, ".pdf"))
ggplot(data = svtabdf) +
    geom_bar(aes(x = Var), fill = "darkgreen") +
    theme_classic(10) + theme(plot.title = element_text(hjust = .5)) +
    xlab("Variable Names") + ylab("Count") +
    ggtitle(paste("Number of times BeSiVa Selected a Variable Out of", MCIter, "Runs\n ", yr, "Election ANES Data")) +
    coord_flip()
graphics.off()

anpcp <- savpcp*100
anesh <- hist(anpcp, freq = F)
## make stats for normal distribution. This was moved up to allow control over ylim in the plot
savseq <- seq(min(anpcp)- 10, max(anpcp)+ 10, length.out = length(anpcp)*100)
normedsavseq <- dnorm(x = savseq, mean = mean(anpcp), sd = sd(anpcp))
normsavylim <-  c(0, max(normedsavseq) + .00625)
##
##
dev.new()
pdf(paste0(writeloc,"ANES2000", MCIter,"runs pcpHist",note, ".pdf"))
##
##
plot(anesh, main = "Histogram of 2000 ANES PCPS", xlab = "Saved PCPs", freq = F, ylim = normsavylim)
lines(x = savseq, y = normedsavseq, lty = 1)
##
## Plot kernel density
lines(density(anpcp, na.rm = T), lty = 2)
##
## Add a legend
legend("topright", legend = c("Normal Distribution", "Kernel Density"), lty = c(1, 2) )
graphics.off()

summarize(anpcp)


##
##
colnames(finalout) <- paste0("iteration", seq_along(besforms))
## Make sure we have teixeira's model somewhere.
ivlist <- lapply(besforms, function(x) as.character(x)[[3]])
teixeiraloc <- ivlist %in% as.character(ftex)[[3]]
michiganloc <- ivlist %in% as.character(michigan)[[3]]
## Just in case the first one is PID
ivlist[[1]] <- FALSE
RnHloc <- ivlist %in% as.character(RnH)[[3]]
##
colnames(finalout)[teixeiraloc] <- "teixeira1987ish"
colnames(finalout)[michiganloc] <- "CCMS1960ish"
colnames(finalout)[RnHloc] <- "RnH1993ish"
##
finaloutdf <- as.data.frame(apply(finalout, 2, summarizeNumerics))
rownames(finaloutdf) <- rownames(summarizeNumerics(finalout[[1]]))
## get bootstrapped confidence intervals
btstpCI <- apply(finalout, 2, quantile, probs = c(.025, .975), na.rm = T) * 100
## If we want 50% confidence intervals, the line below should give it to us
## btstpCI <- apply(finalout, 2, quantile, probs = c(.25, .75), na.rm = T) * 100

rownames(finaloutdf) <- rownames(summarizeNumerics(finalout[,1]))
modalcat <- prop.table(table(anes92$bindep)) * 100
##
write.csv(finaloutdf, paste0(writeloc, "pcpsum", "maxIter", maxIT, note, ".csv"))
##
finaloutmeans <- apply(finalout, 2, mean) * 100
##
## Possible to add error bars to each point/ confidence bands on lines?
algIters <- grep("iteration", names(finaloutmeans))
hbar <- 0.05
dev.new()
jpeg(file = paste0(writeloc,"maxIter", maxIT,"numPts",note, ".jpeg"), width = 504, height = 504, quality = 100)
plot(
    x = seq_along(finaloutmeans[algIters]),
    y = finaloutmeans[algIters],
    main = paste("Bootstrapped PCPs For Predictors \n BeSiVa Selected Over", maxIT, "iterations"),
    ylab = "Percent Correctly Predicted",
    xlab = "Number of Selected Independent Variables\n Included in the Model",
    type = "p", ylim = c(0.25, 0.75)*100)
lapply(seq_along(algIters), function(x) segments(x, btstpCI[2,x], x, btstpCI[1,x]))
lapply(seq_along(algIters), function(g) segments(x0 = g - hbar, y0 = btstpCI[1,g], x1 = g + hbar , y1 = btstpCI[1,g]))
lapply(seq_along(algIters), function(g) segments(x0 = g - hbar, y0 = btstpCI[2,g], x1 = g + hbar , y1 = btstpCI[2,g]))
abline(h = finaloutmeans["teixeira1987ish"],col = "red")
abline(h = finaloutmeans["CCMS1960ish"],col = "green")
abline(h = finaloutmeans["RnH1993ish"], col = "purple")
abline(h = modalcat, col = "blue")
## Which one is largest?
points(x = which(finaloutmeans == max(finaloutmeans)), max(finaloutmeans), pch = 4, lwd = 3, col = "red")
legend("bottomright",
       c("BeSiVa", "Teixeira 1987", "Campbell et al. 1960", "Rosenstone And Hansen 2003",  "Mode for all"),
       lty = c(-1, 1, 1, 1, 1), pch = c(1, -1, -1, -1, -1),
       col = c("black", "red", "green", "purple", "blue"))
graphics.off()


##

dev.new()
jpeg(file = paste0(writeloc,"maxIter", maxIT," Just theoreticalModsPCPS",note, ".jpg"), width = 504, height = 504, quality = 1000)
par(mar = c(5 - 1.75, 4, 4, 2) + 1.25)
boxplot(finalout[, c("CCMS1960ish", "teixeira1987ish", "RnH1993ish", "iteration4")]*100,
        las = 1,
        xaxt = "n",
        ylab = "Percent Correctly Predicted",
        main = "Percent Correctly Predicted among Models"
        )
axis(1, 1:4,  labels = c("Campbell \net. al 1960", "Teixeira\n 1987",  "Rosenstone\n  and Hansen 2003", "BeSiVa"), tick = FALSE)
abline(h = modalcat[2], col = "blue")
graphics.off()



## Start working on a latex table featuring the best models
mods <- lapply(besforms, function(x) glm(x, binomial, anes92))
names(mods) <- colnames(finalout)
lyxout <- outreg(mods[14:16], "latex", showAIC = T)
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
## gsub("chi", "\\chi", fixline)
##
lyxout[badlineloc] <- fixline
##
##
write.table(lyxout, file = paste0("/Users/bjr/GitHub/BeSiVaImp/Output/", "convMods", note,".txt"), row.names = F, col.names = F, quote = FALSE)

library(xtable)
print.xtable(xtable( t(finaloutdf["mean", ]), digits = 4))
