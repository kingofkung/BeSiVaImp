## This is where we'll store our analyses of the ANES data
## getwd()
## source("/Users/bjr/GitHub/BeSiVaImp/R/OpenANES2000.R")
rm(list = ls())
anes2000 <- read.csv("/Users/bjr/GitHub/BeSiVaImp/Data/anes2000.csv")
source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")
source("/Users/bjr/GitHub/BeSiVaImp/R/RecodeANES2000.R")

library(ggplot2)
library(rockchalk)
library(parallel)
no_cores <- detectCores() - 4


writeloc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/"
note <- ""

## head(anes2000)



varstoreallyuse <- c("ednum" = "ednum", "pidstr" = "pidstr", "agesq" = "agesq", "age" = "age", "minority" = "minority", "sex" = "sex", "incNum" = "incNum", "houseTimeNum" = "houseTimeNum", "south" = "south", "divorced" = "divorced", "churchBin" = "churchBin", "daysreadpaper" = "daysreadpaper", "polEff" = "polEff", "partyContact" = "partyContact", "demContact" = "demContact", "repContact" = "repContact", "otherContact" = "otherContact")

cor.test(anes2000$age, anes2000$houseTimeNum, use = "pairwise.complete.obs")
cor.test(anes2000$houseTimeNum, anes2000$incNum, use = "pairwise.complete.obs")
cor.test(anes2000$ednum, anes2000$pidstr, use = "pairwise.complete.obs")

bstst <- besiva("bindep", names(varstoreallyuse), anes2000, showoutput = F, showforms = F)

modmaker(bstst$forms[[1]], anes2000[bstst$tstrows,])

## First effort at parallel programming:
cl <- makeCluster(no_cores, type = "FORK")
MCIter <- 100
clusterExport(cl, c("varstoreallyuse", "MCIter", "anes2000"))
clusterExport(cl, c("findnew", "catprobfinder","modmaker",  "besiva", "getpcp", "predictr"))
pti <- proc.time()
besresults <- parLapply(cl, 1:MCIter, function(i){
    ## print(paste0("MC Progress = ", round(i/MCIter * 100), "%"))
    bes2000 <- besiva("bindep", names(varstoreallyuse), anes2000,
                      iters = 5, sampseed = i,
                      showoutput = F, showforms = F, thresh = .001)
    bes2000}
                        )
ptf <- proc.time() - pti
stopCluster(cl)
##
savvars <- lapply(besresults, function(x) unlist(x$intvars))
savpcp <- unlist(lapply(besresults, function(x) unlist(max(x$intpcps, na.rm = T))))
savvarsU <- unlist(savvars)
savvartab <- sort(table(savvarsU), decreasing = T)



besforms <- lapply(seq_along(savvartab), function(x){
    ivtxt <- paste(names(savvartab)[1:x], collapse = " + ")
    wdv <- paste0("bindep ~ ", ivtxt)
    as.formula(wdv)
})
##
ftex <- formula(bindep ~ daysreadpaper + pidstr + polEff + ednum + incNum + age + houseTimeNum + divorced + minority + south + sex)
michigan <- formula(bindep ~ pid7)
RnH <- formula(bindep ~ polEff + ed + incGroup + partyContact + otherContact + churchBin)
besforms <- c(besforms, ftex, michigan, RnH)

## Maximum iterations
cl <- makeCluster(no_cores)
clusterExport(cl, c("findnew", "catprobfinder","modmaker",  "besiva", "getpcp", "predictr"))
maxIT <- 100
sampsize <- round(nrow(anes2000) * .2)
clusterExport(cl, c("besforms", "maxIT"))
clusterExport(cl, c("anes2000"))
clusterExport(cl, "sampsize")
pti2 <- proc.time()
finalout <- lapply(seq_along(besforms), function(u){
    ##
    print(paste("iteration", u))
    ## A loop designed to replicate the monte Carlo simulations of
    ## BeSiVa, but with a single model instead of many.
    thepcps <- unlist(parLapply(cl, 1:maxIT, function(i, you = u, maxiter = maxIT, sampsz = sampsize, dat = anes2000){
        set.seed(i)
        ## print progress
        ## print(paste0("progress = ", round(i/maxiter * 100), "%" ))
        ## sample the rows
        subsamp <- sample(1:nrow(dat), size = sampsz)
        ## create the model, making sure to pull out some of the data
        ## mod <- speedglm(besforms[[u]], family = binomial(logit), data = droplevels(anes2000[-subsamp,]), fitted = T)
        mod <- glm(besforms[[you]], family = binomial(logit), data = dat[-subsamp,])
        ## get the predictions and the pcps
        ## predsb <- ifelse(predict(mod, newdata = droplevels(anes2000[subsamp,]), type = "response") > .5, 1, 0)
        predsb <- predictr(mod, data = dat, subsamp, loud = F)
        junker <- getpcp(predsb, dat$bindep[subsamp])
        ## save the pcps
        junker
    }))
})
stopCluster(cl)
ptf3 <- proc.time() - pti2
##
finalout <- do.call(cbind, finalout)




length(which(unlist(lapply(savvars, function(x) "age" %in% x))))
length(which(unlist(lapply(savvars, function(x) "age" %in% x | "agesq" %in% x))))
length(which(unlist(lapply(savvars, function(x) "age" %in% x & "agesq" %in% x))))


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
svtabdf$Var <- unlist(lapply(seq_along(datrec[,1]), function(x){
    svtabdf$Var[svtabdf$Var %in% datrec$Var[x]] <- as.character(datrec$Recode[x])
    svtabdf$Var[svtabdf$Var %in% datrec$Recode[x]]
}))

uniqueVar <- unique(svtabdf$Var)
svtabdf$Var <- factor(svtabdf$Var, levels = uniqueVar[length(uniqueVar):1])


dev.new()
pdf(paste0(writeloc, "ANES2000", MCIter, "runs", note, ".pdf"))
ggplot(data = svtabdf) +
    geom_bar(aes(x = Var, stat = "identity"), fill = "darkgreen") +
    theme_classic(10) +
    xlab("Variable Names") + ylab("Count") +
    ggtitle(paste("Number of times BeSiVa Selected a Variable Out of", MCIter, "Runs\n 2000 Election ANES Data")) +
    coord_flip()
graphics.off()


anesh <- hist(savpcp, prob = T)
## anesh$counts <- anesh$counts/sum(anesh$counts)
dev.new()
pdf(paste0(writeloc,"ANES2000", MCIter,"runs pcpHist",note, ".pdf"))
plot(anesh, main = "Histogram of 2000 ANES PCPS")
##
## plot normal distribution
savseq <- seq(min(savpcp), max(savpcp), length.out = length(savpcp))
normedsavseq <- dnorm(x = savseq, mean = mean(savpcp), sd = sd(savpcp))
lines(x = savseq, y = normedsavseq, lty = 1)
##
## Plot kernel density
lines(density(savpcp, na.rm = T), lty = 2)
##
## Add a legend
legend("topright", legend = c("Normal Distribution", "Kernel Density"), lty = c(1, 2) )
graphics.off()




##
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
finaloutdf <- as.data.frame(apply(finalout, 2, summarizeNumerics))
rownames(finaloutdf) <- rownames(summarizeNumerics(finalout[[1]]))
## get bootstrapped confidence intervals
btstpCI <- apply(finalout, 2, quantile, probs = c(.025, .975), na.rm = T) *100
rownames(finaloutdf) <- rownames(summarizeNumerics(finalout[,1]))
modalcat <- prop.table(table(anes2000$bindep)) * 100
##
write.csv(finaloutdf, paste0(writeloc, "pcpsum", "maxIter", maxIT, note, ".csv"))
##
finaloutmeans <- apply(finalout, 2, mean) * 100
##
## Possible to add error bars to each point/ confidence bands on lines?
algIters <- grep("iteration", names(finaloutmeans))
hbar <- 0.05
dev.new()
pdf(file = paste0(writeloc,"maxIter", maxIT,"numPts",note, ".pdf"))
plot(
    x = seq_along(finaloutmeans[algIters]),
    y = finaloutmeans[algIters],
    ylab = "Percent Correctly Predicted",
    xlab = "Number of Selected Independent Variables\n Included in the Model",
    type = "p", ylim = c(0.45, 0.75)*100)
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
