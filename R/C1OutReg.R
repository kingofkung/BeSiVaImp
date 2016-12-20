library(rockchalk)
library(ggplot2)


datloc <- "/Users/bjr/GitHub/BeSiVaImp/Data/GSS_stata/"
if(!exists("dat2")) dat2 <- read.csv(paste0(datloc, "vote12bindat.csv"))


dat2$vote08nu <- factor(dat2$vote08, levels(dat2$vote08)[c(3, 2, 1)])
## table(dat2$vote08, dat2$vote08nu)
dat2$partyidnu <- factor(dat2$partyid, levels(dat2$partyid)[c(8, 5, 2, 3, 1, 4, 7, 6)])
## table(dat2$partyid, dat2$partyidnu)

varLabs <- c(vote08nuineligible = "Ineligible to Vote in 08",
             "vote08nudid not vote" = "Did Not Vote in 08",
             educ = "Education (Years)",
             "degreegraduate" = "Graduate Degree",
             "degreehigh school" = "High School Diploma",
             "degreejunior college" = "Junior College Degree",
             "degreelt high school" = "Less than High School",
             "partyidnunot str republican" = "Weak Republican",
             "partyidnuind,near rep" = "Lean Republican",
             "partyidnuindependent" = "Independent",
             "partyidnuind,near dem" = "Lean Democrat",
             "partyidnunot str democrat" = "Weak Democrat",
             "partyidnustrong democrat" = "Strong Democrat",
             "partyidnuother party" = "Other Party")

m1 <- glm(vote12bin ~ vote08nu + educ , family = binomial, data = dat2)
m2 <- glm(vote12bin ~ degree + partyidnu, family = binomial, data = dat2)
rockchalk::outreg(list("First Attempt" = m1, "Removed Prior Vote" =  m2), varLabels = varLabs, showAIC = T)


dbLoc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/C1/"
vote08Vars <- read.csv(paste0(dbLoc, "C1IntVarsVote08.csv"), stringsAsFactors = FALSE)[, 2]
## Having read in the data, we sort it so that ggplot2 knows what
## order we'd like the data in.
VarTab <- sort(table(vote08Vars), decreasing = FALSE)
vote08VarFac <- factor(vote08Vars, levels = names(VarTab))
## The data.frame makes it so ggplot2 can use it
vote08VarFac <- data.frame("Var" = vote08VarFac)
## Remove any only appearing one time.
votegt2 <- names(which(table(vote08VarFac) > 1))
vote08VarFac <- data.frame("Var" = vote08VarFac[as.character(vote08VarFac$Var) %in% votegt2, ])


NoEducVars <- read.csv(paste0(dbLoc, "C1IntVarsNoEduc.csv"), stringsAsFactors = FALSE)[, 2]
## repeat with the sorting
NoEducTab <- sort(table(NoEducVars), decreasing = FALSE)
NoEducVarFac <- factor(NoEducVars, levels = names(NoEducTab))
NoEducVarFac <- data.frame("Var" = NoEducVarFac)
## Remove the one again
educgt2 <- names(which(table(NoEducVarFac) > 1))
NoEducVarFac <- data.frame("Var" = NoEducVarFac[as.character(NoEducVarFac$Var) %in% educgt2,])







VarPlotFunc <- function(dat, title){
    ggplot(data = dat) +
        geom_bar(aes(x = Var, stat = "identity"), fill = "darkgreen") +
        theme_classic(10) +
        xlab("Variable Names") + ylab("Count") +
        ggtitle(title) +
        coord_flip()
}

dev.new()
pdf(paste0(dbLoc, "GSS", 100, "C1.pdf"))
fullTitle <- paste("Number of times BeSiVa Selected a Variable Out of", 100, "Runs\n 2014 GSS Data")
VarPlotFunc(vote08VarFac, title = fullTitle)
graphics.off()

dev.new()
pdf(paste0(dbLoc, "GSS", 100, "C1NoEduc.pdf"))
fullTitleNE <- paste("Number of times BeSiVa Selected a Variable Out of", 100, "Runs\n 2014 GSS Data Without Education")
VarPlotFunc(NoEducVarFac, title = fullTitleNE)
graphics.off()





vote08PCPs <- read.csv(paste0(dbLoc, "C1PCPsVote08.csv"), stringsAsFactors = FALSE)[, 2]
dev.new()
pdf(paste0(dbLoc, "vote08HistGSS.pdf"))
hist(vote08PCPs*100,
     main = "A Histogram of PCPs for the GSS\n When Vote is Included",
     xlab = "Percent Correctly Predicted")
graphics.off()
mainSumStats <- round(summarize(vote08PCPs * 100)$numerics, 2)
## In order to change their column names in the dataframe, we must
## erase their original names.
colnames(mainSumStats) = NULL

## NoVote08PCPs <- read.csv(paste0(dbLoc, "C1PCPs.csv"), stringsAsFactors = FALSE)[, 2]
## hist(NoVote08PCPs*100)
## round(summarize(NoVote08PCPs * 100)$numerics, 2)

dev.new()
pdf(paste0(dbLoc, "NoEducHistGSS.pdf"))
NoEducPCPS <- read.csv(paste0(dbLoc, "C1PCPsNoEduc.csv"), stringsAsFactors = FALSE)[, 2]
hist(NoEducPCPS*100,
     main = "A Histogram of PCPs for the GSS\n Without Education Variables",
     xlab = "Percent Correctly Predicted")
graphics.off()

noEducSumStats <- round(summarize(NoEducPCPS * 100)$numerics, 2)
colnames(noEducSumStats) <- NULL

SumStatOut <- data.frame("All Variables" = mainSumStats, "Removed Education" = noEducSumStats)
write.csv(SumStatOut, paste0(dbLoc, "PCPSumStats.csv"))
