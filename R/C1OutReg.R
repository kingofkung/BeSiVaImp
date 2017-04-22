library(rockchalk)
library(ggplot2)
library(xtable)


datloc <- "/Users/bjr/GitHub/BeSiVaImp/Data/GSS_stata/"
if(!exists("dat2")) dat2 <- read.csv(paste0(datloc, "vote12bindat.csv"))


dat2$vote08nu <- factor(dat2$vote08, levels(dat2$vote08)[c(3, 2, 1)])
## table(dat2$vote08, dat2$vote08nu)
dat2$partyidnu <- factor(dat2$partyid, levels(dat2$partyid)[c(8, 5, 2, 3, 1, 4, 7, 6)])
## table(dat2$partyid, dat2$partyidnu)

varLabs <- c(vote08bin = "Voted in 2008",
             vote08nuineligible = "Ineligible to Vote in 08",
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

m1 <- glm(vote12bin ~ vote08bin + educ, family = binomial, data = dat2)
m2 <- glm(vote12bin ~ degree + partyidnu, family = binomial, data = dat2)
rockchalk::outreg(list("First Attempt" = m1, "Removed Prior Vote" =  m2), varLabels = varLabs, showAIC = T)

renamr <- c('hhtype' = "Household Type",
            'relig' = "Religion",
            'vote08bin' = "Prior Vote",
            'born' = "Native Born",
            'childs' = "Number Of Children",
            'consent' = "Consented to Recording",
            'ethnum' = "Number of Ethnicities",
            'feeused' = "Respondent was Paid",
            'finalter' = "Change in Financial Situation",
            'gender1' = "Gender of First Family Member",
            'hhrace' = "Race of Household",
            'hompop' = "Size of Household",
            'incom16' = "Family Income at Age 16",
            'intsex' = "Sex of Interviewer",
            'mode' = "Interviewed in Person",
            'parborn' = "Parents Born in U.S.",
            'preteen' = "Number of Children in Household",
            'relactiv' = "Active at Church",
            'relig16' = "Religion at Age 16",
            'relpersn' = "Extent of Religiosity",
            'respnum' = "Respondent's Number in Family",
            'savesoul' = "Evangelistic Activities",
            'whoelse1' = "Presence of Children Under 6",
            'whoelse3' = "Presence of Spouse",
            'xnorcsiz' ="Size of Place",
            'class' = "Social Class",
            'famgen' = "Number of Family Generations in Household",
            'family16' = "Living With Parents at Age 16",
            'happy' = "Overall Happiness",
            'mobile16' = "Geographic Mobility Since Age 16",
            'phone' = "Provided Number for Recontact",
            'racecen1' = "Race Census Categories",
            'reg16' = "Region of Residence at 16",
            'rplace' = "Relationship to Head of Household",
            'sibs' = "Number of Siblings",
            'weekswrk' = "Weeks Worked Last Year",
            'wrkstat' = "Labor Force Status",
            'finrela' = "Relative Income",
            'intethn' = "Interviewer Race",
            'satfin' = "Satisfaction With Finances",
            'comprend' = "Respondent's Understanding of Questions",
            'hhtype1' = "Condensed Household Type",
            'res16' = "Type of Place Lived in at Age 16",
            'hefinfo' = "Household Informant's Number in Family",
            'marital' = "Marital Status",
            'attend' = "Religious Attendance",
            'race' = "Race 3 Categories",
            'hispanic' = "Hispanic Ethnicity",
            'degree' = "Last Degree Attained",
            'educ' = "Education in Years",
            'partyid' = "Party Identification" )

dbLoc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/C1/"
vote08Vars <- read.csv(paste0(dbLoc, "C1IntVarsV08Bin.csv"), stringsAsFactors = FALSE)[, 2]
## Having read in the data, we sort it so that ggplot2 knows what
## order we'd like the data in.
VarTab <- sort(table(vote08Vars), decreasing = FALSE)

paste(names(VarTab), collapse = "', '")

vote08VarFac <- factor(vote08Vars, levels = names(VarTab))
## The data.frame makes it so ggplot2 can use it
vote08VarFac <- data.frame("Var" = vote08VarFac)
## Remove any only appearing threshold times.
threshold <- 0
votegt2 <- names(which(table(vote08VarFac) > threshold))
vote08VarFac <- data.frame("Var" = vote08VarFac[as.character(vote08VarFac$Var) %in% votegt2, ])


NoEducVars <- read.csv(paste0(dbLoc, "C1IntVarsNoEduc.csv"), stringsAsFactors = FALSE)[, 2]
## repeat with the sorting
NoEducTab <- sort(table(NoEducVars), decreasing = FALSE)
NoEducVarFac <- factor(NoEducVars, levels = names(NoEducTab))
NoEducVarFac <- data.frame("Var" = NoEducVarFac)
## Remove the one again
educgt2 <- names(which(table(NoEducVarFac) > threshold))
NoEducVarFac <- data.frame("Var" = NoEducVarFac[as.character(NoEducVarFac$Var) %in% educgt2,])


noVoteVars <- read.csv(paste0(dbLoc, "C1IntVarsNoVote.csv"), stringsAsFactors = FALSE)[, 2]
## Create Data Frame... If we do this again, we should make it a function, especially when we rename them all
noVoteTab <- sort(table(noVoteVars), decreasing = FALSE)
paste(names(noVoteTab), collapse = "', '")

noVoteVarFac <- factor(noVoteVars, levels = names(noVoteTab))
noVoteVarFac <- data.frame("Var" = noVoteVarFac)
## Unique Variable Removal
noVotegt2 <- names(which(noVoteTab > threshold))
noVotegt2bool <- as.character(noVoteVarFac$Var) %in% noVotegt2
noVoteVarFac <- data.frame("Var" = noVoteVarFac[noVotegt2bool,])


VarPlotFunc <- function(dat, title){
    ggplot(data = dat) +
        geom_bar(aes(x = Var), fill = "darkgreen") +
            theme_classic(10) +
                theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", face = "bold"),
                      panel.grid.major.x = element_line(color = "gray")) +
                          xlab("Variable Names") +
                              ylab("Count") +
                                  ggtitle(title) +
                                  coord_flip()
}

dev.new()
pdf(paste0(dbLoc, "GSS", 100, "C1.pdf"))
fullTitle <- paste("Number of times BeSiVa Selected a Variable Out of", 100, "Runs\n In the 2014 GSS Data")
VarPlotFunc(vote08VarFac, title = fullTitle)
graphics.off()

dev.new()
pdf(paste0(dbLoc, "GSS", 100, "C1NoVote08.pdf"))
fullTitleNPV <- paste("Number of times BeSiVa Selected a Variable Out of", 100, "Runs\nIn the 2014 GSS Data Without Prior Vote")
VarPlotFunc(noVoteVarFac, title = fullTitleNPV)
graphics.off()


dev.new()
pdf(paste0(dbLoc, "GSS", 100, "C1NoEduc.pdf"))
fullTitleNE <- paste("Number of times BeSiVa Selected a Variable Out of", 100, "Runs\nIn the 2014 GSS Data Without Education")
VarPlotFunc(NoEducVarFac, title = fullTitleNE)
graphics.off()





vote08PCPs <- read.csv(paste0(dbLoc, "C1PCPsV08Bin.csv"), stringsAsFactors = FALSE)[, 2]
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

NoVote08PCPs <- read.csv(paste0(dbLoc, "C1PCPsNoVote.csv"), stringsAsFactors = FALSE)[, 2]
dev.new()
pdf(paste0(dbLoc, "noVote08HistGSS.pdf"))
hist(NoVote08PCPs*100,
     main = "A Histogram of PCPs for the GSS\n Without Prior Vote",
     xlab = "Percent Correctly Predicted")
graphics.off()

noVote08SumStats <- round(summarize(NoVote08PCPs * 100)$numerics, 2)
colnames(noVote08SumStats) <- NULL


dev.new()
pdf(paste0(dbLoc, "NoEducHistGSS.pdf"))
NoEducPCPS <- read.csv(paste0(dbLoc, "C1PCPsNoEduc.csv"), stringsAsFactors = FALSE)[, 2]
hist(NoEducPCPS*100,
     main = "A Histogram of PCPs for the GSS\n Without Education Variables",
     xlab = "Percent Correctly Predicted")
graphics.off()

noEducSumStats <- round(summarize(NoEducPCPS * 100)$numerics, 2)
colnames(noEducSumStats) <- NULL

dev.new()
pdf(paste0(dbLoc, "JustPriorVoteHistGSS.pdf"))
justVote08PCPs <- read.csv(paste0(dbLoc, "justVote08BinPCPs.csv"))[,2]
hist(justVote08PCPs * 100,
     main = "A Histogram of PCPs for the GSS\n Using Only Prior Vote",
     xlab = "Percent Correctly Predicted")
graphics.off()

justPVSumStats <- round(summarize(justVote08PCPs * 100)$numerics, 2)
colnames(justPVSumStats) <- NULL

SumStatOut <- data.frame("All Variables" = mainSumStats, "Removed Prior Vote" = noVote08SumStats, "Only Prior Vote" = justPVSumStats)
write.csv(SumStatOut, paste0(dbLoc, "PCPSumStats.csv"))

xtable(SumStatOut[, ])

## get some confidence intervals of the mean prediction

meanconfint <- function(x) {mean(x) + qt(c(.025, .975), df = length(x)-1) * sd(x)/sqrt(length(x))}

vote08confint <- meanconfint(vote08PCPs)
noVote08confint <- meanconfint(NoVote08PCPs)
justVote08confint <- meanconfint(justVote08PCPs)


confintdf <- rbind(vote08confint, noVote08confint, justVote08confint)
rownames(confintdf) <- c("All Variables", "No Prior Vote", "Only Prior Vote")
colnames(confintdf) <- c("Lower Bound", "Upper Bound")
print(xtable(confintdf*100, digits =  2))
