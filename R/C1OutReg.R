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
vote08Vars <- read.csv(paste0(dbLoc, "C1IntVarsVote08.csv"), stringsAsFactors = F)[,2 ]
 table(vote08Vars)<5

VarTab <- sort(table(vote08Vars), F)


vote08VarFac <- factor(vote08Vars, levels = names(VarTab))
vote08VarFac <- as.data.frame(vote08VarFac)
colnames(vote08VarFac) <- "Var"

library(ggplot2)
dev.new()
pdf(paste0(dbLoc, "GSS", 100, "runsC1.pdf"))
ggplot(data = vote08VarFac) +
    geom_bar(aes(x = Var, stat = "identity"), fill = "darkgreen") +
    theme_classic(10) +
    xlab("Variable Names") + ylab("Count") +
    ggtitle(paste("Number of times BeSiVa Selected a Variable Out of", 100, "Runs\n 2014 GSS Data")) +
    coord_flip()
graphics.off()





vote08PCPs <- read.csv(paste0(dbLoc, "C1PCPsVote08.csv"), stringsAsFactors = FALSE)[, 2]
hist(vote08PCPs*100)

library(rockchalk)
round(summarize(vote08PCPs * 100)$numerics, 2)
