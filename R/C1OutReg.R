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
