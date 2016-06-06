## Here I see how anes data looks when opened as a .sav file in the r
## programming language. If I am lucky, it will have been recoded
## properly.

library(foreign)

loc <- "/Users/bjr/GitHub/BeSiVaImp/Data/"

anes <- read.spss(paste0(loc, "anes_pilot_2016.sav"), to.data.frame = TRUE)


## Recode vote choice to be in binary format.
anes$turn12bin <- as.character(anes$turnout12)
anes$turn12bin[anes$turn12bin == "Definitely did not vote"] <- 0
anes$turn12bin[anes$turn12bin == "Definitely voted"] <- 1
anes$turn12bin[anes$turn12bin == "Not completely sure"] <- NA
anes$turn12bin <- as.numeric(anes$turn12bin)
table(anes$turnout12, anes$turn12bin, useNA = "ifany")
