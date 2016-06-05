## Here I see how anes data looks when opened as a .sav file in the r
## programming language. If I am lucky, it will have been recoded
## properly.

library(foreign)

loc <- "/Users/bjr/GitHub/BeSiVaImp/Data/"

anes <- read.spss(paste0(loc, "anes_pilot_2016.sav"), to.data.frame = TRUE)
head(anes)
colnames(anes)
anes$religpew
