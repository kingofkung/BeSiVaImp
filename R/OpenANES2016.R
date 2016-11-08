## Open and get anes 2016 ready for analysis: On the enigma of donald trump
library(foreign)

if(!exists("anespilot")) anespilot <- read.spss("/Users/bjr/GitHub/BeSiVaImp/Data/anes_pilot_2016.sav", to.data.frame = T)

head(anespilot)

grep("timing", colnames(anespilot), value = T)
grep("flag", colnames(anespilot), value = T)

