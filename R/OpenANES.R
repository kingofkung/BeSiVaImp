## Here's the script where we open the american national election survey data

loc <- "/Users/bjr/Dropbox/R_Projects/GSSThing/anes_timeseries_cdf_sav/"

library(foreign)

anes <- read.spss(paste0(loc,"anes_timeseries_cdf.sav"))
