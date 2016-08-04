## Here I attempt to work on making a Barplot of the data I leave in
## that new file I made in the dropbox
datloc <- "/Users/bjr/Dropbox/Dissertation Stuff/DatOutPut/"
bardat <- read.csv(paste0(datloc, "ivsofint 50.csv"), header = F, na.strings = "", fill = T)
## Turn from several columns into one column
barvals <- data.frame("vals"= na.omit(unlist(as.list(bardat))))
## Get the order of variables, and the counts for later consideration
bartab <- sort(table(barvals), decreasing = T)
## sort the variables by most frequent
barvals$vals <- factor(barvals$vals, levels = names(bartab))

library(ggplot2)

ggplot(data = data.frame(barvals)) + geom_bar(aes(vals), fill = "darkgreen") + theme_classic() + xlab("Variable Names") + ylab("Count") + ggtitle("Number of times BeSiVa Selected a Variable Out of 50 Runs")

