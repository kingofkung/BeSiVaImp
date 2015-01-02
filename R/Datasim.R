# Create Simulated Data Code
# Created 12/30/14
# A Ben Rogers Joint
# Last Edited 12/30/14

#Preamble: In order to better focus my BeSiVa Code, I've decided to separate out the simulated data code.
 
rm(list = ls()) 
 
# Phase 1: Create Simulated Data
set.seed(123456) # so it's replicable

# make empty matrix where we can store IVs
IVdat <-  data.frame(matrix(data = NA, ncol = 10, nrow = 1017))
IVdat <- as.data.frame(lapply(IVdat, function(x) x <- rnorm(nrow(IVdat), mean = 0, sd = 7))) #Fill that frame with data




seq1 <- sample(seq(0,7, by = .0001), nrow(IVdat), replace = T)
seq2 <- sample(seq(-5, 5, by = .01 ), nrow(IVdat), replace = T)
seq3 <- sample(seq(0,2, by = .001), nrow(IVdat), replace = T)
seq4 <-  1 + seq3 + sample(0:1, nrow(IVdat), replace = T)
seq7 <- sample(1:3, size = nrow(IVdat), replace = T) * 7
# devseq <- seq2




seqlist <- list(seq1, seq2, seq3, seq4, seq7)

# IVsamp <- sample(length(IVdat), length(seqlist)) #create and store a sample of the columns equivalent to the length of seqlist, so that we can affect only those columns.
IVsamp <- c(2,3,6,7,8) #No reason to keep generating this.

names(IVdat[,IVsamp])

IVdat[, IVsamp] <-  IVdat[, IVsamp] + as.data.frame(seqlist) #and add the information

# dvdat <- dvdat + devseq

relation <- 2 + seq2 + seq4 

dvdat <- data.frame('DV' = exp(relation)/(1 + exp(relation))) #Get into logistic form
dvdat <-  ifelse(dvdat >= .5, 1, 0)

# #begin working on BeSiVa guts


