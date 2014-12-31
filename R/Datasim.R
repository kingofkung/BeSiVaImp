# Create Simulated Data Code
# Created 12/30/14
# A Ben Rogers Joint
# Last Edited 12/30/14

#Preamble: In order to better focus my BeSiVa Code, I've decided to separate out the simulated data code.
 
rm(list = ls()) 
 
# Phase 1: Create Simulated Data
set.seed(12345) # so it's replicable

# make empty matrix where we can store IVs
IVdat <-  data.frame(matrix(data = NA, ncol = 10, nrow = 50))
IVdat <- as.data.frame( lapply(IVdat, function(x) x <- rnorm(nrow(IVdat)))) #Fill that frame with data
dvdat <- as.data.frame(rbinom(nrow(IVdat), size = 1, prob = 0.5))

seq1 <- 1:nrow(IVdat)
seq2 <- 1:nrow(IVdat) * 2
negseq <- -1* 1:nrow(IVdat)
countdown <- nrow(IVdat) + 1 + negseq
seq7 <- seq2/2 * 7
# devseq <- seq2


seqlist <- list(seq1, seq2, negseq, countdown, seq7)

IVsamp <- sample(length(IVdat), length(seqlist)) #create and store a sample of the columns equivalent to the length of seqlist, so that we can affect only those columns.

IVdat[, IVsamp] <-  IVdat[, IVsamp] + as.data.frame(seqlist) #and add the information

# dvdat <- dvdat + devseq
names(dvdat) <- 'DV'
# #begin working on BeSiVa guts


