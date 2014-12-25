# Besiva 2 
# Created 12/24/14
# A Ben Rogers Joint
# Last Edited 12/24/14

# Preamble: using what we have done with iteration so far, we will create a new version of the BeSiVa algorithm that works from some simulated data


critergen <- function( predicted, measured, fulltabl = FALSE ) {
	predictedRes <- ifelse(predicted >= .5, 1,0)   
	
	if (fulltabl == TRUE) return( prop.table(table(predictedRes == measured, exclude = NULL))) else return( prop.table(table(predictedRes == measured, exclude = NULL))['TRUE']) #output: % true in table of elastic net's predictions on test set
}


# Phase 1: Create Simulated Data
set.seed(12345) # so it's replicable




# make empty matrix to store IVs in
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

dvdat <- dvdat + devseq
names(dvdat) <- 'DV'
# #begin working on BeSiVa guts


#define your dummy information
aivs = c('X1', 'X3')
Ivys <- IVdat[, !colnames(IVdat) %in% aivs]
Deev <- dvdat
fam = 'gaussian'
aivdat <- IVdat[, colnames(IVdat) %in% aivs]
names(aivdat)
names(Ivys[3])

fdf <- data.frame(Deev, aivdat, Ivys) #create full dataframe like we'd see in real life


subsetter <- function(dataframe){ # make a function to take a dataframe and return rows that will become the pseudotest subset for besiva
	
	fulllen <- nrow(dataframe) #Get and store the full length of the dataset
	ptestlen <- round(fulllen/10)	#divide it by ten and store the rounded result
	sample(fulllen, ptestlen)	#then get a sample from the rows that uses ptestlen as the 
}


#lapply function that loops over all independent variables in Ivys and makes a linear regression with them
	singregs <-  lapply(c('', names(Ivys)), FUN = function(col, dvname = names(Deev), aivees = names(aivdat), famiglia = fam, alldat = fdf[-subsetter(fdf),], ptdat = fdf[subsetter(fdf),]){

		ifelse(col == '', Ivform <-  paste(aivees, collapse = '+'), #if there's nothing in the column, paste the aivs together.
		Ivform <-  paste(paste(aivees, collapse = '+'), col, sep = ' + ')) #otherwise, add in the new column and paste taht in
		 
		 
		form <- paste(dvname, '~', Ivform) #Make and store a formula with IVfom and dvname as text together
		reg <-  lm(as.formula(form), data = alldat)	
		print(critergen(predict(reg, ptdat), fdf[,dvname], fulltabl = T))
		
		
		}
	)
	
which(singregs == min(abs(unlist(singregs)))) #until we can get critergen to work via simulated data, this could be useful

#create besiva function
BeSiVa <- function(Deev, Ivys, aivs, fam = 'gaussian'){
#So this is the BeSiVa algorithm
	# Deev: the Dependent variable data. This si input as a column of data
	# Ivys: the independent variable data. Input as a dataframe or list
	# aivs: any always independent variables you'd like included, 
	# fam: the family created by the glm function
	
	
	}
