# Besiva 2 
# Created 12/24/14
# A Ben Rogers Joint
# Last Edited 12/24/14

# Preamble: using what we have done with iteration so far, we will create a new version of the BeSiVa algorithm that works from some simulated data





set.seed(12345)
fdf <- data.frame(dvdat, IVdat) #create full dataframe like we'd see in real life
head(fdf)
# Deevname <- names(dvdat)
# Ivynames <- names(Ivys)
# aivnames <- names(aivdat)
# df <- fdf

# # #create besiva function
BeSiVa <- function(Deevname, Ivynames, aivnames, df, fam = 'gaussian', niter = 3){
	#So this is the BeSiVa algorithm
	# Deevname: the Dependent variable name. This is input as text
	# Ivynames: the independent variable names. Input as a list of text
	# aivnames: any always independent variables you'd like included, as text
	# df: the dataframe 
	# fam: the family created by the glm function
	# niter: the number of iterations used by the innermost loop, and a tuning parameter. Input as integer value
	
	
	#define the data for each
	Ivys <- df[, colnames(df) %in% Ivynames]
	Deev <- df[, colnames(df) %in% Deevname]
	aivdat <- df[, colnames(df) %in% aivnames] 
	
	critergen <- function( predicted, measured, fulltabl = FALSE ) {
	predictedRes <- ifelse(predicted >= .5, 1,0)   
	
	if (fulltabl == TRUE) return( prop.table(table(predictedRes == measured, exclude = NULL))) else return( prop.table(table(predictedRes == measured, exclude = NULL))['TRUE']) #output: % true in table of elastic net's predictions on test set
	}
	
	
	
	subsetter <- function(dataframe, sep = 10){ # make a function to take a dataframe and return rows that will become the pseudotest subset for besiva
	fulllen <- nrow(dataframe) #Get and store the full length of the dataset
	ptestlen <- round(fulllen/sep)	#divide it by ten and store the rounded result
	sample(fulllen, ptestlen)	#then get a sample from the rows that uses ptestlen as the 
	}
	
	for(u in 1:niter){ #determines number of iterations.
		#lapply function that loops over all independent variables in Ivys and makes a linear regression with them
		singregs <-  lapply(c('', Ivynames), FUN = function(col, dvname = Deevname, aivees = names(aivdat), famiglia = fam, alldat = df[-subsetter(df),], ptdat = df[subsetter(df),]){
		
			ifelse(col == '', Ivform <-  paste(aivees, collapse = '+'), #if there's nothing in the column, paste the aivs together.
			Ivform <-  paste(paste(aivees, collapse = '+'), col, sep = ' + ')) #otherwise, add in the new column and paste that into Ivform
			 
			 
			form <- paste(dvname, '~', Ivform) #Make and store a formula with IVfom and dvname as text together
			reg <-  glm(as.formula(form), data = alldat, family = famiglia)	#Perform the regression
			critergen(predict(reg, ptdat), fdf[,dvname], fulltabl = F) #generate the criterion.
				#Just realized, critergen will need to be changed if we ever want to use it on something else. Residuals should work for continuous, but need to brush up on deviance. 
			
			
			}
		)
				
		bestvar <-  names(Ivys)[which(singregs == max(unlist(singregs))) - 1] #Give me the biggest value of singregs, and make sure to subtract 1 since we've added an AIV regression in singregs.
		
		ifelse(test = bestvar %in% aivs, yes = break ,no = aivs <-  c(aivs, bestvar)) #If bestvar has already been found by the algorithm, exit the loop and return only the aivs that matter. If it hasn't, add bestvar to aivs
	 }	
	aivs

	}
	
	
# #define your dummy information
 aivs = c('X1', 'X3')
# Ivys <- IVdat[, !colnames(IVdat) %in% aivs]
# Deev <- dvdat
# fam = 'binomial'
# aivdat <- IVdat[, colnames(IVdat) %in% aivs]
	
BeSiVa(names(dvdat), names(IVdat	[,!names(IVdat) %in% aivs]), aivnames = aivs, df = fdf, niter = 8)
	
	
	
	
	
	
	
	
