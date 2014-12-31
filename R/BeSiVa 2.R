# Besiva 2 
# Created 12/24/14
# A Ben Rogers Joint
# Last Edited 12/24/14

# Preamble: using what we have done with iteration so far, we will create a new version of the BeSiVa algorithm that works from some simulated data


critergen <- function( predicted, measured, fulltabl = FALSE ) {
	predictedRes <- ifelse(predicted >= .5, 1,0)   
	
	if (fulltabl == TRUE) return( prop.table(table(predictedRes == measured, exclude = NULL))) else return( prop.table(table(predictedRes == measured, exclude = NULL))['TRUE']) #output: % true in table of elastic net's predictions on test set
}


#define your dummy information
aivs = c('X1', 'X3')
Ivys <- IVdat[, !colnames(IVdat) %in% aivs]
Deev <- dvdat
fam = 'binomial'
aivdat <- IVdat[, colnames(IVdat) %in% aivs]



fdf <- data.frame(Deev, aivdat, Ivys) #create full dataframe like we'd see in real life




subsetter <- function(dataframe, sep = 10){ # make a function to take a dataframe and return rows that will become the pseudotest subset for besiva
	
	fulllen <- nrow(dataframe) #Get and store the full length of the dataset
	ptestlen <- round(fulllen/sep)	#divide it by ten and store the rounded result
	sample(fulllen, ptestlen)	#then get a sample from the rows that uses ptestlen as the 
}

for(u in names(Ivys)){
#lapply function that loops over all independent variables in Ivys and makes a linear regression with them
	singregs <-  lapply(c('', names(Ivys)), FUN = function(col, dvname = names(Deev), aivees = names(aivdat), famiglia = fam, alldat = fdf[-subsetter(fdf),], ptdat = fdf[subsetter(fdf),]){

		ifelse(col == '', Ivform <-  paste(aivees, collapse = '+'), #if there's nothing in the column, paste the aivs together.
		Ivform <-  paste(paste(aivees, collapse = '+'), col, sep = ' + ')) #otherwise, add in the new column and paste taht in
		 
		 
		form <- paste(dvname, '~', Ivform) #Make and store a formula with IVfom and dvname as text together
		print(reg <-  glm(as.formula(form), data = alldat, family = famiglia))	#Perform the regression
		print(critergen(predict(reg, ptdat), fdf[,dvname], fulltabl = F)) #generate the criterion.
			#Just realized, critergen will need to be changed if we ever want to use it on something else. Residuals should work for continuous, but need to brush up on deviance. 
		
		
		}
	)
	
print(singregs)

# which(singregs == max(unlist(singregs))) # get the biggest in the list of ivs

bestvar <-  names(Ivys)[which(singregs == max(unlist(singregs))) - 1] #Give me the biggest value of singregs, and make sure to subtract 1 since we've added an AIV regression in singregs.

aivs <-  c(aivs, bestvar)
}


#create besiva function
BeSiVa <- function(Deev, Ivys, aivs, fam = 'gaussian'){
#So this is the BeSiVa algorithm
	# Deev: the Dependent variable data. This si input as a column of data
	# Ivys: the independent variable data. Input as a dataframe or list
	# aivs: any always independent variables you'd like included, 
	# fam: the family created by the glm function
	
	
	}
