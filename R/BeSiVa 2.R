# Besiva 2 
# Created 12/24/14
# A Ben Rogers Joint
# Last Edited 12/24/14

# Preamble: using what we have done with iteration so far, we will create a new version of the BeSiVa algorithm that works from some simulated data





set.seed(12345)
fdf <- data.frame(dvdat, IVdat) #create full dataframe like we'd see in real life
head(fdf)
Deevname <- colnames(dvdat)

aivnames <- NULL
Ivynames <- names(IVdat)[ !names(IVdat) %in% aivnames] #Make sure that Ivynames and aivnames are mutually exclusive
fam = 'binomial'
niter <- 1
df <- fdf
# offsettr <- 0
# # #create besiva function
# BeSiVa <- function(Deevname, Ivynames, aivnames = NULL, df, fam = 'gaussian', niter = 3){
	#So this is the BeSiVa algorithm
	# Deevname: the Dependent variable name. This is input as text
	# Ivynames: the independent variable names. Input as a list of text
	# aivnames: any always independent variables you'd like included, as text
	# df: the dataframe 
	# fam: the family created by the glm function
	# niter: the number of iterations used by the innermost loop, and a tuning parameter. Input as integer value
	
	
	#define the data for each
	Ivys <- df[, colnames(df) %in% Ivynames & !colnames(df) %in% aivnames]
	Deev <- df[, colnames(df) %in% Deevname]
	
	
	critergen <- function( predicted, measured, fulltabl = FALSE ) {
	predictedRes <- ifelse(predicted >= .5, 1,0)   
	
	if (fulltabl == TRUE) return( prop.table(table(predictedRes == measured, exclude = NULL))) else return( prop.table(table(predictedRes == measured, exclude = NULL))['TRUE']) #output: % true in table of elastic net's predictions on test set
	}
	
	
	
	subsetter <- function(dataframe, sep = 10){ # make a function to take a dataframe and return rows that will become the pseudotest subset for besiva
	fulllen <- nrow(dataframe) #Get and store the full length of the dataset
	ptestlen <- round(fulllen/sep)	#divide it by ten and store the rounded result
	sample(fulllen, ptestlen)	#then get a sample from the rows that uses ptestlen as the 
	}
	
			ifelse( is.null(aivnames) == T, {LapIvys <- Ivynames; offsettr <- 0}, {LapIvys <-  c('', Ivynames); offsettr <- 1}) #if there are no aivs mentioned, make sure the first regression includes an IV. If there are, make sure that the first regression is blank (to account for only the aivs). In addition, create a value for offsettr
 datset	 <- subsetter(df)
	
	 # for(u in 1:niter){ #determines number of rounds of variable consideration (NVar).
		
		#lapply function that loops over all independent variables in Ivys and makes a linear regression with them
		singregs <-  lapply(LapIvys, FUN = function(col, dvname = Deevname, aivees = aivnames, famiglia = fam, alldat = df[-datset,], ptdat = df[datset,]){
			
			ifelse(col == '', Ivform <-  paste(aivees, collapse = ' + '), #if there's nothing in the column, paste the aivs together.
			Ivform <-  paste(paste(aivees, collapse = '+'), col, sep = ' + ')) #otherwise, add in the new column and paste that into Ivform
			if(is.null(aivnames) == T ) print(Ivform <-  gsub('[+]', '', Ivform)) #makes it so if there are no IVs, the extra + sign is scrubbed from Ivform
			 
			print(form <- paste(dvname, '~', Ivform)) #Make and store a formula with IVfom and dvname as text together
			reg <-  glm(as.formula(form), family = famiglia, data = alldat)	#Perform the regression
			print(summary(reg))
			prediction <- predict(reg, ptdat, type = 'response')
			print(data.frame( prediction, dv = ptdat[,dvname]))
			crit <-  critergen(prediction,  ptdat[,dvname], fulltabl = F) #generate the criterion.
				#Just realized, critergen will need to be changed if we ever want to use it on something else. Residuals should work for continuous, but need to brush up on deviance. 
			rm(reg)
			crit
			
			}
		)
	  # }	#Close NVar loop
	aivnames
print(data.frame(LapIvys, criter = unlist(singregs)))
	# } #Close function
	
	
# #define your dummy information
# Ivys <- IVdat[, !colnames(IVdat) %in% aivs]
# Deev <- dvdat
# fam = 'binomial'
# aivdat <- IVdat[, colnames(IVdat) %in% aivs]
	
# BeSiVa("DV", names(IVdat[,!names(IVdat) %in% aivs]), aivnames = aivs, df = fdf, niter = 8)
	
	
	
	
	
	
	
	
