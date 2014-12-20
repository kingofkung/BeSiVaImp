# BeSiVa Algorithm improvement ed.
# A Ben Rogers Joint
# Started 4/9/2014
# Last Edited 10/22/2014
 

rm(list = ls())# Remove everything that's not the master iterator u

#Load relevant packages

library(car)
library(xlsx)
library(gdata)
library(rockchalk)
library(caret) #loads ggplot2
library(leaps)
library(glmnet)
library(mice)
library(mgcv) 
library(randomForest)
library(rpart) 
library(adabag)
library(ada)

set.seed(2398745)



# function section

##' Critergen is a function that when fully operational, will generate different criteria for how well data is predicted by different values.
##' inputs: the predictions made by the predict() function and the actual values measured as 1d arrays.
critergen <- function( predicted, measured, fulltabl = FALSE ) {
	predictedRes <- ifelse(predicted >= .5, 1,0)   
	
	if (fulltabl == TRUE) return( prop.table(table(predictedRes == measured, exclude = NULL))) else return( prop.table(table(predictedRes == measured, exclude = NULL))['TRUE']) #output: % true in table of elastic net's predictions on test set
}


#Go to correct file location.  

inputwd <- "/Users/bjr/Google Drive/Activate/Wakefield Modelling Project/Wakefield Input 2"
setwd(inputwd)
outputwd <- "/Users/bjr/Google Drive/Activate/Wakefield Modelling Project/Wakefield Output 2/Wakefield Regression Outputs"
dateUnformed <-  date()
dateFormed <-  strsplit(dateUnformed, split = ' ')
dateFormed <- paste(dateFormed[[1]][c(2,4)], collapse = ' ')

maindf <-  readRDS('maindf.RDS')
Vandat <- readRDS('Vandat.RDS')



 
maindforig <-  readRDS('maindf2.rds')


ifilename <-  'imputation3.rds' #Name of file where imputation is stored:
mickey <- readRDS(file = ifilename)
maindf2 <- complete(mickey)

colnames(maindf2)

# Preproc for maindf2
Agebreaks <- c(17, 30, 45, 64, 200)
Agelabels <- c('18-30', '31-45', '46-64', '65+')
maindf2$cat_age <- cut(maindf2$Age, breaks = Agebreaks , labels = Agelabels)

maindf2$SpecVotes <- 0
maindf2[ which(maindf2$voted_08g %in% c(1) & maindf2$voted_12g %in% c(1)), 'SpecVotes'] <- 1
maindf2[maindf2$voted_10g %in% c(1), 'SpecVotes'] <- 0

maindf2$TenDummy <- 0
maindf2[maindf2$voted_10g %in% c(1), 'TenDummy'] <- 1




# app


varsToNotInclude <- c("prospectid", 'attemptcount', 'zipcode', 'county', 'voterid','VANID', 'Notes','PollingAddress', 'PollingLocation', 'PrecinctName', 'block_group', 'X2012.ClarityTurnout','Phone','PersonID', 'votebuilder_identifier', 'BoardOfEducationCode','namecheck', 'datelastcalled', 'datasource_name','agent_name', 'firstname', 'lastname', 'sp04', 'sp05', 'sp06', 'sp03', 'reg_earliest_month', 'cons_childcnt', 'others_num_female')

maindf2 <- maindf2[, !colnames(maindf2) %in% varsToNotInclude]

maindf2[, lapply(maindf2, is.character)==T] <- lapply(maindf2[, lapply(maindf2, is.character)==T], as.factor)
head(maindf2)

maindforig <- maindforig[, !colnames(maindforig) %in% varsToNotInclude]


#begin working on iterating data for purpose of presentation

u <- 1
	ptr <- proc.time()
	

	# pull out a tenth of the data for control purposes
	tenperc <-  createFolds(1:nrow(maindf2), k = 10)
	
	controldf2 <- maindf2[tenperc$Fold06,] #Order Matters: previously this data had not been fully recorded, as parts were cut out due to the redefininition of maindf2 in the line below
	truecont <- maindf2[tenperc$Fold04,]
	
	maincontdf2 <- maindf2[-c(tenperc$Fold04),]
	
	maindf2 <- maindf2[-c(tenperc$Fold06, tenperc$Fold04),]
	
	maindforigcont <- maindforig[tenperc$Fold04,]
	maindffullorig <- maindforig[-tenperc$Fold04,]
	maindforig <- maindforig[-c(tenperc$Fold06, tenperc$Fold04),]
	
	contrasts( maincontdf2$Party)
	contrasts( truecont$Party)
	# rownames(maindf2[tenperc$Fold10,])
	
	
	# junkermod <-  glm(sp03 ~. , family ='binomial',  data = maindf2)
	# model.matrix(maindf2)
	
	
	
	
	#BEGIN LOOP HERE!
	NIVs <- 1 #The number of IVs, this will eventually serve as the building block of the outer loop
	MAXIVs <- 10 # Number of Go rounds for IV selection
	
	deevlist <- c('sp03', 'sp04','sp05','sp06','sp08') 
	extraagevars <-  colnames(maindf2)[grep("age_", colnames(maindf2))]
	gendervars <- colnames(maindf2)[grep("male", colnames(maindf2))]
	
	redundant <- c('reg_party_dem','others_num_dem', 'others_num_rep', 'reg_party_ind', 'clarity_party', 'cons_dbi_educ_nohs', 'cat_ageDe', extraagevars, gendervars) #Age sex and Party aren't actually redundant, but since we hard code them into the loop, this is a good place to tell the code, hey we don't need you included in the boostrapping selection process 
	zerovar <- c('voted_12p_dem', 'voted_10p_dem')
	mostlyNAs <- c('cons_on_outside_list')
	ActivateVars <- c('prospectid', 'PersonID', 'zipcode')
	clarityvars <- colnames(maindf2)[grep('clarity', colnames(maindf2))]
	
	
	colsnottouse <- c("sp07Rec", "sp07.2Rec", "sp08Rec", 'reg_party_rep','attemptcount', 'voterid', 'votebuilder_identifier', 'dsnRec', 'cen10_asian', 'namecheck', 'phone_primary_cell',zerovar, redundant, mostlyNAs, ActivateVars, clarityvars, colnames(maindf2)[nearZeroVar(maindf2)], 'sp08', 'deevdiv')
	
	# colnames(maindf2)
	#construct dataframes for prediction and comparison
	xdata <- 	model.matrix(sp08 ~ ., data = maincontdf2)
	newxdata <- model.matrix(sp08 ~ ., data = truecont) #change it so that all predictions are made on truecont
	data.frame( colnames(xdata), colnames(newxdata))
	ydata <- maincontdf2[,'sp08']
	
	

	
	Rprof('bensprof.txt')
	# write.table('',file = '/Users/bjr/GitHub/bjrThesis/R/scores.csv', sep = ',', col.names = F, row.names = F) #clear scores table					
	
	# for(L in 1:1) { #begin DV loop
		deev <- 'sp08'
		maindf2$deevdiv <- maindf2[,deev] #for this one, we don't need to do anything to deevdiv to make it work. Unfortunately, it's pretty much everywhere instead of deev, so I'm just passing it on here.
		controldf2$deevdiv <- controldf2[,deev] #ditto.
		# colnames(maindf2)
		
		
		
		#chose columns for our sample
		numsforsample <- 1:ncol(maindf2)
		
		initlooplength <-  ncol(maindf2[,!colnames(maindf2) %in% colsnottouse]) 
		
		# colnames(maindf2)
		
		colsnumstoavoid <-  which(colnames(maindf2) %in% colsnottouse)
		colnumstouse <- which(!colnames(maindf2) %in% colsnottouse)  # get all the columns we want to use as IVs
		
		# write.csv( colnames(maindf2[,colnumstouse]), '/Users/bjr/GitHub/bjrThesis/R/colnumsused.csv')
		
		#create variable selecting loop
		# This loop will select a single variable and regress it against deev
		currentReg <- NULL # This will hold the regression of the moment
		bestReg <- NULL
		bestVarResSt <- NULL
		# priorIVs <- c('cat_age', 'Sex', 'PartyRec', 'cen00_medianincomeRec', "CountyName")
		# priorIVs <- c('cat_age', 'Sex', 'PartyRec', 'CountyName','cen10_densityRec','cen00_medianincomeRec')
		
		# priorIVs <- c('cat_age', 'Sex', 'Party', 'CountyName', 'SpecVotes', 'TenDummy', 'cen10_density', 'cen00_medianincome') #Mark's IV's
		# priorIVs <- c('cat_age', 'Sex', 'Party', 'SpecVotes', 'TenDummy')
		 # priorIVs <- NULL #for when we want to run it without initializing. Surprisingly, this does two things. 1. It performs more poorly than if we add AIVs (that is, overall lower criterion values), 2. It doesn't select any of the AIVs we think are relevant. 3. It doesn't 
		
		  priorIVs <- NULL #For when we want to do something without prior IVs
		
		for(NIVs in 1:MAXIVs){ #Outer Loop Begins
			metabreaker <- 999 #set metabreaker to some nonzero value. 999, in honor of the current greatest troll of all time. 
			# print( paste( 'NIVs =', NIVs))
			print(paste('dv =',deev))
			# if(NIVs == 1) priorIVs <- c('cat_age', 'Sex', 'Party')
			
			
			# Need to figure out how to make it so that If at no point does something happen in the loop below, break out of the outermost loop
			
			for(i in 1:initlooplength) {#Inner Loop Begins
				# for(i in 1:length) {#Inner Loop Begins
				
				iloopbreaker <- 1 #iloopbreaker begins as 1
				
				 ivstouse <- c(priorIVs, colnames(maindf2)[colnumstouse[i]])
								

				ivformed <-  paste(ivstouse, collapse = ' + ')
				formedeqn <- as.formula(paste('deevdiv', " ~ ", ivformed)) #Form our equation. In this version, we're going to need to figure out the DV's structure before we start these loops
				
				
				traindf2 <- maindf2[complete.cases(maindf2$deevdiv), c('deevdiv', ivstouse)]
				
				
				currentReg <-  glm(formula = formedeqn, data = maindf2, family = 'binomial') #perform a regression and store it in currentReg
				# summary(currentReg)
				predict(currentReg)
				
				
							 # bestReg <-  glm(formula = formedeqn, data = maindf2, family = 'binomial') #perform a logit regression and store it in currentReg
				
				
				 # currentVarResid <-  var( currentReg$resid)  #Get currentReg's residuals, and store their variance
				traindf2$currentpreds <- predict(currentReg, traindf2, 'response') 
				controldf2$currentpreds <- predict(currentReg, controldf2, 'response') 
				
				criter <- critergen(controldf2$currentpreds, controldf2$deevdiv) #now generates criterion based on validation set
				 
				
				if(i == 1 & NIVs == 1 ){ #On the very first pass,
					bestReg <- currentReg #The First regression is the best regression.
					bestRtPredRat <- criter
				
					print(paste('Rate to beat =',round( criter, 6)))
					bestIV <- colnames(maindf2)[colnumstouse[i]]
					 } #keep them always if i == 1
				
				if(criter > bestRtPredRat){ #if the current prediction ratio is bigger than the best one so far
					bestReg <- currentReg #put the current reg as best reg
					# print(summary(bestReg)$coefficients[,3])#print z values
					# print(criter) 
	
					bestRtPredRat <- criter # Put the current right prediction ratio as the best one, 
					bestIV <- colnames(maindf2)[colnumstouse[i]] #and save the bestIV for storage in priorIVs
					print(paste('Best Criterion Value =',round( bestRtPredRat,6)))
					metabreaker <- 0 #make the metabreaker variable = 0
				} # if the best regressions residual variance is bigger than the current regression's residual variance, the current regression replaces the prior best regression. 	
					
				
				# print(paste('i =', i))
				# rm(rtPredRat)
				 rm(criter)# rm(contPredRat)
				
				if(metabreaker != 0) iloopbreaker <- 0 #if metabreaker is still equal to its original nonzero value, break the loop.
				
			}# Inner Loop Ends
			# 
			# print( paste( 'NIVs =', NIVs))
			priorIVs <- c(priorIVs, bestIV) #The bestIV now becomes part of the prior IVs
			print(priorIVs)
			
			#Executive Decision: This program must become more aggressive at countering multicollinearity, if only because the code runs at a snail's pace. 
			#Also, how to tell whether the IV's that survive the KFold cross validation are actually liable to reduce overall variance of residuals, or just lucky? Preferably without adding on to computational time? 
			
			
			
			 #get the names of all IVs in the best Regression (excluding the intercept)
			 length(priorIVs) == NIVs
			 initlooplength <- initlooplength - 1 # since one of our variables is lost to the best regression, reduce the length of our internal loop by one
			
			#Now, we need to figure out how to make sure it's not going to repeatedly grab the same variable, eliminating it from colnumstouse
			
			
			
			 IVcols <-  which(colnames(maindf2) %in% priorIVs) #get numbers of columns that have been used so far
			
			
			# browser()
			 # colnumstouse <- colnumstouse[ !colnumstouse %in% IVcols] #get rid of the columns used previously by taking them out of colnums to use. 
			 
			 colnumstouse <- setdiff(colnumstouse, IVcols) # only column numbers outside of IVcols 
			 
			 if(iloopbreaker == 0) break #if I loopbreaker is 0, break out of the iv loop and move onto the next dv
		} #Outer Loop ends
		print('')
		print('')
		print(deev)
		print( summary(bestReg))
		# str(bestReg)
		print(paste('nVal =', length(bestReg$residuals)))
		# summary(bestReg$residuals)
		# hist(bestReg$residuals)
		
		print('')		
		deevpreds <-  predict(bestReg, maindf2, 'response')
		contpreds <- predict(bestReg, controldf2, 'response')
		bestRegST <- bestReg 
		
		#Clean up before next iteration
		# rm( deev, colnumstouse, bestRtPredRat)
		# } # End DV Loop
	
	# masterregST <- 	c(masterregST, coef(bestRegST)[-1])
	Rprof(NULL)
		
	# summaryRprof('bensprof.txt')



# system('say Done!')




