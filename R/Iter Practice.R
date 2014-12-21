# Data set design using stuff PJ Taught me
# Begun 12/9/14
# A Ben Rogers Joint
# Last Edited 12/9/14

# Preamble: So this is my attempt at solving two problems. The first involves the fact that I don't really know a lot about iteration. I know how to make a for loop work , but I'm not sure how to apply any of R's other options for iterating. Also, I had a classmate complain about making datasets by hand for his methods class, and I want to make it so he doesn't have to do that. 
# Thus, the goal of this code is to create x datasets using the same generating process while keeping the number of for loops to a minimum.

set.seed(12345) #make it replicable


classnames <- c('Aaron', 'Ben', 'Chuck', 'Denise', 'Edward', 'Frieda', 'Grant', 'Hingle', 'Ian', 'Jung', 'Karl', 'Lionel', 'Mona', 'Nancy', 'Olivia', 'Plectrum', 'Quentin', 'Ryan', 'Sarah', 'Tommy', 'Ulysses', 'Vivian', 'Will', 'Xi', 'Yolanda', 'Zonker') #this could very easily be imported from a .csv file

nstudents <-  length(classnames) #number of sets to create
numobs <- 25 #number of observations in each set

datgenfunc <- function(xlen){ # This function makes a list with two variables (named y1 and x1, adds noise to x1, and returns the list. The only argument xlen is how many obervations should be in the list
	datalist <- list(y1 = 1:xlen, x1 = 1:xlen) #create a list with elements from 1 to length x
	datalist$x1 <- datalist$x1 + rnorm(length(datalist$x1), mean = 0, sd = 1) # add some noise to the x vector
	return(datalist)
	}



datlist2 <- vector(mode = 'list', length = nstudents)
names(datlist2) <- classnames


setwd('/Users/bjr/Desktop/School/POLS_906/Classdata')
for (i in classnames) {
	
	datlist2[[i]] <- datgenfunc(numobs)
	write.csv(datlist2[[i]], file = paste(i,"'s datalist.csv", sep = ''), row.names = F)


}

write.csv(datlist2, file = 'Fulldatalist.csv', row.names = F)