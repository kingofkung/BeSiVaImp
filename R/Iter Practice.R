# Data set design using stuff PJ Taught me
# Begun 12/9/14
# A Ben Rogers Joint
# Last Edited 12/21/14

# Preamble: So this is my attempt at solving two problems. The first involves the fact that I don't really know a lot about iteration. I know how to make a for loop work , but I'm not sure how to apply any of R's other options for iterating. Also, I had a classmate complain about making datasets by hand for his methods class, and I want to make it so he doesn't have to do that. 
# Thus, the goal of this code is to create x datasets using the same generating process while keeping the number of for loops to a minimum.

set.seed(12345) #make it replicable 
inputwd <- '/Users/bjr/Desktop/ClassData' #Set the directory where all data will go
# inputwd <- "C:/Users/usernamehere/Desktop" #Windows version to put the data on the desktop

setwd(inputwd)

classnames <- c('Aaron', 'Ben', 'Chuck', 'Denise', 'Edward', 'Frieda', 'Grant', 'Hingle', 'Ian', 'Jung', 'Karl', 'Lionel', 'Mona', 'Nancy', 'Olivia', 'Plectrum', 'Quentin', 'Ryan', 'Sarah', 'Tommy', 'Ulysses', 'Vivian', 'Will', 'Xi', 'Yolanda', 'Zonker') #this could very easily be imported from a .csv file. You could also use a number sequence if you didn't feel like typing it in

nstudents <-  length(classnames) #number of sets to create
numobs <- 25 #number of observations in each set

datgenfunc <- function(xlen){ # This function makes a list with two variables (named y1 and x1, adds noise to x1, and returns the list. The only argument xlen is how many obervations should be in the list
	
	yuno <- 1:xlen + rnorm(xlen, mean = 0, sd = .01)#make our dv variable and add a little noise. calling it yuno for now
	xuno <- rnorm(xlen, mean = 5, sd = 2) #create irrelevant variables
	xdos <- yuno + rnorm(xlen, mean = 0, sd = 1) #I call them yuno and xuno because you could have multiple values. This one is relevant
	xtres <- rnorm(xlen, mean = 3, sd = 4)
	xquatro <- yuno*-1 + 3

		
	datalist <- list(y1 = yuno, x1 = xuno, x2 = xdos, x3 = xtres, x4 = xquatro) #create a list with multiple elements from 1 to length xlen
	return(datalist)
	}



datlist2 <- vector(mode = 'list', length = nstudents) #create a blank list
names(datlist2) <- classnames #names for each list element become names of each student



for (i in classnames) {	
	datlist2[[i]] <- datgenfunc(numobs) #create data for each student and store it in the list
	write.csv(datlist2[[i]], file = paste(i,"'s datalist.csv", sep = ''), row.names = F) #write each dataset as a .csv file in the inputwd folder
}


write.csv(datlist2, file = 'Fulldatalist.csv', row.names = F) #write the master dataset to another file