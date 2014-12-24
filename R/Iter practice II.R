# Iter practice II
# Created 12/23/14
# A Ben Rogers Joint
# Last Edited 12/23/14

# Preamble: This code replicates an example I found of a way to use lapply to work of ver multiple columns in a regression

set.seed(1000)
values <- runif(50) #Select 50 random Values from the uniform distribution 
values[sample(1:length(values), 10)] <- NA # add some random fluctuations to the data, apparently just for fun
ind.var <- data.frame(matrix(values, ncol = 5)) # and make that into a data frame

# Make a dependent variable
dep <- runif(10)
dep

#create regression formula between each column in the dataframe against the dv

oneVRegNames <-  lapply(colnames(ind.var), function(x) { form.reg <- paste('dep ~ ', x ) }) # What is happening here is that I am applying the function that pastes the "dep ~" to each value in the colnames of ind.var. I do not see a difference between using paste and paste0 in the function I created. I am using paste instead.


#Now let us create a function that lets us make regressions with this technique

oneVRegs <-  lapply(colnames(ind.var), function(x) { 
	form.reg <- paste('dep ~ ', x ) 
	lm(form.reg, data = ind.var)
	}) # Now, instead of returning the text, as seen in oneVarRegNames, we actually return the regressions in the list. What I do not understand is how the code understands what ind.var is if we didn't pass it into the function. Note: It still worked despite using paste!!!
	
	
	
# try it with apply instead of lapply

oneVRegs2 <-  apply(ind.var, MARGIN = 2, function(x) {
	lm(dep ~ x, data = ind.var)
	}
) #Ok. This makes more sense than the one we were using previously, but

# Try to figure out how to make it create a set that starts with just one and adds variables by iteration, without resorting to for loops

dualIter <-  
lapply( colnames(ind.var), function(y){

	x <- lapply(colnames(ind.var), function(x) {paste(x,y)
		})
	})
#This thing iterates first by chainging all the columns in the x, then changing the column in y, and could hypothetically iterate all dual combinations of x, with repeats.

# # Still no clue how to create the output
# X1
# X1 + X2
# X1 + X2 + X3
# X1 + X2 + X3 + X4
# X1 + X2 + X3 + X4 + X5
# # Using only the apply family

# I'd need to figure out how to store the old output somewhere

# How to do it with a for loop:
lastcol <- {}
for( i in colnames(ind.var)) {
	lastcol  <- c(lastcol, i) #how to capture this...
	print(paste(lastcol, collapse = ' + '))
	}


	u <-  t(as.data.frame(lapply(colnames(ind.var), FUN = function(x){ 	
		print(x)
		x
	})))

v <- lapply(u, function(y) {
	
	print(paste(y,yprior))
	
})








