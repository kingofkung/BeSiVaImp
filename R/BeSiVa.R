rm(list = ls())

source("Datasim2.R")

source("BeSiVaFunctions.R")



## Get the data subset. holdoutrows is a sample of rows that will be
## kept out of the data when it is time to make the models, and used
## to test the models as well.
holdoutrows <- sample(1:matrows, round(matrows/10))

## Keep these two rows out. Why? So we can keep that here for when we
## actually want to exclude variables.

dontuse <- c("V5", "V9")
devee <- "DV"


## Make text versions of formulae, but don't make one of with dv or
## the variables in dontuse on the RHS
formulae <- lapply(colnames(mat), function(x, dvname = devee, excludr = dontuse) {
    if(dvname != x & !x %in% excludr) paste0(dvname, " ~ ", x)
}
                   )


## Turn the list into a vector, and get rid of any nulls in one easy unlist command.
formulae <- unlist(formulae)

## Make the glm regressions (taking care to not use the held out data)
## and store them in the variable glms
glms <- lapply(formulae, function(x, dattouse = mat[-holdoutrows, ]){
    glm( as.formula(x), "binomial", dattouse)
})


## Use the predictr function on each glm created and stored in the
## list glms with an lapply command.
predictions <- lapply(glms, predictr)


## Use the getpcp function to get the PCPs using the predictions
## created in predictr
pcps <- unlist(lapply(predictions, getpcp, realresults = mat[holdoutrows,devee]))

## Pick the model with the highest PCP
### Place the pcps in a framework a human being could read
pcps <- data.frame(desnoms= colnames(mat)[!colnames(mat) %in% c(devee, dontuse)], pcps)
print(pcps)

## Get the variable that has the highest pcp. I think this might be
## the slow way of doing it.
bstvar <- as.character(pcps[,1][pcps[,2] %in% max(pcps[,2])])
