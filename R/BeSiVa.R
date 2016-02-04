## Subset the data

holdoutrows <- sample(1:matrows, round(matrows/10))

## Nice and simple subset, to keep me same
## holdoutrows <- 901:1000



##Keep these two rows out. Why? So we can keep that here for when we
##actually want to exclude variables

dontuse <- c("V5", "V9")
devee <- "DV"
## Make text versions of formulae, but don't make one of with dv or the variables in dontuse on the RHS
formulae <- lapply(colnames(mat), function(x, dvname = devee, excludr = dontuse) {
    if(dvname != x & !x %in% excludr) paste0(dvname, " ~ ", x)
}
       )
## Turn the list into a vector, and get rid of any nulls in one easy unlist.
formulae <- unlist(formulae)

## Make the glm regressions (taking care to not use the held out data) and store them in the variable glms
glms <- lapply(formulae, function(x, dattouse = mat[-holdoutrows, ]){
    glm( as.formula(x), "binomial", dattouse)
})

## How to get the formula out
glms[[1]]$formula

## make the predictions
predictr <- function(x, data = mat, rowstouse = holdoutrows){
    ifelse(predict(x, newdata = data[rowstouse,], "response") >=.5, 1, 0)
}

predict(glms[[1]], "response", newdata = mat[holdoutrows,])

predictions <- lapply(glms, predictr)

head(glms[[1]]$data)
## Get pcp

preds <- predictions[[1]]
realresults <- mat[holdoutrows, devee]


## Get percent correctly predicted for the
getpcp <- function(preds, realresults) length(which(preds == realresults))/length(preds)

pcps <- unlist(lapply( predictions, getpcp, realresults = mat[holdoutrows,devee]))
data.frame(desnoms= colnames(mat)[!colnames(mat) %in% c("DV", dontuse)], pcps)
