## Any functions that were created will be kept separate here

##' predictr
##'
##' make predictions for the glm models of BeSiVa
##' @title
##' @param x a glm model to make predictions
##' @param data # the entire dataset
##' @param rowstouse row information for the holdout set. Set to
##' @return the predictions: Usually vector of 0's and 1's, but this might change in future
##' @author Benjamin Rogers
predictr <- function(x, data = mat, rowstouse = holdoutrows){
    thepreds <- predict(x, newdata = data[rowstouse,], "response")
    ifelse(thepreds >=.5, 1, 0)
    ## unlist(lapply(thepreds, function(x) rbinom(1, size = 1, prob = x)))
}

##' getpcp
##'
##' Get percent correctly predicted (PCP) for the models
##' @title
##' @param preds
##' @param realresults
##' @param fullpreds Do you use all predictions, or just the ones that were possible to predict?
##' @return
##' @author Benjamin Rogers
getpcp <- function(preds, realresults, fullpreds = TRUE) {
    ifelse(fullpreds == TRUE , denom <- length(preds), denom <- length(na.omit(preds)))
    length(which(preds == realresults))/denom
}

## Function wishlist
## 1. Function that is BeSiVa (duh)

##' Display the full model and that with just training data
##'
##' @title dispboth
##' @param model The model to update
##' @param fulldata the data you'd like to use to estimate the model
##' @return a list that includes both regression models
##' @author Benjamin Rogers
dispboth <- function(model, fulldata){
    fullmod <- update(model, formula = .~. , data = fulldata)
    list(model, fullmod)
}
