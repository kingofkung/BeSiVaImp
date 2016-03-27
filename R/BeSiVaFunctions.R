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
    ifelse(fullpreds == TRUE , denom <- length(preds), denom <- length(realresults))
    length(which(preds == realresults))/denom
}
