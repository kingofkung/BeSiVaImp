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

##' the besiva function
##'
##' this function will take data in a given dataset, and return a
##' model based on subset selection.
##' @title besiva
##' @param devee The dependent variable of interest. Must be specified as 0-1
##' @param ivs The list of independent variables. Currently, must be text
##' @param dat The data to be considered. MUST CONTAIN DEVEE AND IVS!
##' @param fam The family of GLM To use. Currently stays at binomial
##' @param iters Number of iterations
##' @param perc ## The percentage of the data going into the test set Must be specified as between 0 and 1
##' @param nfolds ## number of folds of the data. For implementing k-fold cross validation
##' @param sampseed The seed for set.seed. Set, but could change as desired
##' @return
##' @author Benjamin Rogers
besiva <- function(devee, ivs, dat, fam = "binomial", iters = 1, perc = .2, nfolds = 1, sampseed = 12345){
        set.seed(sampseed)
        ## divy up data
        testrows <- sample(nrow(dat), round(nrow(dat)* perc))

        for(i in 1:iters){
            ## Make some formulas
            ## Set vars as blank if i == 1
            if(i == 1) vars <- ""


            forms <- lapply(ivs, function(x, deev = devee, invars = vars){
                if(vars == ""){ as.formula(paste(deev, "~", x))}
                else {as.formula(paste(deev, "~", x, "+", vars))}

            })
            ## Run the formulas

            glms <- lapply(forms,
                           function(x, thedat = dat[-testrows, ], famille = fam){
                                   try( glm(x
                                          , data = thedat, family = famille))

                           }
                           )
            predvals <- lapply(glms,
                               function(x) predictr(x,
                                                    data = dat, rowstouse = testrows))
            pcps <- sapply(predvals, function(x) getpcp(x, dat[testrows, devee]))

            ## So we've got the formula that yields the best
            ## predictions. This is how we extract everything from that
            ## formula after the ~ sign.
            maxpcp <- which(pcps == max(pcps))
            ## So it turned out that if there was a tie, the code
            ## would return an error. To remedy this, I break out of
            ## the for loop if we get more than 1 with a maximum pcp.
            if(length(maxpcp)>1) break
            ## print(maxpcp)
        vars <- as.character(forms[[maxpcp]]) [3]
        }

        ## What do we output?
        ## The sorted percents correctly predicted
        ## print(sort(pcps))
        ## This one gives the list of variables
        ## strsplit( vars, split = "\\s[+]\\s")

        ## glm(as.formula(paste0(devee, "~", vars)), data = dat)
}
