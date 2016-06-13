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

##' Proposed function: folder/foldmaker.
##' The function will take the DV, IV(s), data, and number of folds you'd like, and return the mean/median pcps for each fold

foldmaker <- function(foldnum = 3){}


##' the besiva function
##'
##' this function will take data in a given dataset, and return
##' @title besiva
##' @param devee The dependent variable of interest. Must be specified as 0-1
##' @param ivs The list of independent variables. Currently, must be text
##' @param dat The data to be considered. MUST CONTAIN DEVEE AND IVS!
##' @param fam The family of GLM To use. Currently stays at binomial
##' @param iters Number of iterations
##' @param perc ## The percentage of the data going into the test set Must be specified as between 0 and 1
##' @param nfolds ## number of folds of the data. For implementing k-fold cross validation
##' @param thresh The threshold by an improvement must be made to be considered important. Currently quite small
##' @param sampseed The seed for set.seed. Set, but could change as desired
##' @return  the IVs of the best model based on subset selection, as well as the percent correctly predicted by that model.
##' @author Benjamin Rogers
besiva <- function(devee, ivs, dat, fam = "binomial", iters = 1, perc = .2, nfolds = 1, thresh = 1E-6, sampseed = 12345){
        set.seed(sampseed)
        ## divy up data
        testrows <- sample(nrow(dat), round(nrow(dat)* perc))

        ## calculate appropriate number of digits to round to
        ## if(thresh != 0) digs = -1 * log(thresh, base = 10)


        for(i in 1:iters){
            ## Make some formulas
            ## Set vars as blank if i == 1
            if(i == 1) vars <- ""


            forms <- lapply(ivs, function(x, deev = devee, invars = vars){
                if(vars == ""){ as.formula(paste(deev, "~", x))}
                else {as.formula(paste(deev, "~", x, "+", vars))}

            })
            ##

            ## Here is where the k-fold cross-validation would need to begin
            ## Problem: We'd need to create rows for each fold.
            ## At this point, testrows is a single variable. Would we want it as a list? A vector?
            ## Basically it'd be a getpcp outside of the current getpcp function.
            glms <- lapply(forms,
                           function(x, thedat = dat[-testrows, ], famille = fam){
                                   try(junker <- glm(x
                                          , data = thedat, family = famille))
                                   ## try(print(summary(junker)))
                                   ## try(junker)

                           }
                           )
            predvals <- lapply(glms,
                               function(x) try(predictr(x,
                                                    data = dat, rowstouse = testrows)))
            pcps <- sapply(predvals, function(x) try(getpcp(x, dat[testrows, devee])))
            ## While I had the round command here, round_any doesn't
            ## look that much slower, and it does exactly what I'm
            ## looking for without needing to resort to any kind of
            ## trick. I suppose I could use what's inside, which is just
            ## round(pcps/thresh) * thresh, but we'll see if that's
            ## really necessary when stress testing.
            if(thresh != 0) pcps <- plyr::round_any(pcps, thresh)
            ## Here is where it would end. Basically we'd need to run
            ## it over the different folds of data.



            ## So we've got the formula that yields the best
            ## predictions. This is how we extract everything from that
            ## formula after the ~ sign.
            maxpcp <- which(pcps == max(pcps))
            ## So it turned out that if there was a tie, the code
            ## would return an error. To remedy this, I break out of
            ## the for loop if we get more than 1 with a maximum pcp.
            if(length(maxpcp)>1) {
                print(paste("We have a tie between ", forms[maxpcp], "!", sep = ""))
                break}
            ## print(maxpcp)
        vars <- as.character(forms[[maxpcp]]) [3]
        }

        ## What do we output?
        ## The sorted percents correctly predicted
        ## print(sort(pcps), digits = 10)
        ## print(predvals)
        ## This one gives the list of variables
        print( strsplit( vars, split = "\\s[+]\\s"))
        ## glms
        ## glm(as.formula(paste0(devee, "~", vars)), data = dat)
        ## strsplit( vars, split = "\\s[+]\\s")
        predvals
}
