## Any functions that were created will be kept separate here


##' This function gets rid of everything that can slow down a glm
##' model, without breaking predict.
##' Based on the notes at
##' http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/
##'
##' @title Removes Unnecessary elements of glm Models
##' @param mod a glm model
##' @return just enough of the glm model to make predictions
##' @author Nina Zumel
glmnullifier <- function(mod){
    mod$data <- NULL
    mod$y <- NULL
    mod$linear.predictors <- NULL
    mod$weights <- NULL
    mod$fitted.values <- NULL
    mod$model <- NULL
    mod$prior.weights <- NULL
    mod$residuals <- NULL
    mod$effects <- NULL
    mod$qr$qr <- NULL
##
    mod$family$variance <- NULL
    mod$family$def.resids <- NULL
    mod$family$aic <- NULL
    mod$family$validmu <- NULL
    mod$family$simulate <- NULL
##
##
    mod
}


##' A Faster way of Eliminating problem Categorical variables
##'
##' .. content for \details{} ..
##' @title
##' @param dat The data with potential problem variables
##' @param holdoutRows how the variables are divided
##' @param facvarnames names of factors variables to check
##' @return the data, sans problem variables
##' @author Benjamin Rogers
bettercpf <- function(dat, holdoutRows, facvarnames){
    goodfac <- lapply(seq_along(facvarnames), function(x, facnames = facvarnames, trdat = dat[-holdoutRows , ], tesdat = dat[holdoutRows , ]){
        ## Get the levels for the factor of choice
        fac <- facnames[x]
        trlvls <- levels(trdat[,fac])
        teslvls <- levels(tesdat[,fac])
        ## Figure out which levels are/aren't in the training data.
        ## These are bad levels, as they screw with our ability to predict
        badlvlbool <- !teslvls %in% trlvls
        badlvls <- teslvls[badlvlbool]
        ## Remove the bad levels, and return the column without the bad levels
        tesdat[tesdat[, fac] %in% badlvls, fac] <- NA
        tesdat[ , fac]
    })
    ## make sure the names match the data
    names(goodfac) <- facvarnames
    goodfac <- as.data.frame(goodfac)
    dat[holdoutRows, colnames(goodfac)] <- goodfac
    dat
}


##' findnew takes two lists, testlist and modlist, and sees
##' whether there is a difference in the categories in the two lists.
##' @title findnew
##' @param r the list element to be considered. An integer
##' @param testlist a list of unique elements in categorical variables in the test set
##' @param modlist a list of unique elements in categorical variables in the model data
##' @return a list of new elements in each list element
##' @author Benjamin Rogers
findnew <- function(r, testlist = testuniques , modlist = moduniques){
    if(is.factor(testlist[[r]])){
        jn <- testlist[[r]][ !testlist[[r]] %in% modlist[[r]] ]
        jn <- jn[!is.na(jn)]
    } else jn <- 0

    jn
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title catprobfinder
##' @param nx the glm model we'll be using
##' @param data The dataset from the models
##' @param testrows The rows designating the test set
##' @return the test data set with the new categories set as NA's,
##'     ready to be plugged into the predict function in predictr
##' @author Benjamin Rogers
catprobfinder <- function(nx, data, testrows){

    ivsused <- all.vars(formula(nx))[-1]
    ## print(ivsused)

    tdat <- data[testrows, ivsused, drop = FALSE]
    testuniques <- lapply(tdat, function(x) unique(na.omit(x)))

    mdat <- model.frame(nx)[, -1, drop = FALSE]
    moduniques <- lapply(mdat, function(x) unique(na.omit(x)))

    ## newlvls <- lapply(1:length(testuniques), findnew, testuniques, moduniques)
    newlvls <-lapply(1:length(testuniques),
                     function(x)  testuniques[[x]][!testuniques[[x]] %in% moduniques[[x]]] )


    nNewcats <- sum(unlist(lapply(newlvls, length)))

    if(nNewcats > 0){
        tdatnu <- data.frame(lapply(seq_along(newlvls), function(i){
            tdat[tdat[,i] %in% newlvls[[i]], i] <- NA
            tdat[,i]}
            ))
        colnames(tdatnu) <- ivsused

    } else tdatnu <- NULL

    list("newlevels" = newlvls, "numnewcats" = nNewcats,
         "muniques" = moduniques, "tuniques" = testuniques,
         "tstdatnu" = tdatnu)

}


##' predictr
##'
##' make predictions for the glm models of BeSiVa
##' @title
##' @param x a glm model to make predictions
##' @param data # the entire dataset
##' @param rowstouse row information for the holdout set. Set to
##' @param loud If loud is true, then print the formulae. Should be based on showoutput
##' @return the predictions: Usually vector of 0's and 1's, but this might change in future
##' @author Benjamin Rogers
predictr <- function(x, data = mat, rowstouse = holdoutrows, loud = TRUE){

    ## So right here: Somewhere between where the data in newdata
    ## comes in and the predict function is where we could place our
    ## variables
    if(loud == T) print(formula(x))

    ## tryCatch(cpf <- catprobfinder(nx = x, data, rowstouse ), warning = "nu")
    ## if(!is.null(cpf$tstdatnu)){
    ##     thepreds <- predict(x, newdata = cpf$tstdatnu, "response")
    ## } else thepreds <- predict(x, newdata = data[rowstouse, , drop = FALSE], "response")
    thepreds <- predict(x, newdata = data[rowstouse, , drop = FALSE], "response")
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
    out <- length(which(preds == realresults))/denom
    out

}

## Function wishlist

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


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @param thedat
##' @param famille
##' @param loud
##' @return
##' @author Benjamin Rogers
modmaker <- function(x, thedat, famille = binomial(), loud = TRUE){
    eval(bquote(
        try(junker <- glmnullifier(glm(.(x), data = model.frame(.(x), thedat), family = famille, model = FALSE, y = FALSE)))
    ))
    if(loud == TRUE) eval(bquote(print(.(x))))

    try(junker)
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
##' @param showoutput do I want to show the output from the algorithm or not?
##' @param showforms Do I want to see the formulae on screen, or not? Best to see them if I've got a lot of variables, but not if there's only a few.
##' @return  the IVs of the best model based on subset selection, as well as the percent correctly predicted by that model.
##' @author Benjamin Rogers
besiva <- function(devee, ivs, dat, fam = binomial(), iters = 5, perc = .2, nfolds = 1, thresh = 1E-6, sampseed = 12345, showoutput = TRUE, showforms = TRUE){
        set.seed(sampseed)
        ## divy up data
        dat <- dat[,c(devee, ivs)]
        testrows <- sample(nrow(dat), round(nrow(dat)* perc))

        ## calculate appropriate number of digits to round to
        ## if(thresh != 0) digs = -1 * log(thresh, base = 10)
        ## get rid of any variable categories that might be a problem
        facvars <- unlist(lapply(dat, is.factor))
        factorVars <- names(facvars[facvars])
        dat <- bettercpf(dat, testrows, facvarnames = factorVars)

        for(i in 1:iters){
            ## Make some formulas
            ## Set vars as blank if i == 1
            if(i == 1) vars <- ""
            ## make it clear which variables are no longer considered
            outvars <- unlist(strsplit(vars, "\\s[+]\\s"))


            forms <- lapply(ivs[!ivs %in% outvars], function(x, deev = devee, invars = vars){
                if(vars == ""){ as.formula(paste(deev, "~", x))}
                else {as.formula(paste(deev, "~", x, "+", vars))}

            })
            ##

            ## Here is where the k-fold cross-validation would need to begin
            ## issue: when attempting to run speedglm, it doesn't recognize the data
            ## modmaker makes glms according to our specifications
            glms <- lapply(forms, modmaker, thedat = dat[-testrows,], loud = showforms)


            predvals <- lapply(glms,
                               function(x) try(predictr(x,
                                                    data = dat, rowstouse = testrows, loud = showforms)))
            pcps <- sapply(predvals, function(x) try(getpcp(x, dat[testrows, devee])))

            ## round to a given threshold, as per user preference.
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
            if(length(maxpcp) > 1) {
                if(i == 1) oldpcps <- pcps
                tieforms <- forms[maxpcp]
                if(showoutput == TRUE) print(paste("We have a tie between: ", paste(tieforms, sep = " \n "), "", sep = ""))
                break} else tieforms <- NA
            ## print(maxpcp)
            vars <- as.character(forms[[maxpcp]]) [3]
            if(showoutput == TRUE) {
                print(vars)
                print(i)
            }
            ## by saving the pcps here, if the loop breaks first, then
            ## the old pcps are saved, without having them be rewritten
            oldpcps <- pcps
        }

        ## What do we output?
        ## The sorted percents correctly predicted
        ## print(sort(pcps), digits = 10)
        ## print(predvals)
        ## This one gives the list of variables
        intvars <- strsplit( vars, split = "\\s[+]\\s")
        if(showoutput == TRUE) print(intvars)
        ## So when we end the loop, there should only be one set of
        ## PCPs that are output at any time. This makes sure that the
        ## one set is the last one before the tie, if there is one.
        if(length(maxpcp) > 1) pcps <- oldpcps
        list("intvars" = intvars, "tieforms" = tieforms, "forms" = forms, "glms" = glms, "predvals" = predvals, "pcps" = pcps, "tstrows" = testrows)
}
