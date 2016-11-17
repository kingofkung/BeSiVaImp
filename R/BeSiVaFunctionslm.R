## Any functions that were created will be kept separate here


##' A Faster way of Eliminating problem Categorical variables
##'
##' .. content for \details{} ..
##' @title
##' @param dat The data with potential problem variables
##' @param holdoutRows how the variables are divided
##' @param facvarnames names of factors variables to check
##' @return the test data, sans problem variables
##' @author Benjamin Rogers
bettercpf <- function(dat, holdoutRows, facvarnames){
    goodfac <- lapply(seq_along(facvarnames), function(x, facnames = facvarnames, trdat = dat[-holdoutRows , ], tesdat = dat[holdoutRows , ]){
        ## Get the levels for the factor of choice
        fac <- facnames[x]
        trlvls <- unique(factor(trdat[,fac]))
        teslvls <- unique(factor(tesdat[,fac]))
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




getrmses <- function(model, datain, dvname, rowstouse, naremove = TRUE){

    facstat <- lapply(model.frame(model, data = datain), is.factor)
    facnames <- names(facstat[unlist(facstat)])
    rownums <- as.numeric(rownames(model.frame(model, data = datain)))

    smalldf <- datain[c(rownums, rowstouse),]
    tesrows <- seq_along(rowstouse) + length(rownums)
    smalldf <- bettercpf(smalldf, seq_along(rowstouse) + length(rownums), facnames)



    try(tstrmse <- sqrt(mean((datain[rowstouse, dvname] -
             predict(model, newdata = smalldf[ seq_along(rowstouse) + length(rownums),]))^2,
        na.rm = naremove)))
    ifelse(exists("tstrmse"), return(tstrmse), NA)

}

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
modmakerlm <- function(x, thedat, loud = FALSE){
    eval(bquote(
        try(junker <- (lm(.(x), data = model.frame(.(x), thedat), model = FALSE, y = FALSE)))
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
besivalm <- function(devee, ivs, dat, fam = binomial(), iters = 5, perc = .2, nfolds = 1, thresh = 1E-6, sampseed = 12345, showoutput = TRUE, showforms = TRUE){
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
            lms <- lapply(forms, modmakerlm, thedat = dat[-testrows,], loud = showforms)
            ## print(lapply(lms, class))
            rmses <- unlist(lapply(lms, function(x, dattmp = dat, dv = devee, tr = testrows){
                ifelse(class(x) == "lm",
                       yes = getrmses(x, data = dattmp, dvname = dv, tr),
                       no = NA)
            }))
            if(is.character(rmses)){
                rmses[rmses %in% c("Error in try(tstrmse) : object 'tstrmse' not found\n", "NaN")] <- NA
                rmses <- as.numeric(rmses)
            }
            print(sort(rmses))



            ## round to a given threshold, as per user preference.
            if(thresh != 0) rmses <- plyr::round_any(rmses, thresh)
            ## Here is where it would end. Basically we'd need to run
            ## it over the different folds of data.



            ## So we've got the formula that yields the best
            ## predictions. This is how we extract everything from that
            ## formula after the ~ sign.
            ## print(paste("min rmse ="))
            mincriter <- which(rmses %in% sort(rmses)[1])
            ## print(mincriter)
            ## So it turned out that if there was a tie, the code
            ## would return an error. To remedy this, I break out of
            ## the for loop if we get more than 1 with a maximum pcp.
            if(length(mincriter) > 1) {
                if(i == 1) oldrmses <- rmses
                tieforms <- forms[mincriter]
                if(showoutput == TRUE) print(paste("We have a tie between: ", paste(tieforms, sep = " \n "), "", sep = ""))
                break} else tieforms <- NA
            ## print(maxpcp)
            vars <- as.character(forms[[mincriter]]) [3]
            if(showoutput == TRUE) {
                print(vars)
                print(i)
            }
            ## by saving the pcps here, if the loop breaks first, then
            ## the old pcps are saved, without having them be rewritten
            oldrmses <- rmses
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
        if(length(mincriter) > 1) rmses <- oldrmses
        list("intvars" = intvars, "tieforms" = tieforms, "forms" = forms, "lms" = lms, "rmses" = rmses, "tstrows" = testrows)
}
