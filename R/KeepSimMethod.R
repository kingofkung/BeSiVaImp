## Here's where we keep how we made the simulated data.
varsofint <- c("educ", "degree", "rincome", "wealth", "poleff11", "poleff18", "poleff19", "poleff20", "polinter", "news", "newsfrom", "polnews", "partyid", "grpparty", "mobile16", "marital", "racecen1","race", "region", "sex", "age" )

## dattouse <- dat[, c(varsofint, "vote12bin")]

## percMissing <- lapply(dattouse, function(x) sum(is.na(x))/nrow(dat))


## library(mice)

## for(i in 1:10){
## ## if it's the first iteration, I want dattouse to be what's iterated. Otherwise, I want recursion.
##     ifelse(i == 1, dturm <- dattouse, dturm <- dtu1rm)
##     dtu <- mice(dturm, m = 1, MaxNWts = 2000)
##     ## grab the dataset from dtu
##     dtu1 <- complete(dtu, 1)

##     ## proof of concept of the replacement with missing data
##     ## set.seed(1234)
##     ## mrows <- sample(1:nrow(dtu1), round(percMissing[["rincome"]] * nrow(dtu1)), replace = T)
##     ## length(mrows)
##     ## dtu1[mrows, "rincome"] <- NA

##     ## take the names from percent missing.
##     dtu1rm <- as.data.frame(lapply(names(percMissing), function(x){
##         ## Then, get a sample of the rows of the dataset
##         mrows <- sample(1:nrow(dtu1), round(percMissing[[x]] * nrow(dtu1)), replace = T)
##         ## Put them as NA
##         dtu1[mrows, x] <- NA
##         ## and output that column of data as the result of this little lapply function
##         dtu1[,x]
##     }))
##     ## fix the names
##     colnames(dtu1rm) <- colnames(dtu1)


## }
## write.csv(dtu1rm, "../Data/GSS_stata/simdat.csv")
