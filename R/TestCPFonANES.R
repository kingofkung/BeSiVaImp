rm(list = ls())
anes2000 <- read.csv("/Users/bjr/GitHub/BeSiVaImp/Data/anes2000.csv")
source("/Users/bjr/GitHub/BeSiVaImp/R/RecodeANES2000.R")

dat <- anes2000

sampProp <- .25
rowsToHold <- sample(1:nrow(dat), round(nrow(dat) * sampProp))


facvars <- unlist(lapply(dat, is.factor))
facvarnames <- names(facvars[facvars])


##
## find variables highly likely to crash the code
lapply(anes2000[,facvars], function(x) length(unique(x)))

glm(bindep~vcf1023b + vcf1023b, data = anes2000, family = binomial)







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

anes2000 <- bettercpf(anes2000, rowsToHold, facvarnames)
