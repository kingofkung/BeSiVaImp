## Begin analyzing GSS Data

source("GSSDatFix.R")
source("BeSiVaFunctions.R")


devee <- "vote12bin"
## colnames(dat)

for(i in 1:ncol(dat)) p

## Keep some columns from being used
avoidcols <- c("year", "id", "ballot", "version", "issp", "formwt", "sampcode", "sample", "phase", "spanself", "spanint", "spaneng", "vote12","wtss", "wtssnr", "wtssall", "vrstrat", "vpsu", "vote12bin" )

## Keep vote12, and the sample/weight info out of the data
colstouse <- colnames(dat)[!colnames(dat) %in% tolower(avoidcols)]

## Keep some data held out
## First, make sure our DV Is included in all cases rows
dat2 <- dat[complete.cases(dat$vote12bin),]
dat2$vote12bin


## get the sample of rows
set.seed(1234)
test <- sample(seq_along(dat2$vote12bin), size = round(nrow(dat2)/10))



## Placein will be where we store the variables we want included
placein <- ""

formulae <- lapply(colstouse, function(x) as.formula(paste0(devee, " ~ ",x, placein  )) )


glms <- lapply(formulae[9], function(x)
    try(
        glm(x, family = "binomial", data = dat2[-test, ])
        )
       )
## print(glms)

head(glms)

predictions <- lapply(glms, function(x)  try(predictr( x  , data = dat2, rowstouse = test)))


pcps <- lapply(predictions, function(x)  getpcp(x, dat2$vote12bin[test]) )
