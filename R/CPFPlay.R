## Start working on improvements to catprobfinder
## make some data
rm(list = ls())
set.seed(1234)
x1 <- rnorm(100, 0, 1)
x2 <- rbinom(100, 2, .5)
## Remember, rbinom can go to 0, while the letters can go to
x3num <- 3
x3 <- factor(letters[1 + rbinom(100, x3num, .2)])
x3dums <- dummies::dummy(x3)

x4num <- 5
x4 <- factor(letters[1 + rbinom(100, x4num, .2)])
x4dums <- dummies::dummy(x4)


b0 <- 0
b1 <- 5
b2 <- 3
b3vec <- sample(-5:5, size = x3num)
b4vec <- sample(-5:5, size = x4num - 1)


yp <- b0 + b1 * x1 + b2 * x2 + rowSums(x3dums[, -1] %*% diag(b3vec))+ rowSums(x4dums[, -1] %*% diag(b4vec))

pr <- 1/(1 + exp(-yp))

ytrain <- rbinom(length(yp), size = 1, pr)

traindat <- data.frame(ytrain, x1, x2,  x3, x4)

m1 <- glm(ytrain ~ x1 + x2 + x3 + x4, binomial, data = traindat, model = F, y = F)
format(object.size(m1), "Kb")

m1$data <- NULL
m1$y <- NULL
m1$linear.predictors <- NULL
m1$weights <- NULL
m1$fitted.values <- NULL
m1$model <- NULL
m1$prior.weights <- NULL
m1$residuals <- NULL
m1$effects <- NULL
m1$qr$qr <- NULL

format(object.size(m1), "Kb")


x12pred <- rnorm(100, 0, 1)
x22pred <- rbinom(100, 2, .5)
x32pred <- factor(letters[1 + rbinom(100, x3num + 5, .2)])
x42pred <- factor(letters[1 + rbinom(100, x4num + 5, .2)])

pdat <- data.frame("x1" = x12pred, "x2" = x22pred, "x3" = x32pred, "x4" = x42pred)

## Imagine that we had the data inside a locked box. What kind of
## function would we write to get it out?
facvars <- unlist(lapply(pdat, is.factor))
facvarnames <- names(facvars[facvars])

## i <- 2
## plvls <- levels(pdat[, facvarnames[i]])
## tlvls <- levels(traindat[, facvarnames[i]])
## badlvlbool <- !plvls %in% tlvls
## badlvls <- plvls[badlvlbool]
## pdat[pdat[,facvarnames[i]] %in% badlvls  ,facvarnames[i]] <- NA
## pdat


goodfac <- lapply(seq_along(facvarnames), function(x, facnames = facvarnames, trdat = traindat, tesdat = pdat){
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
## and plug it in
pdat[, colnames(goodfac)] <- goodfac





predict(m1, newdata = pdat)
