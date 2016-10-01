## Start working on improvements to catprobfinder
## make some data
rm(list = ls())
set.seed(1234)
x1 <- rnorm(100, 0, 1)
x2 <- rbinom(100, 2, .5)
## Remember, rbinom can go to 0, while the letters can go to
x3 <- factor(letters[1 + rbinom(100, 3, .2)])
x3dums <- dummies::dummy(x3)

b0 <- 0
b1 <- 5
b2 <- 3
b3vec <- c( 1, -5, -1)


yp <- b0 + b1 * x1 + b2 * x2 + x3dums[, -1] %*% diag(b3vec)

pr <- 1/(1 + exp(-yp))

ytrain <- rbinom(length(yp), size = 1, pr)

traindat <- data.frame(ytrain, x1, x2,  x3)

m1 <- glm(ytrain ~ x1 + x2 + x3 , binomial, data = traindat)

x12pred <- rnorm(100, 0, 1)
x22pred <- rbinom(100, 2, .5)
x32pred <- factor(letters[1 + rbinom(100, 8, .2)])
pdat <- data.frame("x1" = x12pred, "x2" = x22pred, "x3" = x32pred)

 !levels(pdat$x3) %in% levels(traindat$x3)
predict(m1, newdata = pdat)
