## Make a really easy data set to play with BeSiVa
rm(list = ls())

set.seed(123)
matrows <- 1000

x1 <- rbinom(1000, 1, .5)
x2 <- rgamma(1000, 3, .5)

b0 <- 4
b1 <- 9
b2 <- -2
tee <- b0 + b1 *x1 + b2 * x2

exp(-tee)

pr <- 1/(1 + exp(-1*tee))

dv <- rbinom(1000, 1, pr)

##The answer we're looking for
summary(glm(dv ~ x1 + x2, "binomial"))

## Simluate some data
mat <- matrix(NA, nrow = matrows, ncol = 30)

## Fill the rows I don't care about with noise
mat <- apply(mat, 2,
             function(x) {
                 rnorm(1000, 0, 100)
             })

mat <- as.data.frame(mat)

##Place in the DV, and call it "DV"
mat[,1] <- dv
colnames(mat)[1] <- "DV"

answers <- sample(2:ncol(mat), size = 2)
mat[, answers] <- c(x1, x2)
colnames(mat)[answers] <- c("x1", "x2")
