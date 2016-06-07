## Just Create a Toy Dataset

## Set the seed and select sample size
set.seed(123456789)
sampsize <- 25

## Make some fake variables
catmaker <- function(x, num = sampsize){letters[sample(x, num, replace = T)]}
binmaker <- function(num = sampsize, ntrial = 1, prob = .5){rbinom(num, ntrial, prob)}

cont1 <- rnorm(sampsize)
cont2 <- sample(1:100, sampsize)

cats <- sapply(rep(2:5,2), function(x) catmaker(1:x))
binars <- sapply(seq(0.1, .9, .1), function(x) binmaker(prob = x))
daf <- data.frame(cats, binars, cont1, cont2)

## Make my dependent variable
lc <- 27 + 45 * cont1 - 12 *daf[14]
pr <- (1 + exp(-lc))^-1
yvar <- rbinom(sampsize, 1, pr)

daf <- cbind(daf, yvar)


