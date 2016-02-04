## Make a really easy data set to play with BeSiVa


set.seed(123)

x1 <- rnorm(1000)
x2 <- rnorm(1000)

b0 <- 4
b1 <- 9
b2 <- -2
tee <- b0 + b1 *x1 + b2 * x2

exp(-tee)

pr <- 1/(1 + exp(-1*tee))

dv <- rbinom(1000, 1, pr)

glm(dv ~ x1 + x2, "binomial")

## Simluate some data
matrows <- 1000
mat <- matrix(NA, nrow = matrows, ncol = 30)
mat <- apply(mat, 2, function(x) {rnorm(matrows)})
mat <- as.data.frame(mat)

## Subset the data

## holdoutrows <- sample(1:matrows, round(matrows/10))

## Nice and simple subset, to keep me same
holdoutrows <- 900:1000


mat[,1] <- dv
colnames(mat)[1] <- "DV"

answers <- sample(2:ncol(mat), size = 2)
mat[, answers] <- c(x1, x2)
dontuse <- c("V5", "V9")

## Make text versions of formulae, but don't make one of the dv
formulae <- lapply(colnames(mat), function(x, dvname = "DV", excludr = dontuse) {
    if(dvname != x & !x %in% excludr) paste0(dvname, " ~ ", x)
}
       )
## Get rid of any formulae that is null
formulae <- unlist(formulae)

## Make the glm regressions and store them in glms
glms <- lapply(formulae, function(x, dattouse = mat[-holdoutrows, ]){glm( as.formula(x), "binomial", dattouse) })

## How to get the formula out
glms[[1]]$formula

round(predict(glms[["V16"]], newdata = mat[holdoutrows,], "response"), 2)

lapply(1:length(glms), function(x) predict(glms[[x]], newdata = mat[holdoutrows,]) )
