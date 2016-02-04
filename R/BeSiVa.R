## Subset the data

## holdoutrows <- sample(1:matrows, round(matrows/10))

## Nice and simple subset, to keep me same
holdoutrows <- 900:1000



##Keep these two rows out. Why? So we can keep that here for when we
##actually want to exclude variables

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
