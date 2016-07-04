## This code will allow us to, in one fell swoop, remove the missing
## data from the anes.  Right now, there are a series of codings that
## anes claims are missing, but are treated as categorical variables.

## The goal is to get the missing data categories into a spreadsheet, so we can easily recode the missings as such.

codeloc <- "/Users/bjr/GitHub/BeSiVaImp/Data/anes_timeseries_cdf_codebook/"

cb <- readLines(paste0(codeloc, "anes_timeseries_cdf_codebook_var.txt"))
head(cb, 100)

## Make variable names match the data in the .sav file
cb <- gsub("VCF", "vcf", cb)

## This is the divider between lines
divider <- "============================================================================="

divlocations <- grep(divider, cb)

varnames <- unique(cb[ divlocations + 1])
## get rid of the value that's all spaces
varnames <- varnames[-grep("\\s+", varnames)]

## Get missing codes beginning locations
missingbeg <- grep("MISSING_CODES", cb, T)

## Figure out how to get all lines between missingbeg and the next blank line
## Write a function to get the next blank line after missingbeg


## note to you: You cannot use a vector of numbers as its index
## without seq_along. This should get us the first fully blank line
## after the missing codes line.
getmissingend <- function(x){
    missingbeg[x] + which(cb[missingbeg[x]+ 1:100] %in% "")[1]
              }
missingend <- unlist(lapply(seq_along(missingbeg), getmissingend))

##Join missingbeg and missingend together in a single
missingind <- data.frame(missingbeg, missingend)


getthemissings <- function(x){
    cb[missingind$missingbeg[x]:missingind$missingend[x]]
}

missinglist <- lapply(seq_along(missingind$missingbeg), getthemissings)

## So now we have a real problem... how can we guarantee which missing
## values correspond to which variable?

divrange <- 200:1
missrange <- missingbeg[2] - divrange

## Find which values before missingbeg have the divider

getvarmissings <- function(x, divrange = 200:1){
    missrange <- missingbeg[x] - divrange
    cb[missrange][ tail(which(cb[missrange] %in% divider), 1) + 1 ]
}
getvarmissings(34)
names(missinglist) <- lapply(seq_along(missingind$missingbeg), getvarmissings)
missinglist
