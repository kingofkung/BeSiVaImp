## This code will allow us to, in one fell swoop, remove the missing
## data from the anes.  Right now, there are a series of codings that
## anes claims are missing, but are treated as categorical variables.

## The goal is to get the missing data categories into a spreadsheet, so we can easily recode the missings as such.

codeloc <- "/Users/bjr/GitHub/BeSiVaImp/Data/anes_timeseries_cdf_codebook/"

cb <- readLines(paste0(codeloc, "anes_timeseries_cdf_codebook_var.txt"))
head(cb, 100)

cb <- gsub("VCF", "vcf", cb)

divider <- "============================================================================="

divlocations <- grep(divider, cb)
varnames <- unique(cb[ divlocations + 1])
## get rid of the value that's all spaces
varnames <- varnames[-grep("\\s+", varnames)]

missingbeg <- grep("MISSING_CODES", cb, T)

cb[]
## Figure out how to get all lines between missingbeg and the next blank line
cb[missingbeg[700] + 1:10]

## Write a function to get the next blank line after missingbeg


## note to you: You cannot use a vector of numbers as its index without seq_along
missingend <- unlist(lapply(seq_along(missingbeg), function(x){
    missingbeg[x] + which(cb[missingbeg[x]+ 1:100] %in% "")[1]
              }))

cb[missingend]
missingind <- data.frame(missingbeg, missingend)


getthemissings <- function(x){
    cb[missingind$missingbeg[x]:missingind$missingend[x]]
}

lapply(seq_along(missingind$missingbeg), getthemissings)


####

allvcfs <- "vcf\\d+[a-z]*"
findme <- paste0(divider, "[\n]", allvcfs)


varrxp <- regexpr(findme, cb)
vnames1 <- cb[varrxp==1]
vnames1[grep("[:/:]", vnames1, invert = T)]

cb[grep("MISSING_CODES", cb, T)]

anes$vcf0012a
unique(anes$vcf0873)

