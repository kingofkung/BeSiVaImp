## This code will allow us to, in one fell swoop, remove the missing
## data from the anes.  Right now, there are a series of codings that
## anes claims are missing, but are treated as categorical variables.

## Here's the script where we open the american national election survey data
rm(list = ls()[!ls() %in% "anes"])
loc <- "/Users/bjr/Dropbox/R_Projects/GSSThing/anes_timeseries_cdf_sav/"

library(foreign)
library(memisc)

## anes <- read.spss(paste0(loc,"anes_timseries_cdf.sav"), to.data.frame = T)
if(!exists("anes")){
    anes <- read.spss(paste0(loc,"anes_timseries_cdf.sav"))
    anes <- as.data.frame(anes)
    colnames(anes) <- tolower(colnames(anes))
}

anes$vcf0202[!is.na(anes$vcf0202)]

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

## Get missing codes beginning locations. The + 2 at the end should
## eliminate the first two rows (i.e. the MISSING_CODES and dashes).
missingbeg <- grep("MISSING_CODES", cb, T) + 2

## Figure out how to get all lines between missingbeg and the next blank line
## Write a function to get the next blank line after missingbeg


## note to you: You cannot use a vector of numbers as its index
## without seq_along. This should get us the first fully blank line
## after the missing codes line.
getmissingend <- function(x){
    missingbeg[x] + which(cb[missingbeg[x]+ 1:100] %in% "")[1] - 1
              }
missingend <- unlist(lapply(seq_along(missingbeg), getmissingend))

##Join missingbeg and missingend together in a single
missingind <- data.frame(missingbeg, missingend)


getthemissings <- function(x){
    cb[(missingind$missingbeg[x]):missingind$missingend[x]]
}

missinglist <- lapply(seq_along(missingind$missingbeg), getthemissings)

head(missinglist)

## So now we have a real problem... how can we guarantee which missing
## values correspond to which variable?


## Find which values before missingbeg have the divider

getvarmissings <- function(x, divrange = 200:1){
    missrange <- missingbeg[x] - divrange
    cb[missrange][ tail(which(cb[missrange] %in% divider), 1) + 1 ]
}
tst <- getvarmissings(4)

names(missinglist) <- lapply(seq_along(missingind$missingbeg), getvarmissings)
## missinglist


## remove the first set of missings from anes. The ^ means beginning of
## the line. Before we were getting a digit followed by a period just
## anywhere in the line, which was giving us trouble.
justcodes <- lapply(missinglist,function(x) grep("^\\d+\\.", x, value = T))


## This is how I find that first period and grab everything before it.
missingvals <- lapply(justcodes, function(avar){
    endpt <- unlist(regexec("\\.", avar)) -1
    as.numeric(substring(avar, 1, endpt))
})


## Good news: With the removal of the as.character command from the
## above section, we don't have to worry about different length
## missing values. This can be confirmed with the line below.
## which(unlist(lapply(missingvals, function(x) all(nchar(x) == nchar(x[1]))) == F) )

## So we'll just get that character length and use it when trying to
## get the length of the strings of our categorical variables.
strlen <- lapply(missingvals, function(x) nchar(x[1]))

x <- names(missingvals)[which( names(missingvals) == "vcf0120")]


anes2 <- anes
 for(x in names(missingvals))   anes[ substr(anes[,x], 1, strlen[[x]]) %in% missingvals[[x]], x] <- NA

anothervar <- names(missingvals)[sample(seq_along(missingvals), 1)]
table(anes2[, anothervar], anes[, anothervar], useNA = "always")

