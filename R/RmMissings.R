## This code will allow us to, in one fell swoop, remove the missing
## data from the anes.  Right now, there are a series of codings that
## anes claims are missing, but are treated as categorical variables.

## The goal is to get the missing data categories into a spreadsheet, so we can easily recode the missings as such.

codeloc <- "/Users/bjr/GitHub/BeSiVaImp/Data/anes_timeseries_cdf_codebook/"

cb <- readLines(paste0(codeloc, "anes_timeseries_cdf_codebook_var.txt"))
head(cb, 100)

cb <- gsub("VCF", "vcf", cb)

divider <- "============================================================================="

cb[grep(divider, cb)]

cb[grep("MISSING_CODES", cb, T)]

anes$vcf0012a
unique(anes$vcf0873)

