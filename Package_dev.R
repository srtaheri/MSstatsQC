install.packages("devtools")
library("devtools")

devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("/Users/ed/GitHub")
create("MSstatsQC")
setwd("/Users/ed/GitHub/MSstatsQC")
document()

###############################################################
####### START FROM HERE########################################
setwd("/Users/ed/GitHub")
install("MSstatsQC", dep=TRUE)
library(MSstatsQC)
##############################################################
## Import datasets
setwd('/Users/ed/GitHub/msstats-qc/datasets')
S9Site54 <- read.csv('Study9.1-Site54.csv') 
setwd("/Users/ed/GitHub/MSstatsQC")
save(S9Site54, file="data/S9Site54.RData")
##############################################################
##http://r-pkgs.had.co.nz/check.html##########################
setwd("/Users/ed/GitHub/MSstatsQC")
devtools::check()
##############################################################