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
options(devtools.desc.author="'First Last <first.last@example.com> [aut, cre]'")
install("MSstatsQC", dep=TRUE)
library(MSstatsQC)

setwd('/Users/ed/GitHub/msstats-qc/datasets')
#################
## Import datasets
CPTAC.Study9.1.Site54 <- read.csv('Study9.1-Site54.csv') 
save(CPTAC.Study9.1.Site54 , file="data/CPTAC.Study9.1.Site54.RData")
