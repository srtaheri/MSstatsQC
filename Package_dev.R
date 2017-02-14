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

S9Site54 <- read.csv('Study9.1-Site54.csv') 
setwd("/Users/ed/GitHub/MSstatsQC")
save(S9Site54, file="data/S9Site54.RData")
##############################################################
##http://r-pkgs.had.co.nz/check.html##########################
setwd("/Users/ed/GitHub/MSstatsQC")
devtools::check()
##############################################################
#####Shiny STats##############################################
library(rsconnect)
rsconnect::setAccountInfo(name='eralpdogu', token='D9DFDC81EEDC698CA24D6875AFB72D3F', secret='Rd+qnS6fYmojzqAJnzUAzmUIHVXU6R4LQVB0PySA')
df <- rsconnect::showMetrics("container.cpu",c("cpu.user"),server="shinyapps.io",account ='eralpdogu',appName = 'msstatsqc')
df<- rsconnect::showUsage(appDir = getwd(), appName = 'msstatsqc', account = 'eralpdogu',usageType = "hours",
               interval = '52w')
