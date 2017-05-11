#' A data processing function
#'
#' This function allows you to validate the dataset for further analysis
#' @param data Comma-separated (*.csv), QC file format.
#'  It should contain a Precursor column and the metrics columns.
#' @export
#' @import dplyr
#' @import RecordLinkage


#########################################################################################################

DataProcess <- function(data = NULL){
  if(is.null(data))
    return()
  ## save process output in each step #### creating a log file ########### from Meena's code
  allfiles <- list.files()

  num <- 0
  filenaming <- "./log/msstatsqc"
  finalfile <- "msstatsqc.log"

  while(is.element(finalfile,allfiles)) {
    num <- num+1
    finalfile <- paste(paste(filenaming,num,sep="-"),".log",sep="")
  }

  data <- input.sanity.check(data, finalfile)

  return(data)
}
