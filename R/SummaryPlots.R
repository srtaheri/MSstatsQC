#' A river plot to summarize results from XmR or CUSUM plots
#'
#' This function allows you to draw the XmR or CUSUM summary plot.
#' @param data Comma-separated (*.csv), QC file format. It should contain a
#'  Precursor column and the metrics columns.
#' @param L Lower bound of the giude set. Default is 1.
#' @param U Upper bound of the guide set. Default is 5.
#' @param method It is either "CUSUM" or "XmR"
#' @param listMean List of the means for the metrics.
#' If you don't know the means leave it as NULL and they will be calculated automatically
#'  by using L and U. The default is NULL.
#' @param listSD List of the standard deviations for the metrics.
#'  If you don't know the standard deviations leave it as NULL and they will be calculated automatically
#'   by using L and U. The default is NULL.
#' @keywords XmR
#'           CUSUM
#'           Summary plot
#' @export
#' @import ggplot2
#' @import RecordLinkage
#' @import grid

RiverPlots <- function(data = NULL, L=1, U=5, method = "XmR", listMean=NULL, listSD=NULL) {

  if(method == "XmR") {
    gg <- SummaryPlot(data , L , U , method = "XmR",
                            listMean=NULL, listSD=NULL)
      gg <- gg + ggtitle("Overall Summary \nXmR")
      gg
  }

  else if(method == "CUSUM") {
    gg <- SummaryPlot(data , L , U , method = "CUSUM",
                                  listMean=NULL, listSD=NULL)
    gg <- gg + ggtitle("Overall Summary \nCUSUM")
    gg
  }

}

