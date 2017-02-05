#' An Individual (X) and Moving Range (mR) Function
#'
#' This function allows you to calculate XmR statistics for a step change.
#' @param data Comma-separated (*.csv), QC file format. It should contain a Precursor column and the metrics columns.
#' @param peptide The name of precursor you want to draw the plot for
#' @param L Lower bound of the giude set
#' @param U Upper bound of the guide set
#' @param ytitle The y-axis title of the plot. The x-axis title is by default "QCno-name of peptide"
#' @param type can take two values, "mean" or "dispersion".
#' @keywords XmR
#'          control chart
#' @export
#' @import dplyr
#' @import plotly
#' @import RecordLinkage
#' @examples
#' XmRPlots()
#########################################################################################################################
XmRPlots <- function(data, peptide, L = 1, U = 5, metric, normalization = FALSE,  ytitle = "XmR Plot - mean", type = "mean") {
  #data <- input_checking(data)
  if(!is.data.frame(data)){
    stop(data)
  }
  metricData <- getMetricData(data, peptide, L, U, metric, normalization)
  precursor.data <- data[data$Precursor==peptide,]
  plot.data <- XmR.data.prepare(data, metricData, L, U, type)

  x <- list(
    title = paste("QCno - ", peptide)
  )
  y <- list(
    title = ytitle
  )

  plot_ly(plot.data, x = ~QCno, y = ~t,showlegend = FALSE) %>%
    add_lines(y = ~LCL, color = I("red"), name = "LCL") %>%
    add_lines(y = ~UCL, color = I("red"), name = "UCL") %>%
    add_lines(x = ~QCno, y = ~t, color = I("blue")) %>%
    #add_markers(color= ~InRangeOutRange, colours=c("blue","red")) %>%
    add_trace(x = ~QCno, y = ~t, color = ~InRangeOutRange, type="scatter", mode="markers", colors = c("blue","red"), inherit=FALSE)%>%
    layout(xaxis = x,yaxis = y)

}
