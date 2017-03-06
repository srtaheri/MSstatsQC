#' A Change Point (CP) Function
#'
#' This function allows you to calculate change point statistics for a step change in mean or variability.
#' @param data Comma-separated (*.csv), QC file format. It should contain a Precursor column and the metrics columns.
#' @param peptide The name of precursor you want to draw the plot for
#' @param L Lower bound of the guide set
#' @param U Upper bound of the guide set
#' @param metric QC metric to be monitored
#' @param normalization TRUE if QC metric is standardized and FALSE if not standardized
#' @param ytitle The y-axis title of the plot. Defaults to "Change Point Plot - mean". The x-axis title is by default "QCno-name of peptide"
#' @param type can take two values, "mean" or "dispersion".
#' @keywords Change point, control chart
#' @export
#' @import dplyr
#' @import plotly
#' @import RecordLinkage
#' @examples
#' ChangePointEstimator()
ChangePointEstimator <- function(data, peptide, L = 1, U = 5, metric, normalization = TRUE,  ytitle = "Change Point Plot - mean", type = "mean") {
  if(!is.data.frame(data)){
    stop(data)
  }
  metricData <- getMetricData(data, peptide, L, U, metric, normalization)
  precursor.data <- data[data$Precursor==peptide,]
  ## Create variables
  plot.data <- CP.data.prepare(data, metricData, type)

  x <- list(
    title = paste("QCno - ", peptide)
  )
  y <- list(
    title = ytitle
  )

  plot_ly(plot.data, x = ~QCno, y = ~Et,showlegend = FALSE)%>% #,text=precursor.data$Annotations)
    add_lines(x = ~tho.hat, color = I("red"))%>%
    add_lines(x = ~QCno, y = ~Et, color = I("cornflowerblue"))%>%
    add_markers(x = ~QCno, y = ~Et, color = I("blue"))%>%
    layout(xaxis = x,yaxis = y)
}
