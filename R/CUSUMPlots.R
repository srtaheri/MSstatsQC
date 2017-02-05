#' A Cumulative Sum (CUSUM) Function
#'
#' This function allows you to calculate CUSUM statistics.
#' @param data Comma-separated (*.csv), QC file format. It should contain a Precursor column and the metrics columns.
#' @param peptide The name of precursor you want to draw the plot for
#' @param L Lower bound of the giude set. Defaults to L = 1
#' @param U Upper bound of the guide set. Defaults to U = 5
#' @param ytitle The y-axis title of the plot.Defaults to "CUSUM Plot - mean". The x-axis title is by default "QCno-name of peptide"
#' @param type can take two values, "mean" or "dispersion". Defaults to "mean"
#' @keywords Cumulative Sum, control chart
#' @export
#' @import dplyr
#' @import plotly
#' @import RecordLinkage
#' @examples
#' CUSUMPlots()

#################################################################################################
CUSUMPlots<- function(data, peptide, L = 1, U = 5, metric, normalization = TRUE,  ytitle = "CUSUM Plot - mean", type = "mean") {
  #data <- input_checking(data)
  if(!is.data.frame(data)){
    stop(data)
  }
  CUSUM.outrange.thld <- 5
  metricData <- getMetricData(data, peptide, L, U, metric, normalization)
  plot.data <- CUSUM.data.prepare(data, metricData, peptide, L, U, type)

  x <- list(
    title =  paste("QCno - ", peptide),
    range = c(0, max(plot.data$QCno))
  )
  y <- list(
    title = ytitle
  )

  plot_ly(plot.data, x = ~QCno, y = ~CUSUM.poz,showlegend = FALSE)%>%
    add_lines(y = CUSUM.outrange.thld, color = I("red"), name = "CUSUM thld", showlegend = FALSE) %>%
    add_lines(y = -CUSUM.outrange.thld, color = I("red"), name = "CUSUM thld", showlegend = FALSE) %>%
    add_lines(x = ~QCno, y = ~CUSUM.poz, color = I("aquamarine"),name = "CUSUM+", showlegend = TRUE)%>%
    add_lines(x = ~QCno, y = ~CUSUM.neg, color = I("blue"), name = "CUSUM-",showlegend = TRUE)%>%
    add_markers(x = ~QCno, y = ~CUSUM.neg, color = ~outRangeInRangeNeg,colors = c("red","green"),showlegend = TRUE)%>%
    add_markers(x = ~QCno, y = ~CUSUM.poz, color = ~outRangeInRangePoz,colors = c("red","green"),showlegend = TRUE)%>%
    layout(xaxis = x,yaxis = y)


}
