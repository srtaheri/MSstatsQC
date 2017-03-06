#' A function to create individual (X) and moving range (mR) control charts for QC metrics
#'
#' This function allows you to calculate X and mR statistics and draw control charts for certain QC metrics and peptides.
#'
#' @param data Comma-separated (*.csv), QC metric file. It should contain a "Precursor" column and the "QC metrics" columns.
#' It can also include "Annotations" for each observation.
#' @param peptide The name of precursor you want to draw the plot for.
#' @param L Lower bound of the guide set.
#' @param U Upper bound of the guide set.
#' @param metric The name of the QC metric. For example it can be "BestRetentionTime"
#' @param normalization TRUE if data is standardized.
#' @param ytitle The y-axis name of the plot. It can be either "individual observations" or "moving ranges".
#' The x-axis title is by default "QCno-name of peptide".
#' @param type It can take two values, "mean" or "dispersion".
#' @keywords XmR
#'          control chart
#' @export
#' @import dplyr
#' @import plotly
#' @import RecordLinkage
#' @examples
#' XmRPlots()
################################################################################################################
XmRPlots <- function(data, peptide, L = 1, U = 5, metric, normalization = FALSE,  ytitle = "Individual Observations", type = "mean") {
  #data <- input_checking(data)
  if(!is.data.frame(data)){
    stop(data)
  }
  metricData <- getMetricData(data, peptide, L, U, metric, normalization)
  precursor.data <- data[data$Precursor==peptide,]
  plot.data <- XmR.data.prepare(data, metricData, L, U, type)

  pal <- c("blue","red")
  pal <- setNames(pal,c("InRange","OutRange"))
  x <- list(
    title = paste("QCno - ", peptide)
  )
  y <- list(
    title = ytitle
  )

  plot_ly(plot.data, x = ~QCno, y = ~t,showlegend = TRUE) %>%
    add_trace(x = ~QCno, y = ~t, color = ~InRangeOutRange, type="scatter",
              mode="markers", colors = pal , showlegend = TRUE) %>%
    add_lines(x = ~QCno, y = ~t, color = I("cornflowerblue"), showlegend = FALSE) %>%
    add_lines(y = ~LCL, color = I("red"), name = "LCL", showlegend = FALSE) %>%
    add_lines(y = ~UCL, color = I("red"), name = "UCL", showlegend = FALSE) %>%
    layout(xaxis = x,yaxis = y)
}
