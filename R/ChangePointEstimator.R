#' A Change Point (CP) Function
#'
#' This function allows you to calculate change point statistics for a step change in mean or variability. 
#' @param data Comma-separated (*.csv), QC file format. It should contain a Precursor column and the metrics columns.
#' @param peptide The name of precursor you want to draw the plot for
#' @param L Lower bound of the guide set
#' @param U Upper bound of the guide set
#' @param metric QC metric to be monitored
#' @param normalization TRUE if QC metric is standardized and FALSE if not standardized
#' @param ytitle The y-axis title of the plot. The x-axis title is by default "QCno-name of peptide"
#' @param type can take two values, "mean" or "dispersion".
#' @keywords Change point
#'          control chart
#' @import dplyr
#' @import plotly
#' @import RecordLinkage
#' @examples
#' ChangePointEstimator()
ChangePointEstimator <- function(data, peptide, L = 1, U = 5, metric, normalization = TRUE,  ytitle = "Change Point Plot", type = "mean") {

  data <- input_checking(data)

  if(!is.data.frame(data)){
    stop(data)
  }
  metricData <- getMetricData(data, peptide, L, U, metric, normalization)
  precursor.data <- data[data$Precursor==peptide,]
  ## Create variables
  plot.data <- CP.data.prepare(data, metricData, type)
  y.max=max(plot.data$Et) # y axis upper limit
  y.min=0 # y axis lower limit

  x <- list(
    title = paste("QCno - ", peptide)
  )
  y <- list(
    title = ytitle
  )

  plot_ly(plot.data, x = QCno, y = Et
          ,type = "scatter"
          ,line = list(shape = "linear")
          ,showlegend = FALSE,name=""
          , text=precursor.data$Annotations
  ) %>%
    layout(xaxis = x,yaxis = y) %>%
    add_trace( x = c(tho.hat,tho.hat), y = c(0, (max(Et)+2))
               ,marker=list(color="red", size=4, opacity=0.5)
               , mode = "lines"
               ,showlegend = FALSE,name=""
    ) %>%
    add_trace(x = QCno, y =  Et
              ,mode = "markers"
              , marker=list(color="blue" , size=8 , opacity=0.5)
              ,showlegend = FALSE,name=""
    )
}
