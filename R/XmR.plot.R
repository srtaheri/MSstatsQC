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
XmRPlots <- function(data, peptide, L = 1, U = 5, metric, normalization = FALSE,  ytitle = "XmR Plot", type = "mean") {
  data <- input_checking(data)
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
  plot_ly(plot.data, x = QCno, y = t, type = "scatter",
          name = "",  line = list(shape = "linear"),
          marker=list(color="dodgerblue" , size=4 , opacity=0.5)
          ,showlegend = FALSE
          , text=precursor.data$Annotations
  ) %>%
    layout(xaxis = x,yaxis = y) %>%
    add_trace(y = UCL, marker=list(color="red" , size=4 , opacity=0.5), mode = "lines",showlegend = FALSE,name="UCL") %>%
    add_trace(y = LCL, marker=list(color="red" , size=4 , opacity=0.5), mode = "lines",showlegend = FALSE,name="LCL") %>%
    add_trace(x = plot.data[t <= LCL, ]$QCno, y = plot.data[t <= LCL, ]$t
              , mode = "markers"
              , marker=list(color="red" , size=8 , opacity=0.5)
              ,showlegend = FALSE,name=""
    ) %>%
    add_trace(x = plot.data[t >= UCL, ]$QCno, y = plot.data[t >= UCL, ]$t
              , mode = "markers"
              , marker=list(color="red" , size=8 , opacity=0.5)
              ,showlegend = FALSE,name=""
    ) %>%
    add_trace(x = plot.data[t > LCL & t < UCL, ]$QCno, y = plot.data[t > LCL & t < UCL, ]$t
              , mode = "markers"
              , marker=list(color="blue" , size=8 , opacity=0.5)
              ,showlegend = FALSE,name=""
    )

}
