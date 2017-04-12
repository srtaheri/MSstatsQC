#' A function to create individual (X) and moving range (mR) control charts for QC metrics
#'
#' This function allows you to calculate X and mR statistics
#'  and draw control charts for certain QC metrics and peptides.
#'
#' @param data Comma-separated (*.csv), QC metric file.
#' It should contain a "Precursor" column and the "QC metrics" columns.
#' It can also include "Annotations" for each observation.
#' @param peptide The name of precursor you want to draw the plot for.
#' @param L Lower bound of the guide set.
#' @param U Upper bound of the guide set.
#' @param metric The name of the QC metric in your data.
#' @param normalization TRUE if data is standardized.
#' @param ytitle The y-axis name of the plot.
#' It can be either "individual observations" or "moving ranges" or any name of your choice.
#' The x-axis title is by default "QCno-name of peptide".
#' @param type It can take two values, "mean" or "dispersion".
#' @param selectMean The mean of the metric. If you know it, enter the value of mean.
#' If you don't know the mean of the metric leave it as Null and it will be calculated
#'  automatically by using L and U. The default is NULL.
#' @param selectSD The standard deviatiob of the metric.
#' If you know it, enter the value of standard deviation. I
#' If you don't know the standard deviation of the metric leave it as Null and it will
#'  be calculated automatically by using L and U. The default is NULL.
#' @keywords XmR
#'          control chart
#' @export
#' @import dplyr
#' @importFrom plotly plot_ly add_trace add_lines layout
#' @importFrom stats setNames sd
#' @import RecordLinkage
#' @examples
#' # First process the data to make sure it's ready to use
#' sampleData <- DataProcess(S9Site54)
#' head(sampleData)
#' # Find the name of the peptides
#' levels(sampleData$Precursor)
#' # Calculate X and mR statistics
#' XmRPlots(data = sampleData, peptide = "VLVLDTDYK", metric = "BestRetentionTime")
#' XmRPlots(data = sampleData, peptide = "VLVLDTDYK", metric = "BestRetentionTime",
#'          ytitle = "moving ranges", type = "dispersion")
#' XmRPlots(data = sampleData, peptide = "VLVLDTDYK", metric = "BestRetentionTime",
#'          selectMean = 27.78, selectSD = 8.19)
#' XmRPlots(data = sampleData, peptide = "DDGSWEVIEGYR", metric = "TotalArea")
#' XmRPlots(data = sampleData, peptide = "DDGSWEVIEGYR", metric = "TotalArea",
#'          selectMean = 35097129, selectSD = 34132861)
#' XmRPlots(data = sampleData, peptide = "TAAYVNAIEK", metric = "MaxFWHM")
#' XmRPlots(data = sampleData, peptide = "LVNELTEFAK", metric = "Peak Assymetry")
################################################################################################################
XmRPlots <- function(data = NULL, peptide, L = 1, U = 5, metric, normalization = FALSE,
                     ytitle = "Individual Observations", type = "mean",
                     selectMean = NULL,selectSD = NULL) {
  #data <- input_checking(data)
  if(is.null(data))
    return()
  if(!is.data.frame(data)){
    stop(data)
  }
  metricData <- getMetricData(data, peptide, L, U, metric,
                              normalization, selectMean, selectSD)
  precursor.data <- data[data$Precursor==peptide,]
  plot.data <- XmR.data.prepare(data, metricData, L, U, type,selectMean,selectSD)

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
