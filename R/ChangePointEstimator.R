#' A Change Point (CP) Function
#'
#' This function allows you to calculate change point statistics for a step change in mean or variability.
#' @param data Comma-separated (*.csv), QC file format. It should contain a Precursor column and the metrics columns.
#' @param peptide The name of precursor you want to draw the plot for
#' @param L Lower bound of the guide set
#' @param U Upper bound of the guide set
#' @param metric QC metric in your data to be monitored.
#' @param normalization TRUE if QC metric is standardized and FALSE if not standardized
#' @param ytitle The y-axis title of the plot. It can be any name of your choice. Defaults to "Change Point Plot - mean". The x-axis title is by default "QCno-name of peptide"
#' @param type can take two values, "mean" or "dispersion".
#' @param selectMean The mean of the metric. If you know it, enter the value of mean. If you don't know the mean of the metric leave it as Null and it will be calculated automatically by using L and U. The default is NULL.
#' @param selectSD The standard deviatiob of the metric. If you know it, enter the value of standard deviation. If you don't know the standard deviation of the metric leave it as Null and it will be calculated automatically by using L and U. The default is NULL.
#' @keywords Change point, control chart
#' @export
#' @import dplyr
#' @importFrom plotly plot_ly add_markers add_lines layout
#' @import RecordLinkage
#' @examples
#' # First process the data to make sure it's ready to use
#' sampleData <- DataProcess(S9Site54)
#' head(sampleData)
#' # Find the name of the peptides
#' levels(sampleData$Precursor)
#' # Calculate change point statistics
#' ChangePointEstimator(data = sampleData, peptide = "VLVLDTDYK", metric = "BestRetentionTime")
#' ChangePointEstimator(data = sampleData, peptide = "VLVLDTDYK", metric = "BestRetentionTime",
#'                      ytitle = "Change Point Plot - dispersion", type = "dispersion")
#' ChangePointEstimator(data = sampleData, peptide = "VLVLDTDYK", metric = "BestRetentionTime",
#'                      selectMean = 27.78, selectSD = 8.19)
#' ChangePointEstimator(data = sampleData, peptide = "DDGSWEVIEGYR", metric = "TotalArea")
#' ChangePointEstimator(data = sampleData, peptide = "DDGSWEVIEGYR", metric = "TotalArea",
#'                      selectMean = 35097129, selectSD = 34132861)
#' ChangePointEstimator(data = sampleData, peptide = "TAAYVNAIEK", metric = "MaxFWHM")
#' ChangePointEstimator(data = sampleData, peptide = "LVNELTEFAK", metric = "Peak Assymetry")

ChangePointEstimator <- function(data = NULL, peptide, L = 1, U = 5, metric, normalization = TRUE,
                                 ytitle = "Change Point Plot - mean", type = "mean", selectMean = NULL,
                                 selectSD = NULL) {
  if(is.null(data))
    return()
  if(!is.data.frame(data)){
    stop(data)
  }
  metricData <- getMetricData(data, peptide, L, U, metric, normalization, selectMean, selectSD)
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
