#' A function to create cumulative sum charts for mean (CUSUMm) and cumulative sum charts for variability (CUSUMv) control charts for QC metrics
#'
#' This function allows you to calculate CUSUMm and CUSUMv statistics and draw control charts for certain QC metrics and peptides.
#'
#' @param data Comma-separated (*.csv), QC metric file. It should contain a "Precursor" column and the "QC metrics" columns.
#' It can also include "Annotations" for each observation.
#' @param peptide The name of precursor you want to draw the plot for.
#' @param L Lower bound of the guide set.
#' @param U Upper bound of the guide set.
#' @param metric The name of the QC metric in your data.
#' @param normalization TRUE if data is standardized.
#' @param ytitle The y-axis name of the plot. It can be either "CUSUMm" or "CUSUMv" or any name of your choice.
#'  Defaults to "CUSUMm".  The x-axis title is by default "QCno-name of peptide"
#' @param type can take two values, "mean" or "dispersion". Defaults to "mean"
#' @param selectMean The mean of the metric. If you know it, enter the value of mean. If you don't know the mean
#'  of the metric leave it as Null and it will be calculated automatically by using L and U. The default is NULL.
#' @param selectSD The standard deviatiob of the metric. If you know it, enter the value of standard deviation.
#'  If you don't know the standard deviation of the metric leave it as Null and it will be calculated automatically
#'   by using L and U. The default is NULL.
#' @keywords Cumulative Sum, control chart
#' @export
#' @import dplyr
#' @importFrom plotly plot_ly add_markers add_lines layout
#' @importFrom stats setNames
#' @import RecordLinkage
#' @examples
#' # First process the data to make sure it's ready to use
#' sampleData <- DataProcess(S9Site54)
#' head(sampleData)
#' # Find the name of the peptides
#' levels(sampleData$Precursor)
#' # Calculate CUSUM statistics
#' CUSUMPlots(data = sampleData, peptide = "VLVLDTDYK", metric = "BestRetentionTime")
#' CUSUMPlots(data = sampleData, peptide = "VLVLDTDYK", metric = "BestRetentionTime",
#'            ytitle = "CUSUMv", type = "dispersion")
#' CUSUMPlots(data = sampleData, peptide = "VLVLDTDYK", metric = "BestRetentionTime",
#'            selectMean = 27.78, selectSD = 8.19)
#' CUSUMPlots(data = sampleData, peptide = "DDGSWEVIEGYR", metric = "TotalArea")
#' CUSUMPlots(data = sampleData, peptide = "DDGSWEVIEGYR", metric = "TotalArea",
#'            selectMean = 35097129, selectSD = 34132861)
#' CUSUMPlots(data = sampleData, peptide = "TAAYVNAIEK", metric = "MaxFWHM")
#' CUSUMPlots(data = sampleData, peptide = "LVNELTEFAK", metric = "Peak Assymetry")

#################################################################################################
CUSUMPlots<- function(data = NULL, peptide, L = 1, U = 5, metric, normalization = TRUE,
                      ytitle = "CUSUMm", type = "mean", selectMean = NULL, selectSD = NULL) {
  if(is.null(data))
    return()
  #data <- input_checking(data)
  if(!is.data.frame(data)){
    stop(data)
  }
  CUSUM.outrange.thld <- 5
  metricData <- getMetricData(data, peptide, L, U, metric, normalization, selectMean, selectSD)
  plot.data <- CUSUM.data.prepare(data, metricData, peptide, type)
  plot.data1 <- data.frame(
    QCno = rep(plot.data$QCno,2),
    CUSUMValue = c(plot.data$CUSUM.poz, plot.data$CUSUM.neg),
    Annotations = rep(plot.data$Annotations,2),
    outRangeInRange = c(as.character(plot.data$outRangeInRangePoz),
                        as.character(plot.data$outRangeInRangeNeg))
  )

  pal <- c("lightslateblue","red","blue","red")
  pal <- setNames(pal,c("InRangeCUSUM-","OutRangeCUSUM-","InRangeCUSUM+","OutRangeCUSUM+"))
  x <- list(
    title =  paste("QCno - ", peptide),
    range = c(0, max(plot.data$QCno))
  )
  y <- list(
    title = ytitle
  )

  plot_ly(plot.data1, x = ~QCno, y = ~CUSUM.poz,showlegend = FALSE)%>%
    add_markers(x = ~QCno, y = ~CUSUMValue, color = ~outRangeInRange,
                type="scatter",mode="markers", colors = pal , showlegend = TRUE) %>%

    add_lines(y = CUSUM.outrange.thld, color = I("red"), name = "CUSUM thld", showlegend = FALSE) %>%
    add_lines(y = -CUSUM.outrange.thld, color = I("red"), name = "CUSUM thld", showlegend = FALSE) %>%
    add_lines(data = plot.data, x = ~QCno, y = ~CUSUM.poz, color = I("cornflowerblue"),
              name = "CUSUM+", showlegend = FALSE) %>%
    add_lines(data = plot.data, x = ~QCno, y = ~CUSUM.neg, color = I("lightskyblue"),
              name = "CUSUM-",showlegend = FALSE)%>%
    layout(xaxis = x,yaxis = y)
}
