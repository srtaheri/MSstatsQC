#' Heatmaps for visualization of overall system performance
#'
#' This function allows you to draw the heatmaps to help user test their decision intervals. This plot provides massages about overall system performance.
#' @param data Comma-separated (*.csv), QC file format. It should contain a Precursor column and the metrics columns.
#' @param method It is either "CUSUM" or "XmR"
#' @param peptideThresholdRed Is a threshold that marks percentage of peptides above it red on the heatmap. Defaults to 0.7
#' @param peptideThresholdYellow Is a threshold that marks percentage of peptides above it and below the peptideThresholdRed, yellow on the heatmap. Defaults to 0.5
#' @param L Lower bound of the giude set. Defaults to 1
#' @param U Upper bound of the guide set. Defaults to 5
#' @param type can take two values, "mean" or "dispersion". Defaults to "mean"
#' @param title the title of the plot. Defaults to "heatmap plot"
#' @keywords heatmap
#' @export
#' @import ggplot2
#' @import ggExtra
#' @import RecordLinkage
#' @import grid
#' @examples
#' DecisionMaker()

#########################################################################################################
DecisionMaker <- function(data, method,peptideThresholdRed = 0.7,peptideThresholdYellow = 0.5, L = 1, U = 5, type = "mean", title = "heatmap plot") {
  if(!is.data.frame(data)){
    stop(data)
  }

  data.metrics <- c(find_custom_metrics(data))
  remove <- c("MinStartTime","MaxEndTime")
  data.metrics <- data.metrics[!data.metrics %in% remove]

  data <- heatmap.DataFrame(data, data.metrics,method,peptideThresholdRed,peptideThresholdYellow, L, U, type)
  p <- ggplot(data,aes(time,metric, group = bin, fill = bin))
  p <- p + scale_fill_manual(values=c("Acceptable" = "blue","Unacceptable" = "red","Poor" = "yellow"))
  p <- p + geom_tile(colour="white",size=.1)
  p <- p + coord_equal()
  p <- p + removeGrid()
  p <- p + rotateTextX()
  p <- p + ggtitle(title,subtitle = "")
  p <- p + labs(x=NULL, y=NULL)
  p <- p +  theme(axis.text=element_text(size=12),legend.title = element_blank())
  p
}
