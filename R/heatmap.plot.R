#' heatmap plot
#'
#' This function allows you to draw the heatmap plot.
#' @param data Comma-separated (*.csv), QC file format. It should contain a Precursor column and the metrics columns.
#' @param method It is either "CUSUM" or "XmR"
#' @param peptideThresholdRed Is a threshold that marks percentage of peptides above it red on the heatmap. Defaults to 0.7
#' @param peptideThresholdYellow Is a threshold that marks percentage of peptides above it and below the peptideThresholdRed, yellow on the heatmap. Defaults to 0.5
#' @param L Lower bound of the giude set. Defaults to 1
#' @param U Upper bound of the guide set. Defaults to 5
#' @param type can take two values, "mean" or "dispersion". Defaults to "mean"
#' @param title the title of the plot. Defaults to "heatmap plot"
#' @param selectMean If you want to choose mean yourself and do not use the guide set. Defaults to NULL
#' @param selectSD If you want to choose standard deviation yourself and do not want to use the guide set. Defaults to NULL
#' @keywords heatmap
#' @export
#' @import ggplot2
#' @import RecordLinkage
#' @import grid
#' @examples
#' heatmap.plot()

#########################################################################################################
heatmap.plot <- function(data, method,peptideThresholdRed = 0.7,peptideThresholdYellow = 0.5, L = 1, U = 5, type = "mean", title = "heatmap plot",selectMean = NULL,selectSD = NULL) {
  data <- input_checking(data)
  if(!is.data.frame(data)){
    stop(data)
  }

  data.metrics <- c(find_custom_metrics(data))
  remove <- c("MinStartTime","MaxEndTime")
  data.metrics <- data.metrics[!data.metrics %in% remove]

  color_palette <- colorRampPalette(c("green", "yellow", "red"))(3)
  data <- heatmap.DataFrame(data, data.metrics,method,peptideThresholdRed,peptideThresholdYellow, L, U, type,selectMean,selectSD)
  p <- ggplot(data,aes(time,metric, group = bin, fill = bin))
  # p <- p + scale_fill_gradient2(low="#F0E442", high="#000000", mid="#D55E00",
  #                               midpoint=0.5,
  #                               #, limit=c(0.2,0.8)
  #                               name="Correlation\n(Pearson)",guide = "legend"
  #                               #, na.value = "red"
  #                               )

  # p <- p + scale_color_manual(values=c("Good" = "green","Bad" = "red","Warning" = "orange"),
  #                             breaks=c("Good","Bad","Warning"),
  #                             guide='legend')
  p <- p + geom_tile(colour="white",size=.1)
  p <- p + coord_equal()
  #p <- p + theme_minimal(base_size = 10, base_family = "Trebuchet MS")
  p <- p + removeGrid()
  p <- p + rotateTextX()

  p <- p + ggtitle(title,subtitle = "")

  p <- p + labs(x=NULL, y=NULL)
  #p <- p + theme(plot.title=element_text(hjust=0))
  #p <- p + theme(axis.ticks=element_blank())

  #p <- p +  theme(legend.title=element_text(size=16))
  #p <- p +  theme(legend.text=element_text(size=12))
  p<-p + scale_fill_manual(values = color_palette, name = "")
  p <- p +  theme(axis.text=element_text(size=12))

  p
}
