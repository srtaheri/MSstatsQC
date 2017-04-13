#' A radar plot to summarize results from XmR plots
#'
#' This function allows you to draw the XmR river plot.
#' @param data Comma-separated (*.csv), QC file format.
#' It should contain a Precursor column and the metrics columns.
#' @param L Lower bound of the giude set. Default is 1.
#' @param U Upper bound of the guide set. Default is 5.
#' @param listMean List of the means for the metrics.
#' If you don't know the means leave it as NULL and they will be calculated automatically
#' by using L and U. The default is NULL.
#' @param listSD List of the standard deviations for the metrics.
#' If you don't know the standard deviations leave it as NULL and they will be calculated
#' automatically by using L and U. The default is NULL.
#' @keywords XmR
#'           river plot
#' @export
#' @import ggplot2
#' @import RecordLinkage
#' @import grid
#' @importFrom  stats reorder
#' @examples
#' # First process the data to make sure it's ready to use
#' sampleData <- DataProcess(S9Site54)
#' head(sampleData)
#' # Draw XmR radar plot
#' XmR.Radar.Plot(data = sampleData)
#' XmR.Radar.Plot(data = sampleData,
#'                 listMean = list("BestRetentionTime" = 27.78,
#'                                 "TotalArea" = 35097129,
#'                                 "MaxFWHM" = 0.28,
#'                                 "Peak Assymetry" = 0.98),
#'                 listSD = list("BestRetentionTime" = 8.19,
#'                               "TotalArea" = 34132861,
#'                               "MaxFWHM" = 0.054,
#'                               "Peak Assymetry" = 0.002)
#'                 )
###########################################################################################
XmRRadarPlots <- function(data = NULL, L = 1, U = 5,
                           listMean = NULL, listSD = NULL) {
  if(is.null(data))
    return()
  data.metrics <- c(find_custom_metrics(data))
  remove <- c("MinStartTime","MaxEndTime")
  data.metrics <- data.metrics[!data.metrics %in% remove]
  dat <- XmR.Radar.Plot.DataFrame(data, data.metrics, L,U,listMean,listSD)
  OutRangeQCno <- dat$OutRangeQCno
  peptides <- dat$peptides
  orderby <- dat$orderby
  group <- dat$group
  ggplot(dat, aes(y = OutRangeQCno, x = reorder(peptides,orderby),
                  group = group, colour = group, fill=group)) +
    coord_polar() +
    geom_point() +
    scale_fill_manual(breaks = c("Metric mean increase",
                                 "Metric mean decrease",
                                 "Metric dispersion increase",
                                 "Metric dispersion decrease"),
                      values = c("Metric mean increase" = "#E69F00",
                                 "Metric mean decrease" = "#56B4E9",
                                 "Metric dispersion increase" = "#009E73",
                                 "Metric dispersion decrease" = "#D55E00")) +
    scale_color_manual(breaks = c("Metric mean increase",
                                  "Metric mean decrease",
                                  "Metric dispersion increase",
                                  "Metric dispersion decrease"),
                       values = c("Metric mean increase" = "#E69F00",
                                  "Metric mean decrease" = "#56B4E9",
                                  "Metric dispersion increase" = "#009E73",
                                  "Metric dispersion decrease" = "#D55E00")) +
    facet_wrap(~metric,nrow = ceiling(length(data.metrics)/4)) +
    geom_polygon(alpha=0.5)+
    ggtitle("Precursor Level Summary \nXmR") +
    xlab("") +
    ylab("# of out of control \nQC samples") +
    theme(
      axis.text.x = element_text(face="bold",size = rel(0.7)),
      axis.title.y=element_text(size=12),
      axis.text.y=element_text(size=12, hjust=0.5),
      plot.title = element_text(size=15, face="bold",margin = margin(10, 0, 10, 0)),
      legend.title=element_blank(),
      legend.text = element_text(size = 12),
      panel.grid.major = element_line(colour = "firebrick3",linetype = "dotted"),
      plot.margin = unit(c(1,3,1,1), "lines")
    )

}

