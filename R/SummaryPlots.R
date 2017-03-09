#' A river and a radar plot to summarize results from XmR plots
#'
#' This function allows you to draw the XmR summary plot.
#' @param data Comma-separated (*.csv), QC file format. It should contain a Precursor column and the metrics columns.
#' @param L Lower bound of the giude set. Default is 1.
#' @param U Upper bound of the guide set. Default is 5.
#' @keywords XmR
#'           Summary plot
#' @export
#' @import ggplot2
#' @import RecordLinkage
#' @import grid
#' @examples
#' XmRSummaryPlots()
XmRSummaryPlots <- function(data, L=1, U=5) {

   if(!is.data.frame(data)){
    stop(data)
  }
  data.metrics <- c(find_custom_metrics(data))
  dat <- XmR.Summary.DataFrame(data,data.metrics, L, U)
  tho.hat.df <- get_CP_tho.hat(data, L, U, data.metrics)

  gg <- ggplot(dat)
  gg <- gg + geom_hline(yintercept=0, alpha=0.5)
  gg <- gg + geom_smooth(method="loess",aes(x=dat$QCno, y=dat$pr.y,colour = group, group = group))
  gg <- gg + geom_point(data = tho.hat.df, aes(x = tho.hat.df$tho.hat, y = tho.hat.df$y, colour = "Change point"))
  gg <- gg + scale_color_manual(breaks = c("Metric mean increase",
                                           "Metric mean decrease",
                                           "Metric dispersion increase",
                                           "Metric dispersion decrease",
                                           "Change point"),
                                values = c("Metric mean increase" = "#E69F00",
                                           "Metric mean decrease" = "#56B4E9",
                                           "Metric dispersion increase" = "#009E73",
                                           "Metric dispersion decrease" = "#D55E00",
                                           "Change point" = "red"),
                                guide='legend')
  gg <- gg + guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1,0),shape=c(NA,NA,NA,NA,16))))
  gg <- gg + facet_wrap(~metric,nrow = ceiling(length(data.metrics)/4))
  gg <- gg + annotate("text", x = 15, y = 1.3, label = "Mean")
  gg <- gg + annotate("text", x = 25, y = -1.3, label = "Dispersion")
  gg <- gg + scale_y_continuous(expand=c(0,0), limits = c(-1.4,1.4),breaks = c(1,0.5,0,-0.5,-1) ,labels = c(1,0.5,0,"0.5","1"))
  gg <- gg + labs(x = "QC No", y = "% of out of control \nprecursors")
  gg <- gg + ggtitle("Overall Summary \nXmR")
  theme_set(theme_gray(base_size = 15)) # this will change the size of all the texts in all ggplot functions
  gg <- gg + theme(plot.title = element_text(size=15, face="bold",margin = margin(10, 0, 10, 0)),
                   axis.text.x=element_text(size=12, vjust=0.5),
                   axis.text.y=element_text(size=12, hjust=0.5),
                   axis.title.y=element_text(size=12),
                   axis.title.x=element_text(size=12),
                   legend.text = element_text(size = 12),
                   legend.title=element_blank(),
                   plot.margin = unit(c(1,3,1,1), "lines")
  )
  gg

}

#################################################################################################
#' A river and a radar plot to summarize results from CUSUMm and CUSUMv plots
#'
#' This function allows you to draw the CUSUM summary plot.
#' @param data Comma-separated (*.csv), QC file format. It should contain a Precursor column and the metrics columns.
#' @param L Lower bound of the giude set. Default is 1.
#' @param U Upper bound of the guide set. Default is 5.
#' @keywords CUSUM
#'           Summary plot
#' @export
#' @import ggplot2
#' @import RecordLinkage
#' @examples
#' CUSUMSummaryPlots()
CUSUMSummaryPlots <- function(data, L = 1, U = 5) {

  if(!is.data.frame(data)){
    stop(data)
  }
  h <- 5
  data.metrics <- c(find_custom_metrics(data))

  dat <- CUSUM.Summary.DataFrame(data, data.metrics, L, U)
  tho.hat.df <- get_CP_tho.hat(data, L, U, data.metrics)

  gg <- ggplot(dat)
  gg <- gg + geom_hline(yintercept=0, alpha=0.5)
  gg <- gg + stat_smooth(method="loess", aes(x=dat$QCno, y=dat$pr.y, colour = group, group = group))
  gg <- gg + geom_point(data = tho.hat.df, aes(x = tho.hat.df$tho.hat, y = tho.hat.df$y, colour = "Change point"))
  gg <- gg + scale_color_manual(breaks = c("Metric mean increase",
                                           "Metric mean decrease",
                                           "Metric dispersion increase",
                                           "Metric dispersion decrease",
                                           "Change point"),
                                values = c("Metric mean increase" = "#E69F00",
                                           "Metric mean decrease" = "#56B4E9",
                                           "Metric dispersion increase" = "#009E73",
                                           "Metric dispersion decrease" = "#D55E00",
                                           "Change point" = "red"),
                                guide='legend')
  gg <- gg + guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1,0),shape=c(NA,NA,NA,NA,16))))
  gg <- gg + facet_wrap(~metric,nrow = ceiling(length(data.metrics)/4))
  gg <- gg + annotate("text", x = 15, y = 1.3, label = "Mean")
  gg <- gg + annotate("text", x = 25, y = -1.3, label = "Dispersion")
  gg <- gg + scale_y_continuous(expand=c(0,0), limits = c(-1.4,1.4),
                                breaks = c(1,0.5,0,-0.5,-1) ,labels = c(1,0.5,0,"0.5","1"))
  gg <- gg + ggtitle("Overall Summary \nCUSUM")

  gg <- gg + labs(x = "QC No", y = "% of out of control \nprecursors")
  gg <- gg + theme(plot.title = element_text(size=15, face="bold",margin = margin(10, 0, 10, 0)),
                   axis.text.x=element_text(size=12, vjust=0.5),
                   axis.text.y=element_text(size=12, hjust=0.5),
                   axis.title.y=element_text(size=12),
                   axis.title.x=element_text(size=12),
                   legend.text = element_text(size = 12),
                   legend.title=element_blank(),
                   plot.margin = unit(c(1,3,1,1), "lines")
  )

  gt <- ggplot_gtable(ggplot_build(gg))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
  gg

}
