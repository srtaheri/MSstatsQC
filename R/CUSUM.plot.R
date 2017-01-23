#' A Cumulative Sum (CUSUM) Function
#'
#' This function allows you to calculate CUSUM statistics.
#' @param data Comma-separated (*.csv), QC file format. It should contain a Precursor column and the metrics columns.
#' @param peptide The name of precursor you want to draw the plot for
#' @param L Lower bound of the giude set. Defaults to L = 1
#' @param U Upper bound of the guide set. Defaults to U = 5
#' @param ytitle The y-axis title of the plot.Defaults to "CUSUM Plot". The x-axis title is by default "QCno-name of peptide"
#' @param type can take two values, "mean" or "dispersion". Defaults to "mean"
#' @keywords Cumulative Sum, control chart
#' @export
#' @import dplyr
#' @import plotly
#' @import RecordLinkage
#' @examples
#' CuSumPlots()

#################################################################################################
CuSumPlots<- function(data, peptide, L = 1, U = 5, metric, normalization = TRUE,  ytitle = "CUSUM Plot", type = "mean") {
  data <- input_checking(data)
  if(!is.data.frame(data)){
    stop(data)
  }
  CUSUM.outrange.thld <- 5
  metricData <- getMetricData(data, peptide, L, U, metric, normalization)
  plot.data <- CUSUM.data.prepare(data, metricData, peptide, L, U, type)

  #ymax=ifelse(max(plot.data$CUSUM)>=CUSUM.outrange.thld,(max(plot.data$CUSUM)),CUSUM.outrange.thld)
  #ymin=ifelse(min(plot.data$CUSUM)<=-CUSUM.outrange.thld,(min(plot.data$CUSUM)),-CUSUM.outrange.thld)

  x <- list(
    title =  paste("QCno - ", peptide),
    range = c(0, max(plot.data$QCno))
  )
  y <- list(
    title = ytitle
  )

  p <- plot_ly(plot.data
               , x = QCno
               , y = CUSUM.poz
               , line = list(color = "dodgerblue")
               , name = "CUSUM+"
               , showlegend = FALSE
               , text = Annotations
  ) %>%
    add_trace(  x = QCno
                , y = CUSUM.neg
                , line = list(color = "blue")
                , name = "CUSUM-"
                , showlegend = FALSE
                , text = Annotations
    ) %>%
    layout(xaxis = x,yaxis = y, showlegend = FALSE) %>%
    add_trace(x=c(0, max(plot.data$QCno)),y = c(CUSUM.outrange.thld,CUSUM.outrange.thld), marker=list(color="red" , size=4 , opacity=0.5), name = "UCL",showlegend = FALSE) %>%
    add_trace(x=c(0, max(plot.data$QCno)),y = c(-CUSUM.outrange.thld,-CUSUM.outrange.thld), marker=list(color="red" , size=4 , opacity=0.5), name = "LCL",showlegend = FALSE) %>%
    add_trace(  x = QCno
                , y = CUSUM.neg
                , mode = "markers"
                , marker=list(color="blue" , size=5 , opacity=0.5)
                , showlegend = FALSE,name=""
    ) %>%
    add_trace(  x = QCno
                , y = CUSUM.poz
                , mode = "markers"
                , marker=list(color="blue" , size=5 , opacity=0.5)
                , showlegend = FALSE,name=""
    ) %>%
    add_trace(x = plot.data[CUSUM.poz <= -CUSUM.outrange.thld, ]$QCno,
              y = plot.data[CUSUM.poz <= -CUSUM.outrange.thld, ]$CUSUM.poz,
              mode = "markers",
              marker=list(color="red" , size=5 , opacity=0.5),
              showlegend = FALSE,name=""
    ) %>%
    add_trace(x = plot.data[CUSUM.poz >= CUSUM.outrange.thld, ]$QCno,
              y = plot.data[CUSUM.poz >= CUSUM.outrange.thld, ]$CUSUM.poz,
              mode = "markers",
              marker=list(color="red" , size=5 , opacity=0.5),
              showlegend = FALSE,name=""
    )%>%
    add_trace(x = plot.data[CUSUM.neg <= -CUSUM.outrange.thld, ]$QCno,
              y = plot.data[CUSUM.neg <= -CUSUM.outrange.thld, ]$CUSUM.neg,
              mode = "markers",
              marker=list(color="red" , size=5 , opacity=0.5),
              showlegend = FALSE,name=""
    ) %>%
    add_trace(x = plot.data[CUSUM.neg >= CUSUM.outrange.thld, ]$QCno,
              y = plot.data[CUSUM.neg >= CUSUM.outrange.thld, ]$CUSUM.neg,
              mode = "markers",
              marker=list(color="red" , size=5 , opacity=0.5),
              showlegend = FALSE,name=""
    )

  return(p)
}
