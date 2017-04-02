#' Shiny connection for example datasets
#'
#' This function allows you to draw the heatmaps to help user test their decision intervals. This plot provides massages about overall system performance.
#' @keywords shiny
#' @export
#' @import shiny
#' @examples
#' RunShiny()
RunShiny <- function(data) {
      appDir <- system.file(package = "MSstatsQC")
      source(paste0(appDir,"/shiny-examples/msstats-qc/app.R"))
      runner(data)
}
