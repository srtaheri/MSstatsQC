#' Shiny connection for example datasets 
#'
#' This function allows you to draw the heatmaps to help user test their decision intervals. This plot provides massages about overall system performance.
#' @keywords shiny
#' @export
#' @import shiny
#' @examples
#' RunShiny()
RunShiny <- function() {
    appDir <- system.file("shiny-examples", "msstats-qc", package = "MSstatsQC")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `MSstatsQC`.", call. = FALSE)
    }
    
    shiny::runApp(appDir, display.mode = "normal")
  }