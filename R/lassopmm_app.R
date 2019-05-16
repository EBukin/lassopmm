#' Run shiny app with the lassopmm functionality
#'
#' @export
#'
lassopmm_app <- function() {
  appDir <- system.file("shiny", "lassopmm_app", package = "lassopmm")
  shiny::runApp(appDir, display.mode = "normal")
}
