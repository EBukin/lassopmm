#' Run shiny app available in the package
#'
#' @param app_name is the name of the app availabe in the package.
#' @export
#'
run_shiny_imputations <- function(app_name = "shiny_imputations") {
  valid_app_names <- list.files(system.file("shiny", package = "syntheticpanel"))
  valid_app_names_message <-
    paste0(
      "Valid app_names are: '",
      paste(valid_app_names, collapse = "', '"),
      "'"
    )
  if (missing(app_name) || !nzchar(app_name) ||
    !app_name %in% valid_app_names) {
    stop(
      "Please run `run_shiny_app_name()` with a valid app_name app as an argument.\n",
      valid_app_names_message,
      call. = FALSE
    )
  }
  appDir <- system.file("shiny", app_name, package = "syntheticpanel")
  shiny::runApp(appDir, display.mode = "normal")
}
