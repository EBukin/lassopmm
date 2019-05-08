#' Run shiny app available in the package
#'
#' @param app_name is the name of the app availabe in the package.
#' @export
#'
run_shiny_app_name <- function(app_name = "shiny_imputations") {
  # locate all the shiny app app_names that exist
  valid_app_names <- list.files(system.file("shiny", package = "syntheticpanel"))

  valid_app_names_message <-
    paste0(
      "Valid app_names are: '",
      paste(valid_app_names, collapse = "', '"),
      "'"
    )

  # if an invalid app_name is given, throw an error
  if (missing(app_name) || !nzchar(app_name) ||
    !app_name %in% valid_app_names) {
    stop(
      "Please run `run_shiny_app_name()` with a valid app_name app as an argument.\n",
      valid_app_names_message,
      call. = FALSE
    )
  }

  # find and launch the app
  appDir <- system.file("shiny-app_names", app_name, package = "mypackage")
  shiny::runApp(appDir, display.mode = "normal")
}
