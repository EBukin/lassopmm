#' Check if the object supplied has an empty string value and retun NULL.
#'
#' @param x object to check
#' @export
var_check <- function(x) {
  if (identical(x[x!=""], character(0)))
    return(NULL)
  else
    return(x[x!=""])
}
