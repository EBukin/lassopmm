#' Resample a vector of values with repetition
#'
#' @param ids vector of the values t be resampled
#' @return Same length vector with the resampled values.
#'
#' @export
#'
#' @examples
#' library(lassopmm)
#'
#' sample_vector(1:10)
sample_vector <- function(ids) {
  sample(ids, size = length(ids), replace = T)
}

#' Generates a list of permutation vectors for bootsrapping
#'
#' @param data dataframe containing two columns \code{group} and \code{id}.
#' @param n_boot number of bootstrap permulations (resamplings).
#'     If \code{n_rep = 0}, no bootstraping is performed and we have the
#'     bootstrap sample, whcih consist of the same observations in the same
#'     order as source data.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(tidyr)
#' library(glmnet)
#' library(lassopmm)
#'
#' df <- tibble::tibble(id = 1:30, group = sort(rep(1:3, 10)))
#' get_bootstrap_permutation(df, 3)
get_bootstrap_permutation <- function(data, n_boot) {
  if (is_character(n_boot) ||
    n_boot < 0) {
    stop("n_boot must be numeric or integer and greater than 0.")
  }
  data <-
    data %>%
    dplyr::group_by(group) %>%
    tidyr::nest() %>%
    dplyr::pull(data)

  if (n_boot == 0) {
    purrr::map(1, function(i) {
      purrr::map(data, function(y) {
        y[[1]]
      }) %>%
        unlist()
    }) %>%
      return()
  } else {
    purrr::map(1:n_boot, function(i) {
      purrr::map(data, function(y) {
        sample_vector(y[[1]])
      }) %>%
        unlist()
    }) %>%
      return()
  }
}

#' Converts a table of the values into a list of permutation vectors. (used for development)
#' @param boot_table is the table of the bootstrap iterations.
#' @export
convert_bootstrap_permutation <- function(boot_table) {
  boot_table %>%
    as.matrix() %>%
    unname() %>%
    apply(2, function(x) {
      list(x)
    }) %>%
    unlist(recursive = F)
}
