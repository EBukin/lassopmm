#' Internal function to help in matching observations.
#'
#' @param o is a value to match
#' @param m is a named vector which is used to find a nearest match to the \code{o}
#' @inheritParams find_near
#' @return It produces a list of four elements:
#'     \itemize{
#'       \item `\code{period_1_id} position of the \code{o} (observation element in the named vector)
#'       \item \code{y1_hat} value of the \code{o}
#'       \item \code{period_0_id} position of the nearest match in the alternatives vector \code{m}
#'       \item \code{y0_hat} value of the nearest match.
#'       }
#'
#' @export
#'
#' @examples
#' library(syntheticpanel)
#' find_one_near(c("a" = 10), c(8:15), 3)
#' find_one_near(c("a" = 10), setNames(c(8:15), letters[1:8]), 5)
#' find_one_near(c(9), setNames(c(8:15), letters[1:8]), 5)
#' find_one_near(c(10), c(8:15), 5)
find_one_near <- function(o, m, n_near = 5) {
  null_names_m <- is.null(names(m))
  if (null_names_m) m <- stats::setNames(m, as.character(1:length(m)))
  near_obs <- names(sort(abs(m - o), na.last = TRUE, decreasing = F))[1:min(n_near, length(m))]
  match <- sample(near_obs, size = 1, replace = FALSE)
  list(
    period_1_id = names(o),
    y1_hat = o[[1]],
    period_0_id = if (null_names_m) NULL else match,
    y0_hat = m[[match]]
  )
}

#' Find neares match between a vector of observations and a vector of alternatives
#'
#' @param observ_vector named vector or value to find a match from `match_vector`.
#'   If the vector has no names function will still work, but the resulting
#'   `period_1_id` variable will be empty.
#' @param match_vector named vector which is used to find a nearest match to the
#'   `observ_vector`.
#' @param n_near number of the nearest observations to derive a random match.
#'   If `n_near` is greater than `length(match_vector)`, minimum out of two is
#'   used to create a sample for selecting a random match value.
#'
#' @return A dataframe with the number of rows equall to the lenght of the
#'   `observ_vector` vector and one mathcing value and its index from the
#'   `match_vector` corresponding to each value afrom the `observ_vector`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(glmnet)
#' library(syntheticpanel)
#'
#' obs <- setNames(rnorm(5), as.character(1:5))
#' match_v <- setNames(rnorm(50), as.character(1:50))
#' find_near(obs, match_v, n_near = 10)
find_near <- function(observ_vector, match_vector, n_near = 5) {
  if (length(match_vector) < n_near) {
    warning("Number of the nearest observations for random selection is greater than the vector of alternatives.")
  }
  if (is.null(names(observ_vector))) {
    purrr::map_dfr(
      observ_vector,
      ~ find_one_near(o = .x, m = match_vector, n_near = n_near)
    ) %>%
      return()
  } else {
    purrr::map2_dfr(
      observ_vector, names(observ_vector),
      ~ find_one_near(o = setNames(.x, .y), m = match_vector, n_near = n_near)
    ) %>%
      return()
  }
}
