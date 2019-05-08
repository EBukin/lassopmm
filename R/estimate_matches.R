#' Run lasso estimation on the prepared matrixes
#'
#' @param x_mat,y_mat,w_mat matrixes of independent, dependent variables and weights
#' @param x1_mat matrixes of independent variables for the prediction sample
#' @param reduced if TRUE terurns reduced outpur without specific regression details.
#' @param n_folds number of folds for cross-validation
#' @inheritParams find_near
#'
#' @return In both \code{reduced=TRUE} and \code{reduced=FALSE} forms, the function returns
#'     a list with elements. In the form \code{reduced=TRUE} only results of matching
#'     are returned:
#'     \itemize{
#'       \item \code{y0_hat} is the vecrtor of predicted values based on the result of
#'             the lasso regression with \code{nfolds} cross validation and "mse"
#'             measure for identifying minimizing value of lambda. It uses \code{x_mat},
#'             \code{y_mat}, and \code{w_mat} for running regression and predictind \code{y0_hat}.
#'       \item \code{y1_hat} is the vetor of predicted values produced using the
#'             \code{y0_hat} regression results and \code{x1_mat} independent variables matrix.
#'       \item \code{match} is the dataframe with: columns \code{period_1_id} - index of each
#'              \code{y1_hat} value; column \code{y1_hat} - its' value; column \code{period_0_id} -
#'              position of the nearest match from the \code{y0_hat} vector and
#'              column \code{y0_hat} - value of the nearest match
#'     }
#'
#'     In the not reduced form the list contains more elements:
#'     \itemize{
#'       \item items \code{x_mat}, \code{y_mat}, \code{w_mat} and \code{x1_mat} from the inputs to the
#'             functions.
#'       \item \code{lambda_cv} - result of the \code{\link[glmnet]{cv.glmnet}}.
#'       \item \code{fit}  - result of the \code{\link[glmnet]{glmnet}}
#'     }
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(glmnet)
#' library(syntheticpanel)
#'
#' # Run estimate of a fake data
#' XX <- as.matrix(mtcars[, !names(mtcars) %in% "hp"])
#' YY <- as.matrix(mtcars[, "hp"])
#' WW <- matrix(rep(1, nrow(mtcars)), ncol = 1, nrow = nrow(mtcars))
#' XX1 <- as.matrix(mtcars[1:10, !names(mtcars) %in% "hp"])
#'
#' # Running simple estimation and returning
#' a <- estimate_matches(x_mat = XX, y_mat = YY, w_mat = WW, x1_mat = XX1, reduced = FALSE, n_near = 5)
#'
#' # Extract regression coefficients
#' a$fit %>% coef()
#' str(a, max.level = 1)
#'
#' # Developing a sample bootsrtap vector
#' perm_example <- purrr::map(1:5, ~ sample(1:nrow(YY), nrow(YY), TRUE))
#'
#' # Running estimations on every single bootstrap permutation vector
#' a_boot <-
#'   perm_example %>%
#'   purrr::map(~ syntheticpanel::estimate_matches(
#'     x_mat = XX[.x, ],
#'     y_mat = YY[.x, ],
#'     w_mat = WW[.x, ],
#'     x1_mat = XX1,
#'     reduced = FALSE,
#'     n_near = 5
#'   ))
#'
#' # Exploring the structure
#' a_boot %>%
#'   str(max.level = 1)
#'
#' # Accesssing fit of a specific bootstrap iteration
#' a_boot[[3]]$fit
#'
#' # Doing the same with each single bootstrap iteration
#' a_boot %>%
#'   map("fit")
#'
#' # Extracting coefficinets from each single bootstrap iteration
#' a_boot %>%
#'   map("fit") %>%
#'   map(coefficients)
#'
#' # Combine all coefficients in a table
#' a_boot %>%
#'   map("fit") %>%
#'   map(broom::tidy) %>%
#'   map(~ select(.x, term, estimate)) %>%
#'   map2(.y = 1:length(.), ~ rename_at(.x, vars(estimate), list(~ paste0(., "_", .y)))) %>%
#'   reduce(full_join)
estimate_matches <-
  function(x_mat,
             y_mat,
             w_mat,
             x1_mat,
             n_near = 5,
             reduced = TRUE,
             n_folds = 10) {
    lambda_cv <-
      glmnet::cv.glmnet(
        x = x_mat,
        y = y_mat,
        weights = w_mat,
        type.measure = "mse",
        nfolds = n_folds
      )
    fit <-
      glmnet::glmnet(
        x = x_mat,
        y = y_mat,
        weights = w_mat,
        alpha = 1,
        lambda = lambda_cv$lambda.min
      )

    y0_hat <- stats::predict(fit, x_mat)[, 1]
    y1_hat <- stats::predict(fit, x1_mat)[, 1]

    # Check that predict works the right way
    # t(as.matrix(coef(fit))) %*% t(cbind(rep(1, nrow(x1_mat)), x1_mat))

    match <- find_near(y1_hat, y0_hat, n_near)

    if (!reduced) {
      return(
        list(
          x_mat = x_mat,
          w_mat = w_mat,
          y_mat = y_mat,
          x1_mat = x1_mat,
          lambda_cv = lambda_cv,
          fit = fit,
          y0_hat = y0_hat,
          y1_hat = y1_hat,
          match = match
        )
      )
    } else {
      return(list(
        y0_hat = y0_hat,
        y1_hat = y1_hat,
        match = match
      ))
    }
  }
