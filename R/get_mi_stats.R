
#' Get multiple imputation mean statistics
#'
#' @param .data dataframe that could be converted to the mids object to perform
#'        calculations on the multiple imputations data frame.
#' @param stat_var variable to apply statistics
#' @param weight_var variable for weighting. NULL by default.
#' @param summary return a summary of the pooled analysis.
#' @param use_random harmonise imputation of the unequal size randomly. By
#'        default if FALSE, therefore, to equalise sizes of the imputed samples,
#'        simplu n-first observations is used.
#' @param ids,imps names of the columns wit the observation-specific ID and
#'        number of imputation.
#'
#' @details Results of these imputations id redived from object of the
#' class \code{mipo} and its summary.
#'
#' The parameters returned represent the follwoing:
#' \tabular{ll}{
#' \code{estimate}\tab Pooled complete data estimate of the mean\cr
#' \code{std.error}\tab Mean's stadnard error\cr
#' \code{statistic}\tab t-statistics\cr
#' \code{p.value} \tab p-value\cr
#' \code{2.5 %}   \tab lower CI\cr
#' \code{97.5 %}  \tab upper CI\cr
#' \code{ubar}    \tab Within-imputation variance of \code{estimate}\cr
#' \code{b}       \tab Between-imputation variance of \code{estimate}\cr
#' \code{t}       \tab Total variance, of \code{estimate}\cr
#' \code{dfcom}   \tab Degrees of freedom in complete data\cr
#' \code{df}      \tab Degrees of freedom of $t$-statistic\cr
#' \code{riv}     \tab Relative increase in variance\cr
#' \code{lambda}  \tab Proportion attributable to the missingness\cr
#' \code{fmi}     \tab Fraction of missing information\cr
#' }
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' dep <- "price"            # Dependent variable
#' indep <- c("mpg", "headroom", "trunk",
#'            "weight", "length", "turn",
#'            "displacement", "gear_ratio",
#'            "foreign")     # independent variables
#' weight <- "weight"        # weight variable
#' extrar <- "displacement"  # any extra variable to bring from period 0 data
#' bt_groups <- c("psu")     # Grouping variable for bootsrapping.
#' n_nearest <- 5    # numebr of the nearest observations to drow a random match
#' set.seed(11344)
#' imputation <-
#'   lassopmm(
#'     source = p_0_exmple,
#'     target = p_1_exmple,
#'     dep_var = dep,
#'     indep_var = indep,
#'     weight_var = weight,
#'     strata_vars = bt_groups,
#'     extra_var = extrar,
#'     n_boot = 5, # Numebr of bootstrap iterations
#'     n_near = 1)
#'
#' # Getting results of the averages
#' get_mi_mean(imputation, stat_var = "price_source", use_random = F)
#'
#' # Same by with weights in means
#' get_mi_mean(imputation, stat_var = "price_source", weight_var = "weight")
#'
get_mi_mean <- function(.data, stat_var, weight_var = NULL, ids = ".id", imps = ".imp", summary = TRUE, use_random = FALSE) {

  extra_name <- names(.data)[! names(.data) %in% c(stat_var, weight_var, ids, imps)] %>% sample(1)
  imputation_groups <- unlist(.data[, imps], use.names = FALSE)
  min_group_size <- min(table(imputation_groups))
  max_group_size <- max(table(imputation_groups))

  if (any(diff(table(imputation_groups))) != 0 && use_random) {
    warning(
      stringr::str_interp(
        stringr::str_c(
          "Number of observations vary across imputations in '${stat_var}'. ",
          "It is set to minimum of ${min_group_size}"
        )
      ),
      call. = FALSE, immediate. = FALSE
    )
    .data <-
      .data %>%
      dplyr::group_by_at(dplyr::vars(imps)) %>%
      dplyr::sample_n(size = min_group_size, replace = F) %>%
      dplyr::ungroup()
  } else if (any(diff(table(imputation_groups))) != 0 && !use_random) {
    warning(
      stringr::str_interp(
        stringr::str_c(
          "Number of observations vary across imputations in '${stat_var}'. ",
          "First ${min_group_size} observations are used."
        )
      ),
      call. = FALSE, immediate. = FALSE
    )
    .data <-
      .data %>%
      dplyr::group_by_at(dplyr::vars(imps)) %>%
      dplyr::filter(dplyr::row_number() <= min_group_size) %>%
      dplyr::ungroup()
  }

  if (is.null(weight_var)) {
    result <-
      .data %>%
      dplyr::select_at(dplyr::vars(stat_var, ids, imps, extra_name)) %>%
      mice::as.mids(.imp = imps, .id = ids) %>%
      with(stats::lm(stats::formula(paste0(stat_var, "~1")))) %>%
      mice::pool()
  } else {
    result <-
      .data %>%
      dplyr::select_at(dplyr::vars(stat_var, weight_var, ids, imps, extra_name)) %>%
      mice::as.mids(.imp = imps, .id = ids) %>%
      with(stats::lm(stats::formula(paste0(stat_var, "~1")),
        weights = eval(parse(text = weight_var))
      )) %>%
      mice::pool()
  }
  if (summary) {
    result %>%
      summary(conf.int = TRUE, df = FALSE, type = "all") %>%
      as.data.frame() %>%
      magrittr::set_rownames(stat_var) %>%
      tibble::rownames_to_column("variable") %>%
      tibble::as_tibble() %>%
      return()
  } else {
    return(result)
  }
}




#' Get multiple imputation mean statistics using gouping variable.
#'
#' @param full_data ataframe that could be converted to the mids object to perform
#'        calculations on the multiple imputations data frame.
#' @param group_var variable for grupping
#' @inheritParams get_mi_mean
#'
#' @export
get_mi_mean_by_group <-
  function(full_data,
             stat_var,
             group_var,
             weight_var = NULL,
             ids = ".id",
             imps = ".imp",
             use_random = FALSE) {
    extra_name <- names(full_data)[! names(full_data) %in% c(stat_var, weight_var, group_var, ids, imps)][1]
    full_data %>%
      dplyr::select_at(dplyr::vars(group_var, stat_var, weight_var, ids, imps, extra_name)) %>%
      dplyr::arrange_at(dplyr::vars(group_var)) %>%
      dplyr::filter_at(dplyr::vars(group_var), dplyr::all_vars(!is.na(.))) %>%
      dplyr::group_by_at(dplyr::vars(group_var)) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        data =
          purrr::map(
            data,
            ~ check_original_data(.x, full_data)
          )
      ) %>%
      dplyr::mutate(
        data =
          purrr::map(
            data,
            ~ get_mi_mean(.x, stat_var, weight_var, ids, imps, use_random = use_random)
          )
      ) %>%
      tidyr::unnest()
  }

#' Internal function for checking presence of the "missing data"
#'
#' @param incomplete_data,full_data datasets with incomplete and full data for comparison
#' @inheritParams get_mi_mean
#' @export
check_original_data <-
  function(incomplete_data,
             full_data,
             ids = ".id",
             imps = ".imp") {
    incomplete_ids <-
      incomplete_data %>%
      dplyr::distinct_at(dplyr::vars(ids)) %>%
      dplyr::pull()

    full_data <-
      full_data %>%
      dplyr::select_at(dplyr::vars(names(incomplete_data))) %>%
      dplyr::filter_at(dplyr::vars(imps), dplyr::all_vars(. == 0)) %>%
      dplyr::filter_at(dplyr::vars(ids), dplyr::all_vars(. %in% incomplete_ids))

    available_origin_data <-
      incomplete_data %>%
      dplyr::filter_at(dplyr::vars(imps), dplyr::all_vars(. == 0))

    imputation_groups <- unlist(incomplete_data[, imps], use.names = FALSE)
    min_group_size <- min(table(imputation_groups[imputation_groups != 0]))

    if (nrow(available_origin_data) == 0 || nrow(available_origin_data) < min_group_size) {
      warning(stringr::str_interp(
        stringr::str_c(
          "Missing data was not available for given subset. ",
          "${min_group_size} missing data points were added"
        )
      ), call. = FALSE, immediate. = FALSE)

      dplyr::bind_rows(full_data, incomplete_data) %>%
        return()
    } else {
      return(incomplete_data)
    }
  }


#' Get multiple imputation mean statistics for several variables
#'
#' @param stat_variables_vec is the atomic vector with variables names
#' @inheritParams get_mi_mean
#' @export
#'
get_mi_means_table <-
  function(.data,
           stat_variables_vec,
           weight_var = NULL, ids = ".id", imps = ".imp", use_random = FALSE) {
    .data <-
      .data %>%
      select_at(vars(stat_variables_vec, weight_var, ids, imps))
    stat_variables_vec %>%
      map_dfr(~get_mi_mean(.data, .x, weight_var, ids, imps, summary = TRUE, use_random))

  }
