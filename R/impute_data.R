#' Impute data to from the period 0 dataframe to the period 1 using "lassopmm" logic.
#'
#' @param period_0,period_1 dataframes with the period 0 and period 1 data
#'     respectively. These dataframes have to contain same columns with the
#'     identical names and variable types. If this is violated, errors may
#'     appear in the process of running the function.
#' @param dep_var character vector with one name of the dependent variable.
#' @param indep_var character vector with the names of independent variables.
#' @param weight_var character vector with one name of the weight variable.
#'     Default is `NULL`, when `NULL` equall weights of 1 for each obesrcation
#'     are assumed.
#' @param extra_var,group_boot_var character vectors. Could be \code{NULL}, contain
#'     one element or a vector of multiple lements. \code{extra_var} represents
#'     names of the variable, which should be joint from the \code{period_0} data to
#'     the \code{period_1} data based on the match by the dependent variable \code{dep_var}.
#' @param n_boot number of bootsrtapping iterations.
#' @param force_boot bootstrapping permutation vector externally defined. Default
#'     is \code{NULL}. Has to be provided a dataframe, where each column represent
#'     indexes of the resampled observations for each bootsrtap iteration.
#' @inheritParams estimate_matches
#'
#' @export
#'
#'
impute_data <-
  function(period_0,
             period_1,
             dep_var,
             indep_var,
             weight_var = NULL,
             extra_var = NULL,
             n_near = 10,
             n_boot = 5,
             group_boot_var = NULL,
             force_boot = NULL,
             n_folds = 10,
             reduced = TRUE) {
    y_mat <- period_0 %>% dplyr::select(dep_var) %>% as.matrix()
    x_mat <- period_0 %>% dplyr::select(indep_var) %>% as.matrix()
    x1_mat <- period_1 %>% dplyr::select(indep_var) %>% as.matrix()

    rownames(y_mat) <- 1:nrow(y_mat)
    rownames(x_mat) <- 1:nrow(x_mat)
    rownames(x1_mat) <- 1:nrow(x1_mat)

    # list of variables to add to the period 1 data from period 0.
    vars_to_bring <- names(period_0)[names(period_0) %in% c(dep_var, extra_var)]

    # Here we have to cheack if all selected variables appear in both datasets

    # Checking weighting var
    if (is.na(weight_var) || is.null(weight_var)) {
      message("Default weights equal to all observations are used.")
    } else if (!weight_var %in% names(period_0)) {
      warning(paste0(
        "Weight variable '",
        weight_var,
        "' does not exist in the data.\n",
        "Default weights equal to all observations are used instead."
      ))
      w_mat <- matrix(rep(1, nrow(period_0)), nrow = nrow(period_0), ncol = 1)
    } else {
      w_mat <- period_0 %>% dplyr::select(weight_var) %>% as.matrix()
    }
    rownames(w_mat) <- 1:nrow(w_mat)

    # Creating bootstrap groups
    if (any(!group_boot_var %in% names(period_0)) &&
      !all(!group_boot_var %in% names(period_0)) &&
      !is.null(group_boot_var) &&
      !is.na(group_boot_var)) {
      warning(
        paste0(
          "Variable(s) '",
          paste0(group_boot_var[!group_boot_var %in% names(period_0)], collapse = "', '"),
          "' are not listed in the `period 0` dataset.\n",
          "Only variable(s) '",
          paste0(group_boot_var[group_boot_var %in% names(period_0)], collapse = "', '"),
          "' will be used for creating grouped bootstrap permulation vectors."
        )
      )
    } else if (all(!group_boot_var %in% names(period_0)) &&
      !is.null(group_boot_var) &&
      !is.na(group_boot_var)) {
      warning(
        paste0(
          "Variable(s) '",
          paste0(group_boot_var[!group_boot_var %in% names(period_0)], collapse = "', '"),
          "' are not listed in the `period 0` dataset.\n",
          "No grouped bootstrap permulation is applied."
        )
      )
    }

    if (any(group_boot_var %in% names(period_0))) {
      group_mat <-
        period_0 %>%
        dplyr::mutate(ids = row_number()) %>%
        dplyr::group_by(group_boot_var[group_boot_var %in% names(.)]) %>%
        dplyr::mutate(group = sum(ids)) %>%
        dplyr::ungroup() %>%
        dplyr::select(ids, group)
    } else {
      group_mat <-
        period_0 %>%
        dplyr::mutate(
          ids = row_number(),
          group = 1
        ) %>%
        dplyr::select(ids, group)
      group_boot_var <- "group"
    }

    # Getting bootstrap permutation vectors
    if (is.null(force_boot)) {
      boot_perm_vector <- get_bootstrap_permutation(group_mat, n_boot)
    } else {
      boot_perm_vector <- convert_bootstrap_permutation(force_boot)
    }

    # Running all the analysis on every permutation
    all_boots <-
      boot_perm_vector %>%
      purrr::map(~ estimate_matches(
        x_mat = x_mat[.x, ],
        y_mat = y_mat[.x, ],
        w_mat = w_mat[.x, ],
        x1_mat = x1_mat,
        reduced = reduced,
        n_near = n_near,
        n_folds = n_folds
      ))

    # constructing dataframe, where all bootstraps are matched to the actual observations
    all_matches <-
      all_boots %>%
      purrr::map("match") %>%
      tibble::enframe(".imp") %>%
      tidyr::unnest() %>%
      dplyr::left_join(
        period_0 %>%
          dplyr::mutate(period_0_id = as.character(row_number())) %>%
          dplyr::select(period_0_id, vars_to_bring) %>%
          dplyr::rename_at(dplyr::vars(vars_to_bring), list(~ paste0(., "_period_0"))),
        by = "period_0_id"
      )

    period_1_matches <-
      period_1 %>%
      dplyr::mutate(
        period_1_id = as.character(row_number()),
        .imp = 0L
      ) %>%
      dplyr::bind_rows(
        dplyr::right_join(
          (.) %>%
            dplyr::select(-.imp),
          all_matches,
          by = "period_1_id"
        )
      ) %>%
      dplyr::rename(.id = period_1_id) %>%
      dplyr::select(.imp, .id, tidyselect::everything())

    if (reduced) {
      return(period_1_matches)
    } else {
      list(
        all_boots = all_boots,
        period_1_matches = period_1_matches
      ) %>%
        return()
    }
  }
