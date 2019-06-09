#' Impute data to from the period 0 dataframe to the period 1 using "lassopmm" logic.
#'
#' @param source,target dataframes with the period 0 and period 1 data
#'     respectively. These dataframes have to contain same columns with the
#'     identical names and variable types. If this is violated, errors may
#'     appear in the process of running the function.
#' @param dep_var character vector with one name of the dependent variable.
#' @param indep_var character vector with the names of independent variables.
#' @param weight_var character vector with one name of the weight variable.
#'     Default is `NULL`, when `NULL` equall weights of 1 for each obesrcation
#'     are assumed.
#' @param extra_var character vectors. Could be \code{NULL}, contain
#'     one element or a vector of multiple elements. \code{extra_var} represents
#'     names of the variable, which should be joint from the \code{source} data to
#'     the \code{target} data based on the match by the dependent variable \code{dep_var}.
#' @param strata_vars,cluster_vars character vectors. Could be \code{NULL}, contain
#'     one element or a vector of multiple elements. \code{strata_vars} exist for
#'     using stratified sampling in the bootstrapping process. \code{cluster_vars}
#'     used for clastered sampling in the bootstrapping process. Any combinations
#'     of variables could be used.
#' @param force_boot bootstrapping permutation vector externally defined. Default
#'     is \code{NULL}. Has to be provided a dataframe, where each column represent
#'     indexes of the resampled observations for each bootsrtap iteration.
#' @param updateProgress function that updates the progress bars in the shiny app.
#' @inheritParams estimate_matches
#' @inheritParams get_bootstrap_permutation
#'
#' @export
#'
#'
lassopmm <-
  function(source,
             target,
             dep_var,
             indep_var,
             weight_var = NULL,
             extra_var = NULL,
             strata_vars = NULL,
             cluster_vars = NULL,
             n_near = 10,
             n_boot = 5,
             force_boot = NULL,
             force_lambda = NULL,
             n_folds = 10,
             reduced = TRUE,
             updateProgress = NULL) {
    y_mat <- source %>% dplyr::select(dep_var) %>% as.matrix()
    x_mat <- source %>% dplyr::select(indep_var) %>% as.matrix()
    x1_mat <- target %>% dplyr::select(indep_var) %>% as.matrix()

    rownames(y_mat) <- 1:nrow(y_mat)
    rownames(x_mat) <- 1:nrow(x_mat)
    rownames(x1_mat) <- 1:nrow(x1_mat)

    # list of variables to add to the period 1 data from period 0.
    vars_to_bring <- names(source)[names(source) %in% c(dep_var, extra_var)]

    # Here we have to cheack if all selected variables appear in both datasets

    # Checking weighting var
    if (is.na(weight_var) || is.null(weight_var)) {
      message("Default weights equal to all observations are used.")
      w_mat <- matrix(rep(1, nrow(source)), nrow = nrow(source), ncol = 1)
    } else if (!weight_var %in% names(source)) {
      warning(paste0(
        "Weight variable '",
        weight_var,
        "' does not exist in the data.\n",
        "Default weights equal to all observations are used instead."
      ))
      w_mat <- matrix(rep(1, nrow(source)), nrow = nrow(source), ncol = 1)
    } else {
      w_mat <- source %>% dplyr::select(weight_var) %>% as.matrix()
    }
    rownames(w_mat) <- 1:nrow(w_mat)

    # Creating bootstrap groups
    if (any(!strata_vars %in% names(source)) &&
      !all(!strata_vars %in% names(source)) &&
      !is.null(strata_vars) &&
      !is.na(strata_vars)) {
      warning(
        paste0(
          "Variable(s) '",
          paste0(strata_vars[!strata_vars %in% names(source)], collapse = "', '"),
          "' do not exist in the `source` data.\n",
          "Only variable(s) '",
          paste0(strata_vars[strata_vars %in% names(source)], collapse = "', '"),
          "' will be used for creating stratified bootstrap permulation vectors."
        )
      )
    } else if (all(!strata_vars %in% names(source)) &&
      !is.null(strata_vars) &&
      !is.na(strata_vars)) {
      warning(
        paste0(
          "Variable(s) '",
          paste0(strata_vars[!strata_vars %in% names(source)], collapse = "', '"),
          "' do not exist in the `source` data.\n",
          "No grouped bootstrap permulation is applied."
        )
      )
    }
    if (any(!cluster_vars %in% names(source)) &&
      !all(!cluster_vars %in% names(source)) &&
      !is.null(cluster_vars) &&
      !is.na(cluster_vars)) {
      warning(
        paste0(
          "Variable(s) '",
          paste0(cluster_vars[!cluster_vars %in% names(source)], collapse = "', '"),
          "' do not exist in the `source` data.\n",
          "Only variable(s) '",
          paste0(cluster_vars[cluster_vars %in% names(source)], collapse = "', '"),
          "' will be used for creating clusters in the bootstrap permulation vectors."
        )
      )
    } else if (all(!cluster_vars %in% names(source)) &&
      !is.null(cluster_vars) &&
      !is.na(cluster_vars)) {
      warning(
        paste0(
          "Variable(s) '",
          paste0(cluster_vars[!cluster_vars %in% names(source)], collapse = "', '"),
          "' do not exist in the `source` data.\n",
          "No grouped bootstrap permulation is applied."
        )
      )
    }

    group_mat <-
      source %>%
      dplyr::mutate(ids = row_number(), .strata = 1, .cluster = 1)

    # Creating strata variable
    if (any(strata_vars %in% names(source))) {
      group_mat <-
        group_mat %>%
        dplyr::group_by_at(vars(strata_vars[strata_vars %in% names(source)])) %>%
        dplyr::mutate(.strata = dplyr::group_indices()) %>%
        dplyr::ungroup()
    }

    # Creating cluster variable
    if (any(cluster_vars %in% names(source))) {
      group_mat <-
        group_mat %>%
        dplyr::group_by_at(vars(cluster_vars[cluster_vars %in% names(source)])) %>%
        dplyr::mutate(.cluster = group_indices()) %>%
        dplyr::ungroup()
    }

    group_mat <-
      group_mat %>%
      dplyr::select(ids, .strata, .cluster)

    # Getting bootstrap permutation vectors
    if (is.null(force_boot)) {
      boot_perm_vector <- get_bootstrap_permutation(group_mat, n_boot)
    } else {
      boot_perm_vector <- convert_bootstrap_permutation(force_boot)
    }

    if (is.null(updateProgress)) {
      updateProgress <- function(detail = NULL)
        return(NULL)
    }

    # Running all the analysis on every permutation
    all_boots <-
      boot_perm_vector %>%
      purrr::map2(.x = .,
                  .y = seq_along(.),
                  .f = function(.x, .y) {
        updateProgress(detail = paste0("Bootstrap iteration ", .y, "."))
        estimate_matches(
          source_x_mat = x_mat[.x, ],
          source_y_mat = y_mat[.x, ],
          source_w_mat = w_mat[.x, ],
          target_x_mat = x1_mat,
          reduced = reduced,
          n_near = n_near,
          n_folds = n_folds,
          force_lambda = force_lambda
        )
      })

    updateProgress(detail = "Matching observations")

    # constructing dataframe, where all bootstraps are matched to the actual observations
    all_matches <-
      all_boots %>%
      purrr::map("match") %>%
      tibble::enframe(".imp") %>%
      tidyr::unnest() %>%
      dplyr::left_join(
        source %>%
          dplyr::mutate(source_id = as.character(row_number())) %>%
          dplyr::select(source_id, vars_to_bring) %>%
          dplyr::rename_at(dplyr::vars(vars_to_bring), list(~ paste0(., "_source"))),
        by = "source_id"
      )

    target_matches <-
      target %>%
      dplyr::mutate(
        target_id = as.character(row_number()),
        .imp = 0L
      ) %>%
      dplyr::bind_rows(
        dplyr::right_join(
          (.) %>%
            dplyr::select(-.imp),
          all_matches,
          by = "target_id"
        )
      ) %>%
      dplyr::rename(.id = target_id) %>%
      dplyr::select(.imp, .id, tidyselect::everything())

    if (reduced) {
      return(target_matches)
    } else {
      list(
        all_boots = all_boots,
        target_matches = target_matches
      ) %>%
        return()
    }
  }
