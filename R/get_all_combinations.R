

#' Convert a table of dummy variables to a table of its unique combinations
#'
#' @param .data data frame with only dummy variable to be converted
#' @param new_var name of the new categorical variable to create for
#'        representing all combinations of the previous dummy variables.
#' @param clean_name,clean_expr,connector technical inputs used for modifying
#'        resulting variables names.
#' @export
#' @examples
#' library(dplyr)
#' library(tibble)
#' tibble(
#'        a = sample(c(0L, 1L), 25, replace = TRUE),
#'        b = sample(c(0L, 1L), 25, replace = TRUE)
#'        ) %>%
#'   get_all_combinations(c)
#'
#'
get_all_combinations <- function(.data, new_var, clean_name = TRUE, clean_expr = "_.{1,}$", connector = "_") {
  new_var <- dplyr::enquo(new_var)
  new_var_name <- rlang::as_name(new_var)

  .data %>%
    dplyr::filter_all(all_vars(!is.na(.))) %>%
    dplyr::distinct() %>%
    {
      combinations <-
        (.) %>%
        rlang::as_list() %>%
        purrr::map2(names(.), ~ case_when(.x == 1 ~ .y)) %>%
        purrr::transpose() %>%
        purrr::map( ~ purrr::keep(.x, ~ !is.na(.x))) %>%
        {
          out <- (.)
          if (clean_name) {
            out <- out %>%
              purrr::map( ~ stringr::str_c(stringr::str_replace_all(.x, clean_expr, ""),
                                           collapse = connector))
          } else {
            out <- out %>%
              purrr::map( ~ stringr::str_c(.x, collapse = connector))
          }
          out
        } %>%
        purrr::map( ~ ifelse(identical(.x, character(0)), NA_character_, .x)) %>%
        unlist()

      (.) %>%
        dplyr::mutate(!!new_var_name := combinations) %>%
        dplyr::mutate(!!new_var_name := as.factor(!!new_var)) %>%
        fastDummies::dummy_cols(new_var_name) %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::contains(stringr::str_c(new_var_name, "_"))),
          list( ~ as.numeric(as.character(.)))
        )
    }
}
