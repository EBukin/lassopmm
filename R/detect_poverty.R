
#' Classifies variable between thresholds creating 3 groups: poor, vul, mid
#'
#' @param .data data frame to perform calculations on.
#' @param income_var variable to use for classification
#' @param suffix to add to the neqyly created variables
#' @param pov_line_1,pov_line_2 values to use as the povery lines. \code{pov_line_1} is required.
#'
#' @return Same data frame with two or three extra columns. When the
#'         \code{pov_line_2} is NULL, data frame gains columns \code{poor_suffix},
#'         which is 1 for value of the \code{income_var} below the poverty
#'         line and \code{mid_suffix}, wchih is the opposite to the first one.
#'         When both povery lines are supplied, third variable \code{mid_suffix}
#'         is created, which represents the values of the \code{income_var} in
#'         between two poverty lines.
#' @export
#'
detect_poverty <- function(.data, income_var, suffix = "", pov_line_1, pov_line_2 = NULL) {
  income_var <- dplyr::enquo(income_var)
  poor <- stringr::str_interp("1poor_${suffix}")
  vul <- stringr::str_interp("2vul_${suffix}")
  mid <- stringr::str_interp("3mid_${suffix}")

  if (is_null(pov_line_2)) {
    .data %>%
      dplyr::mutate(
        !!poor := dplyr::case_when(!!income_var < pov_line_1 ~ 1,
                                   !!income_var >= pov_line_1 ~ 0),
        !!mid := dplyr::case_when(!!income_var < pov_line_1 ~ 0,
                                  !!income_var >= pov_line_1 ~ 1)
      ) %>%
      return()
  } else {
    .data %>%
      dplyr::mutate(
        !!poor := dplyr::case_when(!!income_var < pov_line_1 ~ 1,
                                   !!income_var >= pov_line_1 ~ 0),
        !!vul := dplyr::case_when(!!income_var < pov_line_1 ~ 0,
                                  !!income_var >= pov_line_1 & !!income_var < pov_line_2 ~ 1,
                                  !!income_var >= pov_line_2 ~ 0),
        !!mid := dplyr::case_when(!!income_var < pov_line_2 ~ 0,
                                  !!income_var >= pov_line_2 ~ 1)
      ) %>%
      return()
  }
}
