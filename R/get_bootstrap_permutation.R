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

#' Generates a list of permutation vectors for bootsrapping (old version not used)
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
#' get_bootstrap_permutation_old(df, 3)
get_bootstrap_permutation_old <- function(data, n_boot) {
  if (rlang::is_character(n_boot) ||
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


#' Generates a list of permutation vectors for bootsrapping (old version not used)
#'
#' @param .data dataframe containing two columns \code{group} and \code{id}.
#' @param n_boot number of bootstrap permulations (resamplings).
#'     If \code{n_rep = 0}, no bootstraping is performed and we have the
#'     bootstrap sample, whcih consist of the same observations in the same
#'     order as source data.
#'
#' @export
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(purrr)
#' library(lassopmm)
#'
#' # Generating sample data
#' n <- 100
#' sa_df <-
#'   tibble(
#'     ids = 1:n,
#'     .strata = statar::xtile(ids, 10),
#'     .cluster = c(
#'       rep(5, n * 0.1),
#'       rep(2, n * 0.3),
#'       rep(4, n * 0.4),
#'       rep(3, n * 0.05),
#'       rep(1, n * 0.15)
#'     )
#'   )
#'
#' # Dataset, where cluster equall to strata
#' sa_df1 <- sa_df %>% mutate(.cluster = .strata)
#'
#' # Dataset, where cluster equall to one number
#' sa_df2 <- sa_df %>% mutate(.cluster = 0)
#'
#' # Dataset, where strata equall to one number
#' sa_df3 <- sa_df %>% mutate(.strata = 1)
#'
#' # Function for generating cross-tabulation statistics
#' my_table <- function(x, y) {
#'   a <- table(x, y)
#'   b <- cbind(a, t(t(margin.table(a, 1))))
#'   cc <-
#'     margin.table(a, 2) %>%
#'     as.matrix() %>%
#'     t() %>%
#'     cbind(., sum(.))
#'   rbind(b, cc)
#' }
#'
#' # Example of simple bootstrapping with clusters and stratas
#' # Original data strcructure
#' with(sa_df, my_table(.cluster, .strata))
#' # Structure of the three repetedly sampled datasets
#' get_bootstrap_permutation(sa_df, 3) %>%
#'   map(~ tibble(ids = .x) %>% left_join(sa_df, "ids")) %>%
#'   map(~ with(.x, my_table(.cluster, .strata)))
#'
#' # Example of sampling with clusters and stratas,
#' # where data contains clusters equall to stratas.
#' # This is an equivalent of the only clustered sampling
#' with(sa_df1, my_table(.cluster, .strata))
#' # Structure of the three repetedly sampled datasets
#' get_bootstrap_permutation(sa_df1, 3) %>%
#'   map(~ tibble(ids = .x) %>% left_join(sa_df1, "ids")) %>%
#'   map(~ with(.x, my_table(.cluster, .strata)))
#'
#' # Example of sampling with clusters and stratas,
#' # where data contains only one cluster equall to 0
#' # This is an equivalent of the only stratified sampling
#' with(sa_df2, my_table(.cluster, .strata))
#' # Structure of the three repetedly sampled datasets
#' get_bootstrap_permutation(sa_df2, 3) %>%
#'   map(~ tibble(ids = .x) %>% left_join(sa_df2, "ids")) %>%
#'   map(~ with(.x, my_table(.cluster, .strata)))
#'
#' # Example of sampling with clusters and stratas,
#' # where data contains only one strata equall to 0
#' # This is an equivalent of the only clustered sampling
#' with(sa_df3, my_table(.cluster, .strata))
#' # Structure of the three repetedly sampled datasets
#' get_bootstrap_permutation(sa_df3, 3) %>%
#'   map(~ tibble(ids = .x) %>% left_join(sa_df3, "ids")) %>%
#'   map(~ with(.x, my_table(.cluster, .strata)))
#'
#' # Comparing old bootstrapping logic with the new one
#' df <- tibble::tibble(id = 1:30, group = sort(rep(1:3, 10))) %>%
#'   mutate(ids = id, .strata = group, .cluster = 1)
#'
#' set.seed(1)
#' old <- get_bootstrap_permutation_old(df, 3) %>%
#'   map(~ sort(.x))
#' set.seed(1)
#' new <- get_bootstrap_permutation(df, 3) %>%
#'   map(~ sort(.x))
#' all_equal(old, new)
get_bootstrap_permutation <- function(.data, n_boot = NULL) {
  if (n_boot == 0) {
    purrr::map(1, function(i) {
      purrr::map(.data, function(y) {
        y[[1]]
      }) %>%
        unlist()
    }) %>%
      return()
  }
  unique_clusters <- unique(.data$.cluster)
  purrr::map(
    1:n_boot,
    .f = function(z) {
      if (length(unique_clusters) > 1) {
        random_cluster <-
          unique_clusters %>%
          sample_vector()
      } else {
        random_cluster <-
          unique_clusters
      }

      random_cluster %>%
        purrr::map(
          .f = function(x) {
            one_cluster <-
              dplyr::filter(.data, .cluster == x) %>%
              dplyr::select(.strata, ids)
            unique_stratas <-
              unique(one_cluster$.strata)
            unique_stratas %>%
              purrr::map(
                .f = function(y) {
                  dplyr::filter(one_cluster, .strata == y) %>%
                    dplyr::pull(ids) %>%
                    sample_vector()
                }
              )
          }
        ) %>%
        unlist()
    }
  )
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
