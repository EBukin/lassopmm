# Simple R script for running lassopmm and estimating mobility


# Loading packages
library(lassopmm)
library(haven)      # For loading data saved in STATA
library(dplyr)      # For manipulating data
library(purrr)   # For manipulating data

# Loading data
source_data <- read_dta("../eduard-stata/ARG/cross 14.dtaweight.dta")
target_data <- read_dta("../eduard-stata/ARG/cross 13.dtaweight.dta")


# Specifying relevant variables

# Dependent variable
dependent_var <- "lipcf"

# Independent variables. Make sure that they are specified as shown below.
independent_vars <-
  c("hombre", "aedu", "miembros", "miembros2",
    "region1", "region2", "region3", "region4", "region5", "region6",
    "hombre_region1", "hombre_region2", "hombre_region3",
    "hombre_region4", "hombre_region5", "hombre_region6",
    "aedu_region1", "aedu_region2", "aedu_region3",
    "aedu_region4", "aedu_region5", "aedu_region6",
    "miembros_region1", "miembros_region2", "miembros_region3",
    "miembros_region4", "miembros_region5", "miembros_region6",
    "miembros2_region1", "miembros2_region2", "miembros2_region3",
    "miembros2_region4", "miembros2_region5", "miembros2_region6",
    "edad", "edad2",
    "edad_region1", "edad_region2", "edad_region3",
    "edad_region4", "edad_region5", "edad_region6",
    "edad2_region1", "edad2_region2", "edad2_region3",
    "edad2_region4", "edad2_region5", "edad2_region6"
  )

# Weight variable
weight_var <- "pondera"

# Number of the bootstrap iterations
n_iterations <- 5

# Number of the nearest observations
n_nearest <- 10


### Running lassopmm
raw_lassopmm <-
  lassopmm(source = source_data,
           target = target_data,
           dep_var = dependent_var,
           indep_var = independent_vars,
           weight_var = weight_var,
           n_near = n_nearest,
           n_boot = n_iterations)


# Converting predicted income to the regular values from logarithm
raw_lassopmm <-
  raw_lassopmm %>%
  mutate(ipcf_source = exp(lipcf_source))

# Initialising poverty lines
pl_1 <- 5.5 * 30.41667
pl_2 <- 13 * 30.41667

# Calculating poverty status for different income groups
poverty_calc <-
  raw_lassopmm %>%
  mutate(ipcf = if_else(.imp == 0, NA_real_, ipcf)) %>%
  detect_poverty(ipcf, "target", pl_1, pl_2) %>%
  detect_poverty(ipcf_source, "source", pl_1, pl_2) %>%
  select(.id, .imp, pondera, contains("ipcf"),
         contains("_target"), -contains("_actual"),
         contains("_source")) %>%
  left_join(
    (.) %>%
      select(matches("\\d.{1,}_source$"), matches("\\d.{1,}_target$")) %>%
      get_all_combinations(mobility)
  )

# Selecting variables that contain poverty status
compare_vars <-
  poverty_calc %>%
  names(.) %>%
  magrittr::extract(stringr::str_detect(., "mobility_")) %>%
  sort()

# Calculating mobility statistics
arg_act_mob <-
  poverty_calc %>%
  get_mi_means_table(compare_vars, "pondera") %>%
  select(-ubar, -t) %>%
  arrange(variable)

# Saving mobility data into a file
write.csv(arg_act_mob, "mobility_statistics.csv")s





