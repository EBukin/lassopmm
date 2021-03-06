
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lassopmm

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of lassopmm is to replicate functionality of the STATA program
“lassopmm”.

## Installation

You can install development version from
[GitHub](https://github.com/EBukin/lassopmm) with:

``` r
# install.packages("devtools")
devtools::install_github("EBukin/lassopmm")
```

## Example of the basic use

To show how this package work, we will use same example used in the
‘lassopmm’ program does in stata. For that reason, the package
contains three data frames extracted from stata: `p_0_exmple`,
`p_1_exmple` and `sample_bootstrap`. We will use them to reproduce
similar results as the `lassopmm` help file.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(purrr)
library(lassopmm)

glimpse(p_0_exmple)       # help(p_0_exmple)
#> Observations: 74
#> Variables: 16
#> $ make         <chr> "AMC Concord", "AMC Pacer", "AMC Spirit", "Buick ...
#> $ price        <dbl> 4099, 4749, 3799, 4816, 7827, 5788, 4453, 5189, 1...
#> $ mpg          <dbl> 22, 17, 22, 20, 15, 18, 26, 20, 16, 19, 14, 14, 2...
#> $ rep78        <dbl> 3, 3, NA, 3, 4, 3, NA, 3, 3, 3, 3, 2, 3, 3, 4, 3,...
#> $ headroom     <dbl> 2.5, 3.0, 3.0, 4.5, 4.0, 4.0, 3.0, 2.0, 3.5, 3.5,...
#> $ trunk        <dbl> 11, 11, 12, 16, 20, 21, 10, 16, 17, 13, 20, 16, 1...
#> $ weight       <dbl> 2930, 3350, 2640, 3250, 4080, 3670, 2230, 3280, 3...
#> $ length       <dbl> 186, 173, 168, 196, 222, 218, 170, 200, 207, 200,...
#> $ turn         <dbl> 40, 40, 35, 40, 43, 43, 34, 42, 43, 42, 44, 43, 4...
#> $ displacement <dbl> 121, 258, 121, 196, 350, 231, 304, 196, 231, 231,...
#> $ gear_ratio   <dbl> 3.58, 2.53, 3.08, 2.93, 2.41, 2.73, 2.87, 2.93, 2...
#> $ foreign      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
#> $ psu          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2...
#> $ numobs       <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15...
#> $ samples      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
#> $ numobs11     <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15...
glimpse(p_1_exmple)       # help(p_1_exmple)
#> Observations: 7
#> Variables: 16
#> $ make         <chr> "Renault Le Car", "VW Diesel", "Merc. Monarch", "...
#> $ price        <dbl> NA, NA, NA, NA, NA, NA, NA
#> $ mpg          <dbl> 26, 41, 18, 28, 19, 17, 12
#> $ rep78        <dbl> 3, 5, 3, 4, 3, 2, 3
#> $ headroom     <dbl> 3.0, 3.0, 3.0, 1.5, 2.0, 4.5, 2.5
#> $ trunk        <dbl> 10, 15, 15, 9, 16, 21, 18
#> $ weight       <dbl> 1830, 2040, 3370, 1800, 3210, 3740, 4720
#> $ length       <dbl> 142, 155, 198, 147, 201, 220, 230
#> $ turn         <dbl> 34, 35, 41, 33, 45, 46, 48
#> $ displacement <dbl> 79, 90, 250, 98, 231, 225, 400
#> $ gear_ratio   <dbl> 3.72, 3.78, 2.43, 3.15, 2.93, 2.94, 2.47
#> $ foreign      <dbl> 1, 1, 0, 0, 0, 0, 0
#> $ psu          <dbl> 7, 7, 4, 3, 5, 3, 3
#> $ numobs       <dbl> 65, 71, 32, 24, 49, 23, 27
#> $ samples      <dbl> 1, 1, 1, 1, 1, 1, 1
#> $ numobs11     <dbl> 75, 76, 77, 78, 79, 80, 81
glimpse(sample_bootstrap) # help(sample_bootstrap)
#> Observations: 73
#> Variables: 5
#> $ ...1 <dbl> 1, 8, 4, 6, 5, 9, 4, 3, 2, 15, 17, 12, 10, 12, 13, 16, 19...
#> $ ...2 <dbl> 1, 2, 7, 4, 3, 9, 6, 6, 9, 10, 16, 19, 19, 16, 10, 16, 16...
#> $ ...3 <dbl> 7, 6, 9, 2, 1, 9, 4, 3, 4, 10, 11, 14, 16, 16, 10, 13, 10...
#> $ ...4 <dbl> 8, 4, 6, 3, 5, 4, 5, 1, 2, 18, 10, 18, 12, 19, 12, 15, 10...
#> $ ...5 <dbl> 8, 5, 4, 3, 1, 8, 3, 2, 3, 12, 10, 12, 17, 16, 15, 11, 13...

dep <- "price"            # Dependent variable
indep <- c("mpg", "headroom", "trunk",
           "weight", "length", "turn",
           "displacement", "gear_ratio",
           "foreign")     # independent variables
weight <- "weight"        # weight variable
extrar <- "displacement"  # any extra variable to bring from period 0 data
bt_groups <- c("psu")     # Grouping variable for bootsrapping.
                          # May be a combination of variables.
n_nearest <- 1    # numebr of the nearest observations to drow a random match
set.seed(11223344)
imputation <-
  lassopmm(
              source = p_0_exmple, 
              target = p_1_exmple,
              dep_var = dep, indep_var = indep, weight_var = weight,
              cluster_vars = bt_groups,
              extra_var = extrar,
              n_boot = 5, # Numebr of bootstrap iterations
              n_near = 1)
```

In the basic layout, the function returns us a long-structured data
frame, which is built up on the `period_1` input to the function.
Resulting data frame contains essential variables `.imp` and ’.id`.
Variable`.imp`represents the number of the bootstrap iteration (`.imp ==
0`is the originl data) and`.id\` represents the unique ID of the
observation from the period 1.

There are several new variables added in the `period_1` data frame in
the `imputation` form. These are:

  - `target_y_hat` - predicted values for each `.id` in the period 1,
    using id-specific independent variables and lasso regression
    parameters estimated based on the ‘.imp’-specific bootstrap
    sub-sample from the *source* period. `source_y_hat` is the same, but
    for the period 0.
  - `source_id` - id of the observation from the period 0, which is the
    nearest match for the `target_y_hat` from the `source_y_hat`
    estimated on a separate bootstrapped sub-sample according to the
    `.imp`.
  - `price_source` - is the depended variable from the period 0 matched
    to the period 1.
  - `displacement_source` and any other variable such as `*_source` are
    the variables which we specify to extract from the period 0 using
    the parameter `extra_var`.

<!-- end list -->

``` r
glimpse(imputation)
#> Observations: 42
#> Variables: 23
#> $ .imp                <int> 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, ...
#> $ .id                 <chr> "1", "2", "3", "4", "5", "6", "7", "1", "2...
#> $ make                <chr> "Renault Le Car", "VW Diesel", "Merc. Mona...
#> $ price               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
#> $ mpg                 <dbl> 26, 41, 18, 28, 19, 17, 12, 26, 41, 18, 28...
#> $ rep78               <dbl> 3, 5, 3, 4, 3, 2, 3, 3, 5, 3, 4, 3, 2, 3, ...
#> $ headroom            <dbl> 3.0, 3.0, 3.0, 1.5, 2.0, 4.5, 2.5, 3.0, 3....
#> $ trunk               <dbl> 10, 15, 15, 9, 16, 21, 18, 10, 15, 15, 9, ...
#> $ weight              <dbl> 1830, 2040, 3370, 1800, 3210, 3740, 4720, ...
#> $ length              <dbl> 142, 155, 198, 147, 201, 220, 230, 142, 15...
#> $ turn                <dbl> 34, 35, 41, 33, 45, 46, 48, 34, 35, 41, 33...
#> $ displacement        <dbl> 79, 90, 250, 98, 231, 225, 400, 79, 90, 25...
#> $ gear_ratio          <dbl> 3.72, 3.78, 2.43, 3.15, 2.93, 2.94, 2.47, ...
#> $ foreign             <dbl> 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, ...
#> $ psu                 <dbl> 7, 7, 4, 3, 5, 3, 3, 7, 7, 4, 3, 5, 3, 3, ...
#> $ numobs              <dbl> 65, 71, 32, 24, 49, 23, 27, 65, 71, 32, 24...
#> $ samples             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
#> $ numobs11            <dbl> 75, 76, 77, 78, 79, 80, 81, 75, 76, 77, 78...
#> $ target_y_hat        <dbl> NA, NA, NA, NA, NA, NA, NA, 4599.3449, 374...
#> $ source_id           <chr> NA, NA, NA, NA, NA, NA, NA, "65", "71", "1...
#> $ source_y_hat        <dbl> NA, NA, NA, NA, NA, NA, NA, 4599.345, 3744...
#> $ price_source        <dbl> NA, NA, NA, NA, NA, NA, NA, 3895, 5397, 39...
#> $ displacement_source <dbl> NA, NA, NA, NA, NA, NA, NA, 79, 90, 250, 1...

# Summary of the number of observations per one ID
# 6 in total meaning that 1 observation stands for original
# non imputed data and others are bootstrap imputations
imputation %>% group_by(.id) %>% count()
#> # A tibble: 7 x 2
#> # Groups:   .id [7]
#>   .id       n
#>   <chr> <int>
#> 1 1         6
#> 2 2         6
#> 3 3         6
#> 4 4         6
#> 5 5         6
#> 6 6         6
#> 7 7         6

# Summary of the number of observations per imputation. 7 - consisten
# 0 imputation is the original data.
imputation %>% group_by(.imp) %>% count()
#> # A tibble: 6 x 2
#> # Groups:   .imp [6]
#>    .imp     n
#>   <int> <int>
#> 1     0     7
#> 2     1     7
#> 3     2     7
#> 4     3     7
#> 5     4     7
#> 6     5     7
```

## Using multiple imputation logic

Similarly to the `mi` environment in stata, we can use here multiple
imputation techniques for estimating summary statistics of the newly
imputed data. This is possible using the
[mice](https://cran.r-project.org/package=mice) package. Workflow and
logic of this package is well explained in this
[book](https://stefvanbuuren.name/fimd/workflow.html).

First, we need to convert data frame to the `mids` object. Then we used
`mice::with()` to apply specific statistics to each single bootstrap
iteration. With `mice::pool()` we pool statistics results together. We
use `summary()` to extract more user-friendly statistics from the pooled
results.

As straightforward statistics such as `mean()` or `sd()` is slightly
more sophisticated with the multiple imputed data, we use linear model
to derive mean values of the imputed observations in the variable
`price_source`.

``` r
library(mice)
#> Loading required package: lattice
#> 
#> Attaching package: 'mice'
#> The following objects are masked from 'package:base':
#> 
#>     cbind, rbind

mi_test <- as.mids(imputation) # Converting imputated data to the "mids" object
#> Warning in data.matrix(x): NAs introduced by coercion
#> Warning: Number of logged events: 1
mean_stats <- with(mi_test, lm(price_source ~ 1))
est <- pool(mean_stats) # poolling results 
summary(est) # returns mean and standard error for pooled multipuly imputed data.
#>             estimate std.error statistic       df    p.value
#> (Intercept) 6548.229   1637.87  3.998015 2.795263 0.03194716
est          # returns additional data 
#> Class: mipo    m = 5 
#>             estimate    ubar        b       t dfcom       df       riv
#> (Intercept) 6548.229 1753775 774035.4 2682618     6 2.795263 0.5296246
#>                lambda       fmi
#> (Intercept) 0.3462448 0.5718619
```

## R script for using lassopmm

``` r
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
```
