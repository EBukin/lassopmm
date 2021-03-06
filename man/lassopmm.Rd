% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lassopmm.R, R/syntheticpanel.R
\docType{package}
\name{lassopmm}
\alias{lassopmm}
\alias{lassopmm-package}
\title{Impute data to from the period 0 dataframe to the period 1 using "lassopmm" logic.}
\usage{
lassopmm(source, target, dep_var, indep_var, weight_var = NULL,
  extra_var = NULL, strata_vars = NULL, cluster_vars = NULL,
  n_near = 10, n_boot = 5, force_boot = NULL, force_lambda = NULL,
  n_folds = 10, reduced = TRUE, updateProgress = NULL)
}
\arguments{
\item{source, target}{dataframes with the period 0 and period 1 data
respectively. These dataframes have to contain same columns with the
identical names and variable types. If this is violated, errors may
appear in the process of running the function.}

\item{dep_var}{character vector with one name of the dependent variable.}

\item{indep_var}{character vector with the names of independent variables.}

\item{weight_var}{character vector with one name of the weight variable.
Default is `NULL`, when `NULL` equall weights of 1 for each obesrcation
are assumed.}

\item{extra_var}{character vectors. Could be \code{NULL}, contain
one element or a vector of multiple elements. \code{extra_var} represents
names of the variable, which should be joint from the \code{source} data to
the \code{target} data based on the match by the dependent variable \code{dep_var}.}

\item{strata_vars, cluster_vars}{character vectors. Could be \code{NULL}, contain
one element or a vector of multiple elements. \code{strata_vars} exist for
using stratified sampling in the bootstrapping process. \code{cluster_vars}
used for clastered sampling in the bootstrapping process. Any combinations
of variables could be used.}

\item{n_near}{number of the nearest observations to derive a random match.
If `n_near` is greater than `length(match_vector)`, minimum out of two is
used to create a sample for selecting a random match value.}

\item{n_boot}{number of bootstrap permulations (resamplings).
If \code{n_rep = 0}, no bootstraping is performed and we have the
bootstrap sample, whcih consist of the same observations in the same
order as source data.}

\item{force_boot}{bootstrapping permutation vector externally defined. Default
is \code{NULL}. Has to be provided a dataframe, where each column represent
indexes of the resampled observations for each bootsrtap iteration.}

\item{force_lambda}{allows to specify one lamda value. Shoul be 0, when we
want to switch to the linear regression.}

\item{n_folds}{number of folds for cross-validation}

\item{reduced}{if TRUE terurns reduced outpur without specific regression details.}

\item{updateProgress}{function that updates the progress bars in the shiny app.}
}
\description{
The package lassopmm
}
