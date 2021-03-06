% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_poverty.R
\name{detect_poverty}
\alias{detect_poverty}
\title{Classifies variable between thresholds creating 3 groups: poor, vul, mid}
\usage{
detect_poverty(.data, income_var, suffix = "", pov_line_1,
  pov_line_2 = NULL)
}
\arguments{
\item{.data}{data frame to perform calculations on.}

\item{income_var}{variable to use for classification}

\item{suffix}{to add to the neqyly created variables}

\item{pov_line_1, pov_line_2}{values to use as the povery lines. \code{pov_line_1} is required.}
}
\value{
Same data frame with two or three extra columns. When the
        \code{pov_line_2} is NULL, data frame gains columns \code{poor_suffix},
        which is 1 for value of the \code{income_var} below the poverty
        line and \code{mid_suffix}, wchih is the opposite to the first one.
        When both povery lines are supplied, third variable \code{mid_suffix}
        is created, which represents the values of the \code{income_var} in
        between two poverty lines.
}
\description{
Classifies variable between thresholds creating 3 groups: poor, vul, mid
}
