% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_near.R
\name{find_one_near}
\alias{find_one_near}
\title{Internal function to help in matching observations.}
\usage{
find_one_near(o, m, n_near = 5)
}
\arguments{
\item{o}{is a value to match}

\item{m}{is a named vector which is used to find a nearest match to the \code{o}}

\item{n_near}{number of the nearest observations to derive a random match.
If `n_near` is greater than `length(match_vector)`, minimum out of two is
used to create a sample for selecting a random match value.}
}
\value{
It produces a list of four elements:
    \itemize{
      \item `\code{target_id} position of the \code{o} (observation element in the named vector)
      \item \code{target_y_hat} value of the \code{o}
      \item \code{source_id} position of the nearest match in the alternatives vector \code{m}
      \item \code{source_y_hat} value of the nearest match.
      }
}
\description{
Internal function to help in matching observations.
}
\examples{
library(lassopmm)
find_one_near(c("a" = 10), c(8:15), 3)
find_one_near(c("a" = 10), setNames(c(8:15), letters[1:8]), 5)
find_one_near(c(9), setNames(c(8:15), letters[1:8]), 5)
find_one_near(c(10), c(8:15), 5)
}
