\name{convenient_regression_variance}
\alias{convenient_regression_variance}
\title{Kakwani-Wagstaff-van Doorslaer Convenient Regression Variance}
\description{
Computes convenient-regression standard errors (with optional Newey-West rank-autocorrelation lags) matching Stata conindex.
}
\usage{
convenient_regression_variance(
  y,
  ridit,
  weights,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  scale_factor = 1,
  newey_lags = 0L,
  lonely_psu = "remove"
)
}
\arguments{
  \item{y}{Numeric vector of health outcome observations.}
  \item{ridit}{Numeric vector of midpoint fractional ranks.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{strata}{Optional character string naming survey design strata (or character vector in low-level kernels).}
  \item{psu}{Optional character string naming primary sampling units / clusters (or character vector in low-level kernels).}
  \item{fpc}{Optional character string naming finite population correction values.}
  \item{scale_factor}{Numeric scaling factor applied in convenient regression.}
  \item{newey_lags}{Integer number of rank-order autocorrelation lags for Newey-West variance.}
  \item{lonely_psu}{Handling strategy for single-PSU strata: \code{'remove'}, \code{'adjust'}, \code{'certainty'}, or \code{'average'}.}
}
\value{
A list with \code{vcov}, \code{se}, \code{df}, \code{n_strata}, \code{n_psu}, and \code{deff}.
}

