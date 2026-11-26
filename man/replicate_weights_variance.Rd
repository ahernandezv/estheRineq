\name{replicate_weights_variance}
\alias{replicate_weights_variance}
\title{Survey Replicate-Weights Variance Estimator}
\description{
Computes design-based variance across BRR, Fay, Jackknife, or bootstrap replicate weight columns.
}
\usage{
replicate_weights_variance(
  estimator_fn,
  full_weights,
  rep_weights_mat,
  rep_type = "BRR",
  rep_scale = NULL,
  fay_rho = 0.5
)
}
\arguments{
  \item{estimator_fn}{Function mapping a numeric weight vector to parameter estimates.}
  \item{full_weights}{Numeric vector of full-sample weights.}
  \item{rep_weights_mat}{Numeric matrix of replicate weights.}
  \item{rep_type}{Replicate weight scheme (\code{'BRR'}, \code{'FAY'}, \code{'JK1'}, \code{'JKn'}, or \code{'BOOTSTRAP'}).}
  \item{rep_scale}{Optional custom scalar multiplier for replicate weight variance.}
  \item{fay_rho}{Fay's BRR perturbation factor in \code{[0, 1)}.}
}
\value{
A list with \code{vcov}, \code{se}, \code{df}, \code{n_strata}, \code{n_psu}, and \code{deff}.
}

