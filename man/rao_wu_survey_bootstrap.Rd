\name{rao_wu_survey_bootstrap}
\alias{rao_wu_survey_bootstrap}
\title{Rao-Wu-Yue Rescaled Complex-Survey PSU Bootstrap}
\description{
Resamples primary sampling units within strata using Rao-Wu-Yue weight rescaling.
}
\usage{
rao_wu_survey_bootstrap(
  estimator_fn,
  weights,
  strata = NULL,
  psu = NULL,
  n_boot = 200L,
  seed = 42L
)
}
\arguments{
  \item{estimator_fn}{Function mapping a numeric weight vector to parameter estimates.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{strata}{Optional character string naming survey design strata (or character vector in low-level kernels).}
  \item{psu}{Optional character string naming primary sampling units / clusters (or character vector in low-level kernels).}
  \item{n_boot}{Integer number of Rao-Wu-Yue rescaled PSU bootstrap replications.}
  \item{seed}{Integer random seed (restores global \code{.Random.seed} on exit).}
}
\value{
A list with \code{pt}, \code{vcov}, \code{se}, \code{df}, \code{n_strata}, and \code{n_psu}.
}

