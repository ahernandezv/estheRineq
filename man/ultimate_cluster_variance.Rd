\name{ultimate_cluster_variance}
\alias{ultimate_cluster_variance}
\title{Stratified Multistage Ultimate-Cluster Taylor Variance}
\description{
Computes design-based covariance matrices, standard errors, degrees of freedom, and design effects (DEFF) from observation-level influence functions.
}
\usage{
ultimate_cluster_variance(
  psi,
  weights,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  domain_mask = NULL,
  lonely_psu = c("remove", "adjust", "certainty", "average")
)
}
\arguments{
  \item{psi}{Numeric vector or matrix of observation-level influence functions.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{strata}{Optional character string naming survey design strata (or character vector in low-level kernels).}
  \item{psu}{Optional character string naming primary sampling units / clusters (or character vector in low-level kernels).}
  \item{fpc}{Optional character string naming finite population correction values.}
  \item{domain_mask}{Optional logical vector identifying target subpopulation observations.}
  \item{lonely_psu}{Handling strategy for single-PSU strata: \code{'remove'}, \code{'adjust'}, \code{'certainty'}, or \code{'average'}.}
}
\value{
A list with \code{vcov}, \code{se}, \code{df}, \code{n_strata}, \code{n_psu}, and \code{deff}.
}

