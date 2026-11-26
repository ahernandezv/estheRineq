\name{compute_rif}
\alias{compute_rif}
\title{Compute Recentered Influence Function Values}
\description{
Returns RIF = estimate + psi for RIF regressions and decompositions.
}
\usage{
compute_rif(estimate, psi)
}
\arguments{
  \item{estimate}{Character string naming subgroup point estimates.}
  \item{psi}{Numeric vector or matrix of observation-level influence functions.}
}
\value{
A numeric vector of recentered influence function values.
}

