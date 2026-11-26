\name{influence_generalized_entropy}
\alias{influence_generalized_entropy}
\title{Exact Influence Function for Generalized Entropy Indices}
\description{
Computes GE(alpha), including Mean Log Deviation (alpha = 0) and Theil index (alpha = 1), with observation-level influence functions.
}
\usage{
influence_generalized_entropy(y, weights = NULL, alpha = 1)
}
\arguments{
  \item{y}{Numeric vector of health outcome observations.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{alpha}{Generalized entropy parameter (\code{0} for MLD, \code{1} for Theil).}
}
\value{
A list with \code{estimate} and \code{psi}.
}

