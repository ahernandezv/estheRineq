\name{xtile}
\alias{xtile}
\title{Create Survey-Weighted Quantile Categories}
\description{
Assigns observations to weighted quantile bins (e.g., quintiles or deciles) matching Stata xtile.
}
\usage{
xtile(x, weights = NULL, n_quantiles = 5L, labels = NULL)
}
\arguments{
  \item{x}{Numeric, integer, or ordered factor vector (or S3 object in \code{print}/\code{plot}/\code{as.data.frame} methods).}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{n_quantiles}{Integer number of weighted quantile groups (e.g., \code{5L} for quintiles).}
  \item{labels}{Optional character vector of quantile category labels.}
}
\value{
An integer vector or ordered factor of quantile memberships.
}
\examples{
q <- xtile(1:100, n_quantiles = 5L, labels = paste0('Q', 1:5))
}
