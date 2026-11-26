\name{influence_concentration_index}
\alias{influence_concentration_index}
\title{Exact Gateaux Influence Function for Concentration Indices}
\description{
Computes point estimates and O(N log N) observation-level influence functions for standard, generalized, modified, Wagstaff, Erreygers, extended, and symmetric concentration indices.
}
\usage{
influence_concentration_index(
  y,
  rank,
  weights = NULL,
  method = "standard",
  bounds = NULL,
  v = 2,
  beta = 2,
  rank_order = c("asc", "desc")
)
}
\arguments{
  \item{y}{Numeric vector of health outcome observations.}
  \item{rank}{Character string naming the socioeconomic ranking variable.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{method}{Estimation or index normalization method.}
  \item{bounds}{Numeric vector \code{c(min, max)} specifying natural bounds for bounded indices.}
  \item{v}{Extended concentration index inequality-aversion parameter (\code{v > 1}).}
  \item{beta}{Symmetric concentration index parameter (\code{beta > 1}).}
  \item{rank_order}{Ordering direction: \code{'asc'} (disadvantaged to advantaged) or \code{'desc'}.}
}
\value{
A list with \code{estimate}, \code{psi}, \code{mu}, and \code{ridit}.
}

