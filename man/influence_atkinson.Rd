\name{influence_atkinson}
\alias{influence_atkinson}
\title{Exact Influence Function for Atkinson Inequality Indices}
\description{
Computes the Atkinson index and Equally Distributed Equivalent (EDE) with observation-level influence functions.
}
\usage{
influence_atkinson(y, weights = NULL, epsilon = 1)
}
\arguments{
  \item{y}{Numeric vector of health outcome observations.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{epsilon}{Atkinson inequality-aversion parameter (\code{epsilon >= 0}).}
}
\value{
A list with \code{estimate}, \code{psi}, and \code{ede}.
}

