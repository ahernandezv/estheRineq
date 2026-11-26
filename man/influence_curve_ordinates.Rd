\name{influence_curve_ordinates}
\alias{influence_curve_ordinates}
\title{Exact Influence Functions for Concentration and Lorenz Curves}
\description{
Evaluates relative, generalized, or absolute concentration/Lorenz curve ordinates and smoothed influence functions on a grid.
}
\usage{
influence_curve_ordinates(
  y,
  rank = NULL,
  weights = NULL,
  grid = NULL,
  curve_type = c("relative", "generalized", "absolute"),
  rank_order = c("asc", "desc")
)
}
\arguments{
  \item{y}{Numeric vector of health outcome observations.}
  \item{rank}{Character string naming the socioeconomic ranking variable.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{grid}{Numeric vector of cumulative population shares in \code{[0, 1]}.}
  \item{curve_type}{Curve ordinate type: \code{'relative'}, \code{'generalized'}, or \code{'absolute'}.}
  \item{rank_order}{Ordering direction: \code{'asc'} (disadvantaged to advantaged) or \code{'desc'}.}
}
\value{
A list with \code{grid}, \code{ordinates}, and \code{psi_mat}.
}

