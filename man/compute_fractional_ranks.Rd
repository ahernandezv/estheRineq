\name{compute_fractional_ranks}
\alias{compute_fractional_ranks}
\title{Compute Tie-Adjusted Midpoint Fractional Ranks (Ridit Scores)}
\description{
Computes weighted midpoint fractional ranks and cumulative tie-interval bounds invariant to input row order.
}
\usage{
compute_fractional_ranks(x, weights = NULL, rank_order = c("asc", "desc"))
}
\arguments{
  \item{x}{Numeric, integer, or ordered factor vector (or S3 object in \code{print}/\code{plot}/\code{as.data.frame} methods).}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{rank_order}{Ordering direction: \code{'asc'} (disadvantaged to advantaged) or \code{'desc'}.}
}
\value{
A list containing \code{ridit}, \code{f_low}, \code{f_high}, \code{group_share}, \code{inv_idx}, \code{w_norm}, and \code{n_groups}.
}
\examples{
r <- compute_fractional_ranks(c(10, 20, 20, 40), weights = c(1, 2, 1, 1))
}
