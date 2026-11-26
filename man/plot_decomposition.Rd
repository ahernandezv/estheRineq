\name{plot_decomposition}
\alias{plot_decomposition}
\title{Plot Inequality Decomposition Contributions}
\description{
Renders a horizontal bar and 95\% confidence interval forest plot for decomposition factors.
}
\usage{
plot_decomposition(
  decomp_res,
  metric = c("contribution", "pct_share"),
  title = NULL,
  lang = "en",
  backend = "auto"
)
}
\arguments{
  \item{decomp_res}{An \code{esther_decomp} object returned by a decomposition function.}
  \item{metric}{Quantity to plot in decomposition forest plot: \code{'contribution'} or \code{'pct_share'}.}
  \item{title}{Optional custom title string.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{backend}{Plotting backend: \code{'auto'}, \code{'ggplot2'}, or \code{'base'}.}
}
\value{
Invisibly returns the contributions \code{data.frame}.
}

