\name{plot_curve}
\alias{plot_curve}
\title{Plot Concentration and Lorenz Curves with Confidence Ribbons}
\description{
Renders relative, generalized, or absolute concentration and Lorenz curves with sentence-case dynamic variable labels.
}
\usage{
plot_curve(
  curve_res,
  show_ci = TRUE,
  palette = "lancet",
  title = NULL,
  xlabel = NULL,
  ylabel = NULL,
  lang = "en",
  backend = "auto"
)
}
\arguments{
  \item{curve_res}{An \code{esther_curve} object returned by \code{concentration_curve()}.}
  \item{show_ci}{Logical; if \code{TRUE}, draws shaded confidence bands.}
  \item{palette}{Editorial palette name (\code{'pelotas'}, \code{'lancet'}, \code{'okabe_ito'}, or \code{'who'}).}
  \item{title}{Optional custom title string.}
  \item{xlabel}{Optional custom horizontal axis label.}
  \item{ylabel}{Optional custom vertical axis label.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{backend}{Plotting backend: \code{'auto'}, \code{'ggplot2'}, or \code{'base'}.}
}
\value{
A \code{ggplot} object or invisibly returns coordinate \code{data.frame}.
}

