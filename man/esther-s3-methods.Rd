\name{esther-s3-methods}
\alias{esther-s3-methods}
\alias{print.esther_survey}
\alias{print.esther_grouped}
\alias{print.esther_result}
\alias{print.esther_decomp}
\alias{print.esther_curve}
\alias{summary.esther_result}
\alias{summary.esther_decomp}
\alias{as.data.frame.esther_result}
\alias{as.data.frame.esther_decomp}
\alias{as.data.frame.esther_curve}
\alias{coef.esther_result}
\alias{vcov.esther_result}
\alias{confint.esther_result}
\alias{plot.esther_curve}
\alias{plot.esther_decomp}
\title{S3 Methods for estheRineq Containers and Result Objects}
\description{
Standard R S3 methods (\code{print}, \code{summary}, \code{as.data.frame}, \code{coef}, \code{vcov}, \code{confint}, and \code{plot}) for \code{estheRineq} objects.
}
\usage{
\method{print}{esther_survey}(x, ...)

\method{print}{esther_grouped}(x, ...)

\method{print}{esther_result}(x, digits = 4L, lang = "en", ...)

\method{print}{esther_decomp}(x, digits = 4L, lang = "en", ...)

\method{print}{esther_curve}(x, ...)

\method{summary}{esther_result}(object, digits = 4L, lang = "en", ...)

\method{summary}{esther_decomp}(object, digits = 4L, lang = "en", ...)

\method{as.data.frame}{esther_result}(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
)

\method{as.data.frame}{esther_decomp}(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
)

\method{as.data.frame}{esther_curve}(x, row.names = NULL, optional = FALSE, ...)

\method{coef}{esther_result}(object, ...)

\method{vcov}{esther_result}(object, ...)

\method{confint}{esther_result}(object, parm, level = 0.95, ...)

\method{plot}{esther_curve}(x, ...)

\method{plot}{esther_decomp}(x, ...)
}
\arguments{
  \item{x}{Numeric, integer, or ordered factor vector (or S3 object in \code{print}/\code{plot}/\code{as.data.frame} methods).}
  \item{...}{Additional arguments passed to downstream methods.}
  \item{digits}{Integer number of decimal places for formatted tables.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{object}{An \code{esther_result} or \code{esther_decomp} object.}
  \item{row.names}{Optional row names passed to \code{as.data.frame}.}
  \item{optional}{Logical passed to \code{as.data.frame}.}
  \item{parm}{Optional parameter specification for \code{confint}.}
  \item{level}{Confidence level for \code{confint}.}
}
\value{
Depends on the generic: formatted output invisibly returning the input object for \code{print}/\code{summary}, a \code{data.frame} for \code{as.data.frame}, numeric parameter vector for \code{coef}, covariance matrix for \code{vcov}, confidence interval matrix for \code{confint}, and plot output for \code{plot}.
}
