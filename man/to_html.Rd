\name{to_html}
\alias{to_html}
\title{Export Inequality Results to Styled HTML Tables}
\description{
Formats result and decomposition objects as clean HTML fragments.
}
\usage{
to_html(object, lang = "en", digits = 4L, include_strobe = TRUE)
}
\arguments{
  \item{object}{An \code{esther_result} or \code{esther_decomp} object.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{digits}{Integer number of decimal places for formatted tables.}
  \item{include_strobe}{Logical; if \code{TRUE}, appends the STROBE-Equity methodological note.}
}
\value{
An HTML character string.
}

