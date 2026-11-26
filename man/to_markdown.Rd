\name{to_markdown}
\alias{to_markdown}
\title{Export Inequality Results to Formatted Markdown Tables}
\description{
Formats result and decomposition objects as aligned Markdown tables with optional STROBE-Equity notes.
}
\usage{
to_markdown(object, lang = "en", digits = 4L, include_strobe = TRUE)
}
\arguments{
  \item{object}{An \code{esther_result} or \code{esther_decomp} object.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{digits}{Integer number of decimal places for formatted tables.}
  \item{include_strobe}{Logical; if \code{TRUE}, appends the STROBE-Equity methodological note.}
}
\value{
A Markdown-formatted character string.
}

