\name{strobe_equity_note}
\alias{strobe_equity_note}
\title{Generate Automated STROBE-Equity (2025) Methodological Notes}
\description{
Constructs a standardized methodological footnote summarizing indicator polarity, ranking, complex survey design, DEFF, and variance method.
}
\usage{
strobe_equity_note(object, lang = "en")
}
\arguments{
  \item{object}{An \code{esther_result} or \code{esther_decomp} object.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
}
\value{
A character string in English, Spanish, or Portuguese.
}

