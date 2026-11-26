\name{to_word}
\alias{to_word}
\title{Export Inequality Results to OpenXML Word (.docx)}
\description{
Writes a Word document with formatted tables and STROBE-Equity notes with zero external dependencies.
}
\usage{
to_word(object, path, lang = "en", digits = 4L, include_strobe = TRUE)
}
\arguments{
  \item{object}{An \code{esther_result} or \code{esther_decomp} object.}
  \item{path}{Output file path (\code{.xlsx} or \code{.docx}).}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{digits}{Integer number of decimal places for formatted tables.}
  \item{include_strobe}{Logical; if \code{TRUE}, appends the STROBE-Equity methodological note.}
}
\value{
Invisibly returns the normalized output file path.
}

