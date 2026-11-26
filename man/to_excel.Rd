\name{to_excel}
\alias{to_excel}
\title{Export Inequality Results to OpenXML Excel (.xlsx)}
\description{
Writes an Excel workbook with zero external package dependencies.
}
\usage{
to_excel(object, path, lang = "en", include_strobe = TRUE)
}
\arguments{
  \item{object}{An \code{esther_result} or \code{esther_decomp} object.}
  \item{path}{Output file path (\code{.xlsx} or \code{.docx}).}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{include_strobe}{Logical; if \code{TRUE}, appends the STROBE-Equity methodological note.}
}
\value{
Invisibly returns the normalized output file path.
}

