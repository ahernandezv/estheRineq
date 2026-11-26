\name{to_latex}
\alias{to_latex}
\title{Export Inequality Results to Booktabs LaTeX Tables}
\description{
Formats result and decomposition objects as publication-ready booktabs LaTeX tables.
}
\usage{
to_latex(object, lang = "en", digits = 4L, caption = NULL, label = NULL)
}
\arguments{
  \item{object}{An \code{esther_result} or \code{esther_decomp} object.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{digits}{Integer number of decimal places for formatted tables.}
  \item{caption}{Optional LaTeX table caption override.}
  \item{label}{Character string naming unit labels (or LaTeX table label).}
}
\value{
A LaTeX character string.
}

