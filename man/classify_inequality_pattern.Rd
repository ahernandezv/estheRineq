\name{classify_inequality_pattern}
\alias{classify_inequality_pattern}
\title{Classify Ordered Subgroup Profiles into Pelotas Inequality Patterns}
\description{
Classifies an ordered profile into Top inequality, Bottom inequality, or Linear gradient.
}
\usage{
classify_inequality_pattern(values, favorable = TRUE, lang = "en")
}
\arguments{
  \item{values}{Numeric vector of ordered subgroup estimates.}
  \item{favorable}{Optional logical indicating whether higher values of the health indicator are desirable.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
}
\value{
A sentence-case character string describing the inequality pattern.
}

