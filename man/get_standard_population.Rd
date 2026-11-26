\name{get_standard_population}
\alias{get_standard_population}
\title{Retrieve Built-In Epidemiological Standard Populations}
\description{
Returns WHO World (2000-2025), Segi (1960), European (2013), or 5-band WHO broad age standard population weights.
}
\usage{
get_standard_population(name = "who_2000_2025")
}
\arguments{
  \item{name}{Character string identifier for a standard population or color palette.}
}
\value{
A \code{data.frame} with \code{age_group}, \code{std_pop}, and \code{std_weight}.
}

