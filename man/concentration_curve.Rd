\name{concentration_curve}
\alias{concentration_curve}
\title{Estimate Concentration and Lorenz Curves with Confidence Bands}
\description{
Computes relative, generalized, or absolute concentration curves (or Lorenz curves when rank is NULL) with pointwise or simultaneous confidence bands.
}
\usage{
concentration_curve(
  data,
  outcome,
  rank = NULL,
  by = NULL,
  curve_type = c("relative", "generalized", "absolute"),
  n_grid = 21L,
  simultaneous_bands = FALSE,
  rank_order = c("asc", "desc"),
  confidence_level = 0.95,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  lonely_psu = "remove"
)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{survey_health_data} object, \code{grouped_health_data} object, or \code{survey.design} object.}
  \item{outcome}{Character string naming the health outcome variable.}
  \item{rank}{Character string naming the socioeconomic ranking variable.}
  \item{by}{Optional character string naming a domain/subpopulation stratification column.}
  \item{curve_type}{Curve ordinate type: \code{'relative'}, \code{'generalized'}, or \code{'absolute'}.}
  \item{n_grid}{Integer number of evaluation points along \code{[0, 1]}.}
  \item{simultaneous_bands}{Logical; if \code{TRUE}, computes simultaneous Studentized Maximum Modulus bands.}
  \item{rank_order}{Ordering direction: \code{'asc'} (disadvantaged to advantaged) or \code{'desc'}.}
  \item{confidence_level}{Nominal confidence level in \code{(0, 1)}, default \code{0.95}.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{strata}{Optional character string naming survey design strata (or character vector in low-level kernels).}
  \item{psu}{Optional character string naming primary sampling units / clusters (or character vector in low-level kernels).}
  \item{fpc}{Optional character string naming finite population correction values.}
  \item{lonely_psu}{Handling strategy for single-PSU strata: \code{'remove'}, \code{'adjust'}, \code{'certainty'}, or \code{'average'}.}
}
\value{
An object of class \code{esther_curve}.
}

