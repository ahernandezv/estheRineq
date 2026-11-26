\name{survey_mean}
\alias{survey_mean}
\title{Estimate Complex-Survey Means and Proportions with Logit CIs}
\description{
Computes survey-weighted means or prevalences, standard errors, logit confidence intervals, coefficients of variation, and design effects.
}
\usage{
survey_mean(
  data,
  outcome,
  by = NULL,
  scale = 1,
  proportion_ci = TRUE,
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
  \item{by}{Optional character string naming a domain/subpopulation stratification column.}
  \item{scale}{Numeric scale multiplier (e.g., \code{1} for proportions, \code{100} for percentages).}
  \item{proportion_ci}{Logical; if \code{TRUE}, uses logit-transformed confidence intervals for proportions.}
  \item{confidence_level}{Nominal confidence level in \code{(0, 1)}, default \code{0.95}.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{strata}{Optional character string naming survey design strata (or character vector in low-level kernels).}
  \item{psu}{Optional character string naming primary sampling units / clusters (or character vector in low-level kernels).}
  \item{fpc}{Optional character string naming finite population correction values.}
  \item{lonely_psu}{Handling strategy for single-PSU strata: \code{'remove'}, \code{'adjust'}, \code{'certainty'}, or \code{'average'}.}
}
\value{
An object of class \code{esther_result}.
}

