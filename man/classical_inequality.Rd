\name{classical_inequality}
\alias{classical_inequality}
\title{Estimate Classical Univariate Inequality Indices}
\description{
Computes Gini, S-Gini, Atkinson (and EDE), Generalized Entropy (MLD, Theil), Palma, and S80/S20 ratios with complex-survey standard errors.
}
\usage{
classical_inequality(
  data,
  outcome,
  indices = c("gini", "atkinson", "ge", "palma", "s80s20"),
  atkinson_eps = 1,
  ge_alpha = c(0, 1),
  sgini_v = 2,
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
  \item{indices}{Character vector of classical inequality indices to compute.}
  \item{atkinson_eps}{Numeric vector of Atkinson \code{epsilon} values.}
  \item{ge_alpha}{Numeric vector of generalized entropy \code{alpha} values.}
  \item{sgini_v}{Numeric S-Gini parameter \code{v}.}
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

