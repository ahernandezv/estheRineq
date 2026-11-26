\name{indirect_standardize}
\alias{indirect_standardize}
\title{Indirectly Standardized Ratios (SMR) and Rates (ISR)}
\description{
Computes Standardized Mortality/Morbidity Ratios and Indirectly Standardized Rates with exact Garwood or Byar Poisson limits.
}
\usage{
indirect_standardize(
  data,
  events,
  population,
  stratum,
  group,
  ref_rates = NULL,
  multiplier = 1e+05,
  output_type = c("both", "smr", "isr"),
  ci_method = c("exact", "byar"),
  confidence_level = 0.95
)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{survey_health_data} object, \code{grouped_health_data} object, or \code{survey.design} object.}
  \item{events}{Character string naming observed event counts.}
  \item{population}{Character string naming subgroup population denominators or weights.}
  \item{stratum}{Character string naming age/sex standardization strata.}
  \item{group}{Optional character string naming comparison domains or groups.}
  \item{ref_rates}{Optional named numeric vector or data.frame of reference stratum-specific rates.}
  \item{multiplier}{Rate multiplier (default \code{100000} per 100,000 population).}
  \item{output_type}{Output selection for indirect standardization: \code{'both'}, \code{'smr'}, or \code{'isr'}.}
  \item{ci_method}{Confidence interval method (\code{'dobson'}, \code{'byar'}, \code{'exact'}, or \code{'normal'}).}
  \item{confidence_level}{Nominal confidence level in \code{(0, 1)}, default \code{0.95}.}
}
\value{
An object of class \code{esther_result}.
}

