\name{direct_standardize}
\alias{direct_standardize}
\title{Directly Standardized Rates with Dobson and Byar Confidence Intervals}
\description{
Computes age/stratum-standardized rates with exact Poisson-Gamma confidence intervals matching UKHSA PHEindicatormethods.
}
\usage{
direct_standardize(
  data,
  events,
  population,
  stratum,
  group = NULL,
  standard_pop = "who_2000_2025",
  std_pop_col = NULL,
  multiplier = 1e+05,
  ci_method = c("dobson", "byar", "normal"),
  confidence_level = 0.95
)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{survey_health_data} object, \code{grouped_health_data} object, or \code{survey.design} object.}
  \item{events}{Character string naming observed event counts.}
  \item{population}{Character string naming subgroup population denominators or weights.}
  \item{stratum}{Character string naming age/sex standardization strata.}
  \item{group}{Optional character string naming comparison domains or groups.}
  \item{standard_pop}{Standard population name (\code{'who_2000_2025'}, \code{'segi_1960'}, \code{'european_2013'}, \code{'who_broad_5'}), named numeric vector, or data.frame.}
  \item{std_pop_col}{Optional column name containing standard population weights inside \code{data}.}
  \item{multiplier}{Rate multiplier (default \code{100000} per 100,000 population).}
  \item{ci_method}{Confidence interval method (\code{'dobson'}, \code{'byar'}, \code{'exact'}, or \code{'normal'}).}
  \item{confidence_level}{Nominal confidence level in \code{(0, 1)}, default \code{0.95}.}
}
\value{
An object of class \code{esther_result}.
}

