\name{sii_rii}
\alias{sii_rii}
\title{Estimate Epidemiological Slope and Relative Indices of Inequality}
\description{
Estimates SII and RII via survey-weighted logistic G-computation marginal standardization, Moreno-Betancur GLMs, or weighted least squares.
}
\usage{
sii_rii(
  data,
  outcome = NULL,
  rank = NULL,
  method = c("logit_marginal", "moreno_betancur", "linear"),
  covariates = NULL,
  categorical = NULL,
  strata_for_ridit = NULL,
  rank_order = c("asc", "desc"),
  favorable = NULL,
  scale = 1,
  confidence_level = 0.95,
  n_sim = 2000L,
  seed = 42L,
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
  \item{method}{Estimation or index normalization method.}
  \item{covariates}{Optional character vector of adjustment covariates.}
  \item{categorical}{Optional character vector of covariates to treat as categorical factors.}
  \item{strata_for_ridit}{Optional character string naming strata within which ridit ranks are computed separately.}
  \item{rank_order}{Ordering direction: \code{'asc'} (disadvantaged to advantaged) or \code{'desc'}.}
  \item{favorable}{Optional logical indicating whether higher values of the health indicator are desirable.}
  \item{scale}{Numeric scale multiplier (e.g., \code{1} for proportions, \code{100} for percentages).}
  \item{confidence_level}{Nominal confidence level in \code{(0, 1)}, default \code{0.95}.}
  \item{n_sim}{Integer number of Monte Carlo draws for grouped uncertainty propagation.}
  \item{seed}{Integer random seed (restores global \code{.Random.seed} on exit).}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{strata}{Optional character string naming survey design strata (or character vector in low-level kernels).}
  \item{psu}{Optional character string naming primary sampling units / clusters (or character vector in low-level kernels).}
  \item{fpc}{Optional character string naming finite population correction values.}
  \item{lonely_psu}{Handling strategy for single-PSU strata: \code{'remove'}, \code{'adjust'}, \code{'certainty'}, or \code{'average'}.}
}
\value{
An object of class \code{esther_result}.
}

