\name{decompose_rif}
\alias{decompose_rif}
\title{Bivariate Recentered Influence Function (RIF-I-OLS) Decomposition}
\description{
Estimates unconditional marginal effects of covariates on any concentration index via survey-weighted RIF regression.
}
\usage{
decompose_rif(
  data,
  rank,
  formula = NULL,
  outcome = NULL,
  covariates = NULL,
  categorical = NULL,
  method = "erreygers",
  bounds = NULL,
  rank_order = c("asc", "desc"),
  favorable = NULL,
  vce = c("if", "bootstrap"),
  bootstrap_reps = 200L,
  confidence_level = 0.95,
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
  \item{rank}{Character string naming the socioeconomic ranking variable.}
  \item{formula}{Model formula (e.g., \code{stunting ~ mother_edu + area + improved_sanitation}).}
  \item{outcome}{Character string naming the health outcome variable.}
  \item{covariates}{Optional character vector of adjustment covariates.}
  \item{categorical}{Optional character vector of covariates to treat as categorical factors.}
  \item{method}{Estimation or index normalization method.}
  \item{bounds}{Numeric vector \code{c(min, max)} specifying natural bounds for bounded indices.}
  \item{rank_order}{Ordering direction: \code{'asc'} (disadvantaged to advantaged) or \code{'desc'}.}
  \item{favorable}{Optional logical indicating whether higher values of the health indicator are desirable.}
  \item{vce}{Variance estimation mode (\code{'if'}, \code{'convenient'}, \code{'newey'}, \code{'replicate'}, or \code{'bootstrap'}).}
  \item{bootstrap_reps}{Integer number of complex-survey PSU bootstrap replications.}
  \item{confidence_level}{Nominal confidence level in \code{(0, 1)}, default \code{0.95}.}
  \item{seed}{Integer random seed (restores global \code{.Random.seed} on exit).}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{strata}{Optional character string naming survey design strata (or character vector in low-level kernels).}
  \item{psu}{Optional character string naming primary sampling units / clusters (or character vector in low-level kernels).}
  \item{fpc}{Optional character string naming finite population correction values.}
  \item{lonely_psu}{Handling strategy for single-PSU strata: \code{'remove'}, \code{'adjust'}, \code{'certainty'}, or \code{'average'}.}
}
\value{
An object of class \code{esther_decomp}.
}

