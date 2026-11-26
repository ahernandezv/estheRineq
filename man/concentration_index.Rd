\name{concentration_index}
\alias{concentration_index}
\title{Estimate Rank-Dependent Health Inequality Concentration Indices}
\description{
Estimates standard, generalized, modified, Wagstaff, Erreygers, extended, and symmetric concentration indices with complex-survey inference and domain equality Wald tests.
}
\usage{
concentration_index(
  data,
  outcome = NULL,
  rank = NULL,
  method = "erreygers",
  bounds = NULL,
  v = 2,
  beta = 2,
  rank_order = c("asc", "desc"),
  favorable = NULL,
  by = NULL,
  vce = c("if", "convenient", "newey", "replicate", "bootstrap"),
  newey_lags = 1L,
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
  \item{outcome}{Character string naming the health outcome variable.}
  \item{rank}{Character string naming the socioeconomic ranking variable.}
  \item{method}{Estimation or index normalization method.}
  \item{bounds}{Numeric vector \code{c(min, max)} specifying natural bounds for bounded indices.}
  \item{v}{Extended concentration index inequality-aversion parameter (\code{v > 1}).}
  \item{beta}{Symmetric concentration index parameter (\code{beta > 1}).}
  \item{rank_order}{Ordering direction: \code{'asc'} (disadvantaged to advantaged) or \code{'desc'}.}
  \item{favorable}{Optional logical indicating whether higher values of the health indicator are desirable.}
  \item{by}{Optional character string naming a domain/subpopulation stratification column.}
  \item{vce}{Variance estimation mode (\code{'if'}, \code{'convenient'}, \code{'newey'}, \code{'replicate'}, or \code{'bootstrap'}).}
  \item{newey_lags}{Integer number of rank-order autocorrelation lags for Newey-West variance.}
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
An object of class \code{esther_result}.
}
\examples{
df <- load_survey_mch(n = 300)
res <- concentration_index(
  df,
  outcome = 'stunting',
  rank = 'wealth_score',
  method = 'erreygers',
  weights = 'sample_weight',
  strata = 'stratum_id',
  psu = 'cluster_id'
)
}
