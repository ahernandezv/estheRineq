\name{decompose_oaxaca_rif}
\alias{decompose_oaxaca_rif}
\title{Counterfactual Blinder-Oaxaca RIF Inequality Decomposition}
\description{
Decomposes differences in concentration indices across two groups or waves into explained composition and unexplained structure effects.
}
\usage{
decompose_oaxaca_rif(
  data,
  rank,
  group,
  formula = NULL,
  outcome = NULL,
  covariates = NULL,
  categorical = NULL,
  group_levels = NULL,
  method = "erreygers",
  bounds = NULL,
  reweight = FALSE,
  rank_order = c("asc", "desc"),
  favorable = NULL,
  bootstrap_reps = 200L,
  confidence_level = 0.95,
  seed = 42L,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL
)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{survey_health_data} object, \code{grouped_health_data} object, or \code{survey.design} object.}
  \item{rank}{Character string naming the socioeconomic ranking variable.}
  \item{group}{Optional character string naming comparison domains or groups.}
  \item{formula}{Model formula (e.g., \code{stunting ~ mother_edu + area + improved_sanitation}).}
  \item{outcome}{Character string naming the health outcome variable.}
  \item{covariates}{Optional character vector of adjustment covariates.}
  \item{categorical}{Optional character vector of covariates to treat as categorical factors.}
  \item{group_levels}{Optional length-2 vector \code{c(group_0, group_1)} for RIF-Oaxaca comparison.}
  \item{method}{Estimation or index normalization method.}
  \item{bounds}{Numeric vector \code{c(min, max)} specifying natural bounds for bounded indices.}
  \item{reweight}{Logical; if \code{TRUE}, applies inverse-propensity counterfactual reweighting in RIF-Oaxaca.}
  \item{rank_order}{Ordering direction: \code{'asc'} (disadvantaged to advantaged) or \code{'desc'}.}
  \item{favorable}{Optional logical indicating whether higher values of the health indicator are desirable.}
  \item{bootstrap_reps}{Integer number of complex-survey PSU bootstrap replications.}
  \item{confidence_level}{Nominal confidence level in \code{(0, 1)}, default \code{0.95}.}
  \item{seed}{Integer random seed (restores global \code{.Random.seed} on exit).}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{strata}{Optional character string naming survey design strata (or character vector in low-level kernels).}
  \item{psu}{Optional character string naming primary sampling units / clusters (or character vector in low-level kernels).}
  \item{fpc}{Optional character string naming finite population correction values.}
}
\value{
An object of class \code{esther_decomp}.
}

