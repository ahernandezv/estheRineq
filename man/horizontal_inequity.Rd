\name{horizontal_inequity}
\alias{horizontal_inequity}
\title{Estimate Need-Standardized Horizontal Inequity Indices}
\description{
Estimates unstandardized concentration index, need-expected concentration index, and Horizontal Inequity (HI) with PSU survey bootstrap inference.
}
\usage{
horizontal_inequity(
  data,
  outcome,
  rank,
  need_vars,
  non_need_vars = NULL,
  categorical = NULL,
  method = "standard",
  bounds = NULL,
  family = c("auto", "gaussian", "binomial", "poisson"),
  link = NULL,
  rank_order = c("asc", "desc"),
  favorable = NULL,
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
  \item{need_vars}{Character vector of legitimate clinical/demographic need variables.}
  \item{non_need_vars}{Optional character vector of non-need socioeconomic/control variables held at sample means.}
  \item{categorical}{Optional character vector of covariates to treat as categorical factors.}
  \item{method}{Estimation or index normalization method.}
  \item{bounds}{Numeric vector \code{c(min, max)} specifying natural bounds for bounded indices.}
  \item{family}{Generalized linear model family (\code{'auto'}, \code{'gaussian'}, \code{'binomial'}, or \code{'poisson'}).}
  \item{link}{Optional GLM link function override.}
  \item{rank_order}{Ordering direction: \code{'asc'} (disadvantaged to advantaged) or \code{'desc'}.}
  \item{favorable}{Optional logical indicating whether higher values of the health indicator are desirable.}
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

