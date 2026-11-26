\name{decompose_subgroup_ge}
\alias{decompose_subgroup_ge}
\title{Shorrocks Between- and Within-Group Generalized Entropy Decomposition}
\description{
Decomposes generalized entropy indices into exact within-group and between-group components with survey bootstrap standard errors.
}
\usage{
decompose_subgroup_ge(
  data,
  outcome,
  by,
  alpha = 1,
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
  \item{outcome}{Character string naming the health outcome variable.}
  \item{by}{Optional character string naming a domain/subpopulation stratification column.}
  \item{alpha}{Generalized entropy parameter (\code{0} for MLD, \code{1} for Theil).}
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

