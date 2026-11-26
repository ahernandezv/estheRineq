\name{who_summary}
\alias{who_summary}
\title{Compute All 21 WHO Health Inequality Monitor Summary Measures}
\description{
Computes any subset or all 21 WHO Health Inequality Monitor (healthequal / HEAT) summary measures from disaggregated tables or complex survey microdata.
}
\usage{
who_summary(
  data,
  outcome = NULL,
  dimension = NULL,
  measures = "all",
  ordered = NULL,
  favorable = NULL,
  reference_group = NULL,
  scale = NULL,
  heat_scaling = TRUE,
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
  \item{dimension}{Character string naming the equity stratification dimension (or dataset filter).}
  \item{measures}{Character vector of WHO measure codes or \code{'all'}.}
  \item{ordered}{Logical indicating whether the equity dimension has a natural socioeconomic ordering.}
  \item{favorable}{Optional logical indicating whether higher values of the health indicator are desirable.}
  \item{reference_group}{Optional reference subgroup label for reference-based WHO measures.}
  \item{scale}{Numeric scale multiplier (e.g., \code{1} for proportions, \code{100} for percentages).}
  \item{heat_scaling}{Logical; if \code{TRUE}, applies WHO HEAT scaling conventions (\code{x100} for RCI, \code{x1000} for TI/MLD).}
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
\examples{
df_w <- load_who_disaggregated('wealth')
gd <- grouped_health_data(
  df_w,
  subgroup = 'subgroup',
  estimate = 'estimate',
  se = 'se',
  population = 'population',
  scale = 100
)
res <- who_summary(
  gd,
  measures = c('d', 'r', 'par', 'paf', 'sii', 'rii'),
  n_sim = 200
)
}
