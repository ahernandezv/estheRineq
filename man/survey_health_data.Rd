\name{survey_health_data}
\alias{survey_health_data}
\title{Create a Complex-Survey Health Inequality Container}
\description{
Wraps a microdata frame (or a survey::svydesign / svyrep.design object) with sampling weights, strata, PSUs, FPC, or replicate weights.
}
\usage{
survey_health_data(
  data,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  rep_weights = NULL,
  rep_type = "BRR",
  rep_scale = NULL,
  lonely_psu = c("remove", "adjust", "certainty", "average")
)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{survey_health_data} object, \code{grouped_health_data} object, or \code{survey.design} object.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
  \item{strata}{Optional character string naming survey design strata (or character vector in low-level kernels).}
  \item{psu}{Optional character string naming primary sampling units / clusters (or character vector in low-level kernels).}
  \item{fpc}{Optional character string naming finite population correction values.}
  \item{rep_weights}{Optional character vector of replicate weight column names.}
  \item{rep_type}{Replicate weight scheme (\code{'BRR'}, \code{'FAY'}, \code{'JK1'}, \code{'JKn'}, or \code{'BOOTSTRAP'}).}
  \item{rep_scale}{Optional custom scalar multiplier for replicate weight variance.}
  \item{lonely_psu}{Handling strategy for single-PSU strata: \code{'remove'}, \code{'adjust'}, \code{'certainty'}, or \code{'average'}.}
}
\value{
An object of class \code{esther_survey}.
}
\examples{
df <- load_survey_mch(n = 200)
svy <- survey_health_data(
  df,
  weights = 'sample_weight',
  strata = 'stratum_id',
  psu = 'cluster_id'
)
}
