\name{grouped_health_data}
\alias{grouped_health_data}
\title{Create a Disaggregated Subgroup Surveillance Container}
\description{
Wraps pre-aggregated subgroup estimates (or event counts and populations) for WHO Health Inequality Monitor and SII/RII estimation.
}
\usage{
grouped_health_data(
  data,
  subgroup,
  estimate = NULL,
  se = NULL,
  population = NULL,
  events = NULL,
  subgroup_order = NULL,
  ordered = TRUE,
  favorable = NULL,
  reference_group = NULL,
  scale = 1,
  sample_size = NULL
)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{survey_health_data} object, \code{grouped_health_data} object, or \code{survey.design} object.}
  \item{subgroup}{Character string naming the disaggregation subgroup column.}
  \item{estimate}{Character string naming subgroup point estimates.}
  \item{se}{Optional character string naming subgroup standard errors.}
  \item{population}{Character string naming subgroup population denominators or weights.}
  \item{events}{Character string naming observed event counts.}
  \item{subgroup_order}{Optional ordering column name or character vector of ordered subgroup levels.}
  \item{ordered}{Logical indicating whether the equity dimension has a natural socioeconomic ordering.}
  \item{favorable}{Optional logical indicating whether higher values of the health indicator are desirable.}
  \item{reference_group}{Optional reference subgroup label for reference-based WHO measures.}
  \item{scale}{Numeric scale multiplier (e.g., \code{1} for proportions, \code{100} for percentages).}
  \item{sample_size}{Optional character string naming unweighted subgroup sample sizes.}
}
\value{
An object of class \code{esther_grouped}.
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
}
