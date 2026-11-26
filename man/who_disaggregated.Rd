\name{who_disaggregated}
\alias{who_disaggregated}
\docType{data}
\title{WHO Health Inequality Monitor Disaggregated Surveillance Benchmark Table}
\description{
Pre-aggregated DTP3 immunization coverage estimates, standard errors, and subgroup populations across ordered wealth quintiles and unordered subnational regions.
}
\usage{who_disaggregated}
\format{
A \code{data.frame} with 10 rows and 10 variables:
\describe{
  \item{indicator}{Health indicator name (\code{dtp3_coverage}).}
  \item{dimension}{Equity stratification dimension (\code{wealth_quintile} or \code{region}).}
  \item{ordered}{Logical indicating whether the dimension is ordered.}
  \item{favorable}{Logical indicating whether higher coverage is favorable.}
  \item{scale}{Indicator scale (\code{100} for percentages).}
  \item{subgroup}{Subgroup label.}
  \item{subgroup_order}{Integer ordering index.}
  \item{estimate}{Subgroup coverage estimate.}
  \item{se}{Subgroup standard error.}
  \item{population}{Subgroup target population weight.}
  \item{sample_size}{Unweighted subgroup sample size.}
}
}
\keyword{datasets}
