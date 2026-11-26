\name{district_mortality}
\alias{district_mortality}
\docType{data}
\title{Age-Stratified Small-Area District Mortality Surveillance Dataset}
\description{
Age-stratified death counts and population denominators across 24 districts in 5 broad WHO life-course age bands (\code{0-14}, \code{15-29}, \code{30-44}, \code{45-59}, \code{60+}).
}
\usage{district_mortality}
\format{
A \code{data.frame} with 120 rows and 5 variables:
\describe{
  \item{district}{District identifier (\code{District_01} to \code{District_24}).}
  \item{deprivation_quintile}{Area deprivation quintile (\code{1} to \code{5}).}
  \item{age_group}{Age band matching \code{who_broad_5}.}
  \item{deaths}{Observed death count.}
  \item{population}{District age-band population denominator.}
}
}
\keyword{datasets}
