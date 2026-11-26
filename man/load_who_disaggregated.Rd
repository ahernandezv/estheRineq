\name{load_who_disaggregated}
\alias{load_who_disaggregated}
\title{Load WHO Health Inequality Monitor Disaggregated Benchmark Dataset}
\description{
Returns ordered wealth-quintile and unordered subnational-region DTP3 immunization surveillance tables.
}
\usage{
load_who_disaggregated(dimension = c("both", "wealth", "region"))
}
\arguments{
  \item{dimension}{Character string naming the equity stratification dimension (or dataset filter).}
}
\value{
A \code{data.frame}.
}

