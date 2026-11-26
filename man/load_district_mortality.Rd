\name{load_district_mortality}
\alias{load_district_mortality}
\title{Load Age-Stratified Small-Area District Mortality Dataset}
\description{
Generates age-stratified death counts and populations across 24 districts in 5 broad WHO age bands.
}
\usage{
load_district_mortality(seed = 2026L)
}
\arguments{
  \item{seed}{Integer random seed (restores global \code{.Random.seed} on exit).}
}
\value{
A \code{data.frame}.
}

