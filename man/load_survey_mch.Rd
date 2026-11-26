\name{load_survey_mch}
\alias{load_survey_mch}
\title{Load Synthetic Stratified Two-Stage Cluster MCH Survey Dataset}
\description{
Generates a realistic DHS/ENDES-style complex survey dataset with strata, PSUs, weights, wealth, need variables, stunting, immunization, and visits.
}
\usage{
load_survey_mch(n = 1200L, seed = 2026L)
}
\arguments{
  \item{n}{Integer number of observations or palette colors.}
  \item{seed}{Integer random seed (restores global \code{.Random.seed} on exit).}
}
\value{
A \code{data.frame}.
}

