\name{standardize_health_need}
\alias{standardize_health_need}
\title{Gravelle-Consistent Indirect Health-Need Standardization}
\description{
Computes need-expected and indirect need-standardized health outcomes holding non-need variables at survey-weighted means.
}
\usage{
standardize_health_need(
  data,
  outcome,
  need_vars,
  non_need_vars = NULL,
  categorical = NULL,
  family = c("auto", "gaussian", "binomial", "poisson"),
  link = NULL,
  weights = NULL
)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{survey_health_data} object, \code{grouped_health_data} object, or \code{survey.design} object.}
  \item{outcome}{Character string naming the health outcome variable.}
  \item{need_vars}{Character vector of legitimate clinical/demographic need variables.}
  \item{non_need_vars}{Optional character vector of non-need socioeconomic/control variables held at sample means.}
  \item{categorical}{Optional character vector of covariates to treat as categorical factors.}
  \item{family}{Generalized linear model family (\code{'auto'}, \code{'gaussian'}, \code{'binomial'}, or \code{'poisson'}).}
  \item{link}{Optional GLM link function override.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
}
\value{
A \code{data.frame} augmented with \code{y_need_expected} and \code{y_need_standardized}.
}

