\name{plot_sii_gradient}
\alias{plot_sii_gradient}
\title{Plot Socioeconomic Ridit Gradients for SII and RII}
\description{
Renders population-scaled subgroup bubbles with 95\% error bars and fitted SII/RII gradient curves.
}
\usage{
plot_sii_gradient(
  data,
  outcome = NULL,
  subgroup = NULL,
  scale = 1,
  method = c("logit_marginal", "linear"),
  title = NULL,
  lang = "en",
  backend = "auto",
  weights = NULL
)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{survey_health_data} object, \code{grouped_health_data} object, or \code{survey.design} object.}
  \item{outcome}{Character string naming the health outcome variable.}
  \item{subgroup}{Character string naming the disaggregation subgroup column.}
  \item{scale}{Numeric scale multiplier (e.g., \code{1} for proportions, \code{100} for percentages).}
  \item{method}{Estimation or index normalization method.}
  \item{title}{Optional custom title string.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{backend}{Plotting backend: \code{'auto'}, \code{'ggplot2'}, or \code{'base'}.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
}
\value{
Invisibly returns the plotted subgroup \code{data.frame}.
}

