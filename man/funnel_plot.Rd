\name{funnel_plot}
\alias{funnel_plot}
\title{Spiegelhalter Epidemiological Funnel Plot}
\description{
Draws funnel plots with 95\% and 99.8\% Poisson or overdispersed control limits and outlier labels.
}
\usage{
funnel_plot(
  data,
  estimate = "estimate",
  se = "se",
  denominator = "population",
  label = "domain",
  benchmark = NULL,
  overdispersion = FALSE,
  label_outliers = TRUE,
  title = NULL,
  xlabel = NULL,
  ylabel = NULL,
  lang = "en",
  backend = "auto"
)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{survey_health_data} object, \code{grouped_health_data} object, or \code{survey.design} object.}
  \item{estimate}{Character string naming subgroup point estimates.}
  \item{se}{Optional character string naming subgroup standard errors.}
  \item{denominator}{Character string naming precision/population denominator column.}
  \item{label}{Character string naming unit labels (or LaTeX table label).}
  \item{benchmark}{Optional numeric reference target line for funnel plot.}
  \item{overdispersion}{Logical; if \code{TRUE}, applies Spiegelhalter winsorized overdispersion scaling.}
  \item{label_outliers}{Logical; if \code{TRUE}, labels units outside 99.8\% control limits.}
  \item{title}{Optional custom title string.}
  \item{xlabel}{Optional custom horizontal axis label.}
  \item{ylabel}{Optional custom vertical axis label.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{backend}{Plotting backend: \code{'auto'}, \code{'ggplot2'}, or \code{'base'}.}
}
\value{
Invisibly returns a \code{data.frame} flagging outlier units.
}

