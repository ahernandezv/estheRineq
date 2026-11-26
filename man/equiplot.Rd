\name{equiplot}
\alias{equiplot}
\title{Create ICEH Pelotas Equiplots from Microdata, Grouped, or Wide Tables}
\description{
Draws publication-ready horizontal equiplots with optional pattern diagnostics and row sorting.
}
\usage{
equiplot(
  data,
  outcome = NULL,
  subgroup = NULL,
  by = NULL,
  columns = NULL,
  scale = 1,
  favorable = TRUE,
  diagnose_pattern = FALSE,
  sort_by = NULL,
  ascending = TRUE,
  palette = "pelotas",
  title = NULL,
  xlabel = NULL,
  ylabel = NULL,
  lang = "en",
  backend = "auto",
  weights = NULL
)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{survey_health_data} object, \code{grouped_health_data} object, or \code{survey.design} object.}
  \item{outcome}{Character string naming the health outcome variable.}
  \item{subgroup}{Character string naming the disaggregation subgroup column.}
  \item{by}{Optional character string naming a domain/subpopulation stratification column.}
  \item{columns}{Optional character vector of wide-format subgroup columns (Stata \code{equiplot} style).}
  \item{scale}{Numeric scale multiplier (e.g., \code{1} for proportions, \code{100} for percentages).}
  \item{favorable}{Optional logical indicating whether higher values of the health indicator are desirable.}
  \item{diagnose_pattern}{Logical; if \code{TRUE}, annotates Pelotas top/bottom/linear inequality patterns.}
  \item{sort_by}{Optional row sorting criterion (\code{'gap'}, \code{'min'}, \code{'max'}, \code{'mean'}, \code{'first'}, or \code{'last'}).}
  \item{ascending}{Logical sort direction when \code{sort_by} is specified.}
  \item{palette}{Editorial palette name (\code{'pelotas'}, \code{'lancet'}, \code{'okabe_ito'}, or \code{'who'}).}
  \item{title}{Optional custom title string.}
  \item{xlabel}{Optional custom horizontal axis label.}
  \item{ylabel}{Optional custom vertical axis label.}
  \item{lang}{Output language: \code{'en'} (default), \code{'es'}, or \code{'pt'}.}
  \item{backend}{Plotting backend: \code{'auto'}, \code{'ggplot2'}, or \code{'base'}.}
  \item{weights}{Optional character string naming sampling weights (or numeric weight vector in low-level kernels).}
}
\value{
A \code{ggplot} object (when ggplot2 is used) or invisibly returns the plotted summary \code{data.frame}.
}

