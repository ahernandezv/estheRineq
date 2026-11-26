\name{survey_mch}
\alias{survey_mch}
\docType{data}
\title{Synthetic Stratified Two-Stage Cluster Maternal and Child Health Survey}
\description{
A realistic DHS/ENDES-style complex survey dataset (1,200 households across 6 geographic strata and 60 primary sampling units) containing sampling weights, wealth scores, quintiles, clinical need variables, stunting, immunization, and outpatient visits.
}
\usage{survey_mch}
\format{
A \code{data.frame} with 1,200 rows and 18 variables:
\describe{
  \item{hh_id}{Household identifier.}
  \item{survey_wave}{Survey wave (\code{2018} or \code{2025}).}
  \item{stratum_id}{Sampling stratum identifier.}
  \item{cluster_id}{Primary sampling unit (PSU) identifier.}
  \item{sample_weight}{Survey sampling weight.}
  \item{region}{Geographic region (\code{Coast}, \code{Highlands}, \code{Jungle}).}
  \item{area}{Residence area (\code{Urban}, \code{Rural}).}
  \item{wealth_score}{Continuous household wealth index.}
  \item{household_income}{Positive monthly household income.}
  \item{mother_edu}{Maternal education level.}
  \item{improved_sanitation}{Binary indicator of improved sanitation.}
  \item{child_age_mo}{Child age in months.}
  \item{female}{Binary sex indicator.}
  \item{chronic_morbidity}{Binary indicator of baseline clinical morbidity.}
  \item{stunting}{Binary adverse health outcome (child stunting).}
  \item{full_immunization}{Binary favorable health outcome (full immunization).}
  \item{outpatient_visits}{Count of outpatient healthcare visits.}
  \item{wealth_quintile}{Survey-weighted wealth quintile (\code{1} to \code{5}).}
}
}
\keyword{datasets}
