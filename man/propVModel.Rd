% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{propVModel}
\alias{propVModel}
\title{Scale pre-vaccination incident counts to get counts for a particular
vaccination scenario, taking into account immunity and coverage assumptions}
\usage{
propVModel(
  preVacc,
  waning,
  durability,
  efficacy,
  vaccAge = 0,
  ramp,
  maxAge = 99
)
}
\arguments{
\item{preVacc}{matrix of pre-vaccination incident counts}

\item{waning}{assume linear waning immunity, TRUE or FALSE}

\item{durability}{if linear waning, number of years taken to reach 50\%
immunity, else number of years of full immunity (after
which immunity drops to 0)}

\item{efficacy}{the resulting efficacy proportion, efficacy x coverage}

\item{vaccAge}{age of vaccination. Default 0}

\item{ramp}{assume linear 10 year ramp up to maximum efficacy from year of introduction}

\item{maxAge}{maximum age of cohort. Default 99}
}
\value{
matrix
}
\description{
Scale pre-vaccination incident counts to get counts for a particular
vaccination scenario, taking into account immunity and coverage assumptions
}
