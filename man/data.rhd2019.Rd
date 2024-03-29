% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{data.rhd2019}
\alias{data.rhd2019}
\title{Incidence, death rates and DALYs for rheumatic heart disease (RHD)}
\format{
A data table containing 38130 observations of 8 variables.
\describe{
\item{iso3}{iso3 country code}
\item{measure}{Either Incidence, Deaths or DALYs}
\item{location}{Region, subregion, country or area}
\item{age}{age groups, <1, 1 to 4, then 5-year age groups up to 95 plus}
\item{metric}{Rate, Number or Percent}
\item{val}{measure value}
\item{upper}{upper bound of 95\% confidence interval}
\item{lower}{lower bound of 95\% confidence interval}
}
}
\source{
\url{http://ghdx.healthdata.org/gbd-results-tool}
}
\usage{
data.rhd2019
}
\description{
Incidence, death rates and DALYs (per 100,000 persons) for 2019 by country/region
and age group for 205 locations. Data set extracted from the Global Health
Data Exchange (GHDx), created and supported by the Institute for Health
Metrics and Evaluation (IHME)
}
\keyword{datasets}
