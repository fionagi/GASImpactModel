% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{getMorData}
\alias{getMorData}
\title{Retrieve probability of all-cause mortality}
\usage{
getMorData(location, yearV, pYears, ageV = 0, maxAge = 99, impType)
}
\arguments{
\item{location}{country that matches UN data}

\item{yearV}{year of vaccine introduction}

\item{pYears}{number of years to project from year of vaccine introduction}

\item{ageV}{vaccination age}

\item{maxAge}{maximum age of cohort}

\item{impType}{the type of impact analysis, Calendar year, Year of birth or Year of Vaccination}
}
\value{
data.frame
}
\description{
Follow a cohort from age 0 in year of vaccine introduction
to maximum age, retrieving probability of death for each year of life
}
