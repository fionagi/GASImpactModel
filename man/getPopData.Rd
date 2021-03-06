% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{getPopData}
\alias{getPopData}
\title{Find population at age for a location and year range}
\usage{
getPopData(location, yearV, pYears, maxAge)
}
\arguments{
\item{location}{country that matches UN data}

\item{yearV}{year of vaccine introduction}

\item{pYears}{number of years to project from year of vaccine introduction}

\item{maxAge}{maximum age of population}
}
\value{
data.frame
}
\description{
Find population at age for a location and year range
}
