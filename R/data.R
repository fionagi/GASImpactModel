# data.R - This file includes documentation of the data used.

#' Population by age group
#'
#' Total population (both sexes combined) for 2020 by 5-year age group (in thousands).
#' Data set extracted from "Annual Population by Age Groups - Both Sexes" obtained
#' from source.
#'
#' @format A data table containing 251 observations of 22 variables.
#' \describe{
#'   \item{Country}{Region, subregion, country or area}
#'   \item{0-4, 5-9, ..., 95-99, 100+}{5-year age groups}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Population/}
"data.popbyage2020"

#' Incident rates for rheumatic heart disease (RHD)
#'
#' Incident rates (per 100,000 persons) for 2019 by country/region and age group.
#' Data set extracted from the Global Health Data Exchange (GHDx), created and
#'  supported by the Institute for Health Metrics and Evaluation (IHME)
#'
#' @format A data table containing 8295 observations of 5 variables.
#' \describe{
#'   \item{location}{Region, subregion, country or area}
#'   \item{age}{age groups, <1, 1 to 4, then 5-year age groups until 80 plus}
#'   \item{val}{incident rate per 100,000 persons}
#'   \item{upper}{upper bound of 95% confidence interval}
#'   \item{lower}{lower bound of 95% confidence interval}
#' }
#' @source \url{http://ghdx.healthdata.org/gbd-results-tool}
"data.rhd2019"

#' Incident rates for cellulitis
#'
#' Incident rates (per 100,000 persons) for 2019 by country/region and age group.
#' Data set extracted from the Global Health Data Exchange (GHDx), created and
#'  supported by the Institute for Health Metrics and Evaluation (IHME)
#'
#' @format A data table containing 8532 observations of 5 variables.
#' \describe{
#'   \item{location}{Region, subregion, country or area}
#'   \item{age}{age groups, <1, 1 to 4, then 5-year age groups until 80 plus}
#'   \item{val}{incident rate per 100,000 persons}
#'   \item{upper}{upper bound of 95% confidence interval}
#'   \item{lower}{lower bound of 95% confidence interval}
#' }
#' @source \url{http://ghdx.healthdata.org/gbd-results-tool}
"data.cellulitis2019"