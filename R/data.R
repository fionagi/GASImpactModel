# data.R - This file includes documentation of the data used.

#' Countries by UN regions
#'
#'
#' @format A data table containing 230 observations of 2 variables.
#' \describe{
#'   \item{Country}{Country or area name}
#'   \item{Region}{UN region name}
#' }
#' @source \url{https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.}
"data.region"

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

#' Incidence, death rates and DALYs for rheumatic heart disease (RHD)
#'
#' Incidence, death rates and DALYs (per 100,000 persons) for 2019 by country/region
#'  and age group for 205 locations.Data set extracted from the Global Health
#'  Data Exchange (GHDx), created and supported by the Institute for Health
#'  Metrics and Evaluation (IHME)
#'
#' @format A data table containing 16104 observations of 6 variables.
#' \describe{
#'   \item{measure}{Either Incidence, Deaths or DALYs}
#'   \item{location}{Region, subregion, country or area}
#'   \item{age}{age groups, <1, 1 to 4, then 5-year age groups until 80 to 84}
#'   \item{val}{incident rate per 100,000 persons}
#'   \item{upper}{upper bound of 95% confidence interval}
#'   \item{lower}{lower bound of 95% confidence interval}
#' }
#' @source \url{http://ghdx.healthdata.org/gbd-results-tool}
"data.rhd2019"

#' Incidence, death rates and DALYs for cellulitis
#'
#' Incidence, death rates and DALYs (per 100,000 persons) for 2019 by country/region
#'  and age group for 205 locations.Data set extracted from the Global Health
#'  Data Exchange (GHDx), created and supported by the Institute for Health
#'  Metrics and Evaluation (IHME)
#'
#' @format A data table containing 16470 observations of 6 variables.
#' \describe{
#'   \item{measure}{Either Incidence, Deaths or DALYs}
#'   \item{location}{Region, subregion, country or area}
#'   \item{age}{age groups, <1, 1 to 4, then 5-year age groups until 80 to 84}
#'   \item{val}{incident rate per 100,000 persons}
#'   \item{upper}{upper bound of 95% confidence interval}
#'   \item{lower}{lower bound of 95% confidence interval}
#' }
#' @source \url{http://ghdx.healthdata.org/gbd-results-tool}
"data.cellulitis2019"

#' Probability of mortality
#'
#' Probability of dying between ages x and x+1 (2019). Data from the Global
#' Health Observatory, WHO
#'
#' @format A data table containing 6954 observations of 5 variables.
#' \describe{
#'   \item{Region}{Europe, Western Pacific, Americas, Eastern Mediterranean,
#'                 Africa and South-East Asia}
#'   \item{Location}{country or area}
#'   \item{Sex}{female, male}
#'   \item{Age group}{age groups, <1, 1 to 4, then 5-year age groups until 85 plus}
#'   \item{Value}{probability of mortality}
#' }
#' @source \url{https://www.who.int/data/gho/data/indicators/indicator-details/GHO/gho-ghe-life-tables-by-country}
"data.mortality2019"



