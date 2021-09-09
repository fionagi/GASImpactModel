# data.R - This file includes documentation of the data used.

#' Countries by United Nations regions
#'
#'
#' @format A data table containing 230 observations of 2 variables.
#' \describe{
#'   \item{Country}{Country or area name}
#'   \item{Region}{UN region name}
#' }
#' @source \url{https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.}
"data.region"

#' Population by age - 1950 - 2020
#'
#' Total population (both sexes combined) for 1950 - 2020 by 1-year age
#' groups (in thousands).
#' Data set extracted from United Nations data "Annual Population by Age - Both Sexes"
#' obtained from source.
#'
#' @format A data table containing 17821 observations of 103 variables.
#' \describe{
#'   \item{Country}{Region, subregion, country or area}
#'   \item{0, 1, ..., 100}{population in 1-year age groups}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Interpolated/}
"data.popbyage.1950_to_2020"

#' Population by age - 2020 - 2100
#'
#' Total interpolated population (both sexes combined) for 2020 - 2100
#' by 1-year age groups (in thousands).
#' Data set extracted from United Nations data "Annual Population by Age - Both Sexes"
#' obtained from source.
#'
#' @format A data table containing 20331 observations of 103 variables.
#' \describe{
#'   \item{Country}{Region, subregion, country or area}
#'   \item{Year}{2020 - 2100}
#'   \item{0, 1, ..., 100}{population in 1-year age groups}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Interpolated/}
"data.popbyage.pred"

#' Incidence, death rates and DALYs for rheumatic heart disease (RHD)
#'
#' Incidence, death rates and DALYs (per 100,000 persons) for 2019 by country/region
#'  and age group for 205 locations.Data set extracted from the Global Health
#'  Data Exchange (GHDx), created and supported by the Institute for Health
#'  Metrics and Evaluation (IHME)
#'
#' @format A data table containing 54120 observations of 7 variables.
#' \describe{
#'   \item{measure}{Either Incidence, Deaths or DALYs}
#'   \item{location}{Region, subregion, country or area}
#'   \item{age}{age groups, <1, 1 to 4, then 5-year age groups until 80 to 84}
#'   \item{metric}{Rate, Number or Percent}
#'   \item{val}{measure value}
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
#' @format A data table containing 55350 observations of 7 variables.
#' \describe{
#'   \item{measure}{Either Incidence, Deaths or DALYs}
#'   \item{location}{Region, subregion, country or area}
#'   \item{age}{age groups, <1, 1 to 4, then 5-year age groups until 80 to 84}
#'   \item{metric}{Rate, Number or Percent}
#'   \item{val}{measure value}
#'   \item{upper}{upper bound of 95% confidence interval}
#'   \item{lower}{lower bound of 95% confidence interval}
#' }
#' @source \url{http://ghdx.healthdata.org/gbd-results-tool}
"data.cellulitis2019"

#' Probability of mortality - 1950 - 2020
#'
#' Probability of dying between ages x and x+n by country and period.
#' Data set extracted from United Nations data "Abridged Life Table - Both Sexes"
#' obtained from source.
#'
#' @format A data table containing 73794 observations of 5 variables.
#' \describe{
#'   \item{Location}{Region, subregion, country or area}
#'   \item{Period}{Year range}
#'   \item{Age (x)}{Start of age range}
#'   \item{Age interval (n)}{age interval length}
#'   \item{Probability of dying q(x,n)}{probability of all-cause mortality between ages x and x+n}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Mortality/}
"data.mortality.1950_to_2020"

#' Probability of mortality - 2020 - 2050
#'
#' Probability of dying between ages x and x+n by country and period.
#' Data set extracted from United Nations data "Abridged Life Table - Both Sexes"
#' obtained from source.
#'
#' @format A data table containing 31626 observations of 5 variables.
#' \describe{
#'   \item{Location}{Region, subregion, country or area}
#'   \item{Period}{Year range}
#'   \item{Age (x)}{Start of age range}
#'   \item{Age interval (n)}{age interval length}
#'   \item{Probability of dying q(x,n)}{probability of all-cause mortality between ages x and x+n}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Mortality/}
"data.mortality.pred2050"

#' Probability of mortality - 2050 - 2100
#'
#' Probability of dying between ages x and x+n by country and period.
#' Data set extracted from United Nations data "Abridged Life Table - Both Sexes"
#' obtained from source.
#'
#' @format A data table containing 31626 observations of 5 variables.
#' \describe{
#'   \item{Location}{Region, subregion, country or area}
#'   \item{Period}{Year range}
#'   \item{Age (x)}{Start of age range}
#'   \item{Age interval (n)}{age interval length}
#'   \item{Probability of dying q(x,n)}{probability of all-cause mortality between ages x and x+n}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Mortality/}
"data.mortality.pred2100"


