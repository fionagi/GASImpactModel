# data.R - This file includes documentation of the data used.

#' Countries by United Nations regions
#' World Development Indicators and Other World Bank Data updated April 2021
#' From R package WDI
#'
#' @format A data table containing 304 observations of 4 variables.
#' \describe{
#'   \item{Code}{iso3 country code}
#'   \item{Country}{Country or area name}
#'   \item{Region}{UN region name}
#'   \item{IncomeGroup}
#' }
"data.region"

#' Population by age - 1950 - 2020
#'
#' Total population (both sexes combined) for 1950 - 2020 by 1-year age
#' groups (in thousands).
#' Data set extracted from United Nations data "Annual Population by Age - Both Sexes"
#' obtained from source.
#'
#' @format A data table containing 17821 observations of 104 variables.
#' \describe{
#'   \item{Country}{Region, subregion, country or area}
#'   \item{Country code}{iso3 country code}
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
#' @format A data table containing 20331 observations of 104 variables.
#' \describe{
#'   \item{Country}{Region, subregion, country or area}
#'   \item{Country code}{iso3 country code}
#'   \item{Year}{2020 - 2100}
#'   \item{0, 1, ..., 100}{population in 1-year age groups}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Interpolated/}
"data.popbyage.pred"

#' Incidence, death rates and DALYs for rheumatic heart disease (RHD)
#'
#' Incidence, death rates and DALYs (per 100,000 persons) for 2019 by country/region
#'  and age group for 205 locations. Data set extracted from the Global Health
#'  Data Exchange (GHDx), created and supported by the Institute for Health
#'  Metrics and Evaluation (IHME)
#'
#' @format A data table containing 38130 observations of 8 variables.
#' \describe{
#'   \item{iso3}{iso3 country code}
#'   \item{measure}{Either Incidence, Deaths or DALYs}
#'   \item{location}{Region, subregion, country or area}
#'   \item{age}{age groups, <1, 1 to 4, then 5-year age groups up to 95 plus}
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
#'  and age group for 205 locations. Data set extracted from the Global Health
#'  Data Exchange (GHDx), created and supported by the Institute for Health
#'  Metrics and Evaluation (IHME)
#'
#' @format A data table containing 38745 observations of 8 variables.
#' \describe{
#'   \item{iso3}{iso3 country code}
#'   \item{measure}{Either Incidence, Deaths or DALYs}
#'   \item{location}{Region, subregion, country or area}
#'   \item{age}{age groups, <1, 1 to 4, then 5-year age groups up to 95 plus}
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
#' @format A data table containing 73794 observations of 6 variables.
#' \describe{
#'   \item{Location}{Region, subregion, country or area}
#'   \item{Country code}{United Nations M49 numeric codes}
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
#' @format A data table containing 31626 observations of 6 variables.
#' \describe{
#'   \item{Location}{Region, subregion, country or area}
#'   \item{Country code}{United Nations M49 numeric codes}
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
#' @format A data table containing 55220 observations of 6 variables.
#' \describe{
#'   \item{Location}{Region, subregion, country or area}
#'   \item{Country code}{United Nations M49 numeric codes}
#'   \item{Period}{Year range}
#'   \item{Age (x)}{Start of age range}
#'   \item{Age interval (n)}{age interval length}
#'   \item{Probability of dying q(x,n)}{probability of all-cause mortality between ages x and x+n}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Mortality/}
"data.mortality.pred2100"

#' Life expectancy - 1950 - 2020
#'
#' Life expectancy at exact age for both sexes combined by region and period.
#' Data set extracted from United Nations data "Life Expectancy at exact age x (ex) - Both Sexes"
#' obtained from source,
#'
#' @format A data table containing 3514 observations of 25 variables
#' \describe{
#'   \item{Location}{Region, subregion, country or area}
#'   \item{Country code}{United Nations M49 numeric codes}
#'   \item{Period}{Year range}
#'   \item{0, 1, 5, ..., 95, 100+}{life expactancy at age}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Mortality/}
"data.lifeEx.1950_to_2020"

#' Life expectancy - 2020 - 2100
#'
#' Life expectancy at exact age for both sexes combined by region and period.
#' Data set extracted from United Nations data "Life Expectancy at exact age x (ex) - Both Sexes"
#' obtained from source,
#'
#' @format A data table containing 4016 observations of 25 variables
#' \describe{
#'   \item{Location}{Region, subregion, country or area}
#'   \item{Country code}{United Nations M49 numeric codes}
#'   \item{Period}{Year range}
#'   \item{0, 1, 5, ..., 95, 100+}{life expactancy at age}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Mortality/}
"data.lifeEx.pred2100"


#' Country-specific year of vaccine introduction and maximum coverage
#'
#' Shift health prediction of likely year of vaccine introduction for
#' each country and predicted maximum coverage proportion
#'
#' @format A data table containing 183 observations of 6 variables
#' \describe{
#'   \item{Country}{Country}
#'   \item{Region}{Region}
#'   \item{Income.Group}{One of "Low income", "Lower middle income",
#'                       "Upper middle income" or "High income"}
#'   \item{Year.of.Introduction}{Predicted year of vaccine introbution}
#'   \item{Coverage}{As a proportion}
#'   \item{Size.of Public.Market}{As a proportion}
#' }
"data.coveragebycountry"

