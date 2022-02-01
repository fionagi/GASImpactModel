#RETRIEVE DATA FUNCTIONS

#' Retrieve all countries in a particular region
#'
#' @param region UN region
#'
#' @return vector
#' @export
getCountries <- function(region)
{
  allCountries <- data.region[data.region$Code %in% unique(data.rhd2019$iso3),]$Country

  if(region == "All")
  {
    countries <- allCountries
  } else{
    countries <- intersect(data.region$Country[which(data.region$Region == region)], allCountries)
  }
  return(countries)
}

#' Retrieve location and condition specific incidence, DALYs and deaths data
#'
#' Note: Only Cellulitis and Rheumatic Heart Disease have country-specific data
#' @param location country or region
#' @param condition Cellulitis, Rheumatic Heart Disease, Impetigo, Pharyngitis
#'                  or Invasive infection
#' @param metric Rate, Number or Percent. Only used when extracting Cellulitis
#'               or Rheumatic Heart Disease data
#' @param prop Proportion of incident data that is attributable to GAS
#' @return list(data.frame, data.frame, data.frame)
#' @export
getConditionData <- function(location, condition, metric = NA, prop = 1)
{
  label <- lookup_condition[lookup_condition$Condition==condition ,]$Label

  if(condition == "Cellulitis" || condition == "Rheumatic Heart Disease")
  {
    condData <- paste("data.", label, "2019", sep='')
    code <- countrycode::countrycode(location, "country.name", "iso3c")

    data <- get(condData)
    inc <- data[data$iso3 == code & data$measure == "Incidence" & data$metric == metric,]
    inc <- inc[complete.cases(inc),]
    deaths <- data[data$iso3 == code & data$measure == "Deaths" & data$metric == metric,]
    deaths <- deaths[complete.cases(deaths),]
    dalys <- data[data$iso3 == code & data$measure == "DALYs (Disability-Adjusted Life Years)" & data$metric == metric,]
    dalys <- dalys[complete.cases(dalys),]

    inc <- missingData(inc)
    deaths <- missingData(deaths)
    dalys <- missingData(dalys)

    if(prop != 1)
    {
      inc[, c("val", "lower", "upper")] <- prop *inc[, c("val", "lower", "upper")]
      deaths[, c("val", "lower", "upper")] <- prop *deaths[, c("val", "lower", "upper")]
      dalys[, c("val", "lower", "upper")] <- prop *dalys[, c("val", "lower", "upper")]
    }

    return(list(inc, deaths, dalys))
  }

  condData <- paste("inc_", label, sep= '')
  inc <- as.data.frame(get(condData))
  inc[,"val"] <- prop*inc[,"val"]

  return(inc)
}

#' Check for missing age-specific data and add 0 row
#'
#' @param dataF data frame from getRateData
#'
#' @return data.frame
#' @export
missingData <- function(dataF)
{
  if(nrow(dataF) != nrow(age_groups))
  {

    missAge <- setdiff(age_groups$Label, dataF$age)

    for(i in 1:length(missAge))
    {
      dataF <- rbind(dataF, dataF[1,])
      dataF[nrow(dataF),]$age <- missAge[i]
      dataF[nrow(dataF),]$val <- 0
      dataF[nrow(dataF),]$upper <- 0
      dataF[nrow(dataF),]$lower <- 0
    }

  }
  return(dataF)

}

#' Find population at birth for cohorts to be modeled
#'
#' @param location country that matches UN data
#' @param yearV year of vaccine introduction
#' @param pYears number of years to project from year of vaccine introduction
#' @param ageV vaccination age
#' @param birth flag indicating to return population at birth for each cohort
#'
#' @return data.frame
#' @export
getInitPop <- function(location, yearV, pYears, ageV, birth = TRUE)
{
  pop <- matrix(NA, nrow = pYears+1, ncol = 2)
  code <- countrycode::countrycode(location, "country.name", "un")

  if(birth)
  { #find birth population
    colnames(pop) <- c("Birth year", "Pop at birth")
    yearV <- yearV - ageV
    age <- 0
  }else{
    #find pop numbers at age of vaccination for each year of interest
    colnames(pop) <- c("Vaccination year", "Pop at vacc age")
    age <- ageV
  }
  pop[,1] <- yearV:(yearV+pYears)

  year <- as.numeric(yearV)
  for(i in 1:(pYears+1))
  {

    if(year < 2020)
    {
      select <- data.popbyage.1950_to_2020$`Country code` == code &
        data.popbyage.1950_to_2020$Year == year
      pop[i,2] <- as.numeric(data.popbyage.1950_to_2020[select, as.character(age)])*1000 #pop in thousands

    }else{

      select <- data.popbyage.pred$`Country code` == code &
        data.popbyage.pred$Year == year
      pop[i,2] <- as.numeric(data.popbyage.pred[select, as.character(ageV)])*1000 #pop in thousands

    }

    year = year +1
    if(year > 2100) year <- 2100 #Use pop from last year of data
    #for all years > 2100
  }

  return(pop)
}

#' Find population at age for a location and year range
#'
#' @param location country that matches UN data
#' @param yearV year of vaccine introduction
#' @param pYears number of years to project from year of vaccine introduction
#' @param maxAge maximum age of population
#'
#' @return data.frame
#' @export
getPopData <- function(location, yearV, pYears, maxAge)
{
  pop <- matrix(NA, nrow = maxAge + 1, ncol = pYears + 1)
  colnames(pop) <- yearV:(yearV+pYears)
  rownames(pop) <- 0:maxAge

  code <- countrycode::countrycode(location, "country.name", "un")

  age <- as.character(0:maxAge)

  year <- as.numeric(yearV)
  for(i in 1:(pYears+1))
  {

    if(year < 2020)
    {
      select <- data.popbyage.1950_to_2020$`Country code` == code &
        data.popbyage.1950_to_2020$Year == year
      pop[,i] <- as.numeric(data.popbyage.1950_to_2020[select, age])*1000 #pop in thousands

    }else{

      select <- data.popbyage.pred$`Country code` == code &
        data.popbyage.pred$Year == year
      pop[,i] <- as.numeric(data.popbyage.pred[select, age])*1000 #pop in thousands

    }

    year = year +1
    if(year > 2100) year <- 2100 #Use pop from last year of data
    #for all years > 2100
  }

  return(pop)
}


#' Retrieve probability of all-cause mortality
#'
#' Follow a cohort from age 0 in year of vaccine introduction
#' to maximum age, retrieving probability of death for each year of life
#' @param location country that matches UN data
#' @param yearV year of vaccine introduction
#' @param pYears number of years to project from year of vaccine introduction
#' @param ageV vaccination age
#' @param maxAge maximum age of cohort
#' @param impType the type of impact analysis, Calendar year, Year of birth or Year of Vaccination
#'
#' @return data.frame
#' @export
getMorData <- function(location, yearV, pYears, ageV = 0, maxAge = 99, impType)
{
  code <- countrycode::countrycode(location, "country.name", "un")

  tIndex <- rep(1, age_groups$Years[1])
  interval <- rep(age_groups$Years[1], age_groups$Years[1])
  for(i in 2:nrow(age_groups))
  {
    tIndex <- c(tIndex, rep(i, age_groups$Years[i]))
    interval <- c(interval, rep(age_groups$Years[i], age_groups$Years[i]))
  }
  tIndex <- tIndex[1:(maxAge+1)]
  interval <- interval[1:(maxAge+1)]

  #adjust so can be treated as "Year of birth"
  if(impType == "Year of vaccination" || impType == "Calendar year") yearV <- yearV - ageV

  tmpMor <- matrix(NA, nrow = (pYears+1)+maxAge+1, ncol = maxAge+1)
  colnames(tmpMor) <- 0:maxAge
  rownames(tmpMor) <- as.numeric(yearV):(as.numeric(yearV)+pYears+maxAge+1)

  year <- as.numeric(yearV)
  for(i in 1:nrow(tmpMor))
  {
    if(year <2020)
    {

      index <- trunc(((year-1950)/5)+1)
      period <- unique(data.mortality.1950_to_2020$Period)[index]
      select <- data.mortality.1950_to_2020$`Country code` == code &
        data.mortality.1950_to_2020$Period == period
      tmpMor[i,] <- as.numeric(data.mortality.1950_to_2020[select, "Probability of dying q(x,n)"])[tIndex]
      tmpMor[i,] <- 1 - exp(log(1-tmpMor[i,])/interval) #change to single year probability

    } else {

      if(year < 2050)
      {

        index <- trunc(((year-2020)/5)+1)
        period <- unique(data.mortality.pred2050$Period)[index]
        select <- data.mortality.pred2050$`Country code` == code &
          data.mortality.pred2050$Period == period
        tmpMor[i,] <- as.numeric(data.mortality.pred2050[select, "Probability of dying q(x,n)"])[tIndex]
        tmpMor[i,] <- 1 - exp(log(1-tmpMor[i,])/interval) #change to single year probability

      } else {

        index <- trunc(((year-2050)/5)+1)
        period <- unique(data.mortality.pred2100$Period)[index]
        select <- data.mortality.pred2100$`Country code` == code &
          data.mortality.pred2100$Period == period
        tmpMor[i,] <- as.numeric(data.mortality.pred2100[select, "Probability of dying q(x,n)"])[tIndex]
        tmpMor[i,] <- 1 - exp(log(1-tmpMor[i,])/interval) #change to single year probability

      }
    }

    year = year +1
    if(year > 2099) year <- 2099 #Use prob of dying from last year of data
    #for all years > 2099
  }

  mor <- matrix(NA, nrow = pYears+1, ncol = maxAge+1)
  colnames(mor) <- 0:maxAge
  rownames(mor) <- as.numeric(yearV):(as.numeric(yearV)+pYears)

  for(i in 1:(pYears+1))
  {
    mor[i,] <- diag(tmpMor)
    tmpMor <- tmpMor[-1,]
  }

  return(mor)
}

#' Retrieve life expectancy at age
#'
#' Follow a cohort from age 0 in year of vaccine introduction
#' to maximum age, retrieving expected years of life left at age
#' @param location country that matches UN data
#' @param yearV year of vaccine introduction
#' @param pYears number of years to project from year of vaccine introduction
#' @param ageV vaccination age
#' @param maxAge maximum age of cohort
#' @param impType the type of impact analysis, Calendar year, Year of birth or Year of Vaccination
#'
#' @return data.frame
#' @export
getLifeExData <- function(location, yearV, pYears, ageV = 0, maxAge = 99, impType)
{
  code <- countrycode::countrycode(location, "country.name", "un")

  tIndex <- rep(1, age_groups$Years[1])
  interval <- rep(age_groups$Years[1], age_groups$Years[1])
  for(i in 2:nrow(age_groups))
  {
    tIndex <- c(tIndex, rep(i, age_groups$Years[i]))
    interval <- c(interval, rep(age_groups$Years[i], age_groups$Years[i]))
  }
  tIndex <- tIndex[1:(maxAge+1)]
  interval <- interval[1:(maxAge+1)]

  #adjust so can be treated as "Year of birth"
  if(impType == "Year of vaccination" || impType == "Calendar year") yearV <- yearV - ageV

  #tmpLifeEx gives "Calendar year" life expectancy remaining
  tmplifeEx <- matrix(NA, nrow = (pYears+1)+maxAge+1, ncol = maxAge+1)
  colnames(tmplifeEx) <- 0:maxAge
  rownames(tmplifeEx) <- as.numeric(yearV):(as.numeric(yearV)+pYears+maxAge+1)

  year <- as.numeric(yearV)
  for(i in 1:nrow(tmplifeEx))
  {
    if(year <2020)
    {

      index <- trunc(((year-1950)/5)+1)
      period <- unique(data.lifeEx.1950_to_2020$Period)[index]
      select <- data.lifeEx.1950_to_2020$`Country code` == code &
        data.lifeEx.1950_to_2020$Period == period
      tmplifeEx[i,] <- as.numeric(data.lifeEx.1950_to_2020[select, -(1:3)])[tIndex]

    } else {

      index <- trunc(((year-2020)/5)+1)
      period <- unique(data.lifeEx.pred2100$Period)[index]
      select <- data.lifeEx.pred2100$`Country code` == code &
        data.lifeEx.pred2100$Period == period
      tmplifeEx[i,] <- as.numeric(data.lifeEx.pred2100[select, -(1:3)])[tIndex]

    }

    year = year +1
    if(year > 2099) year <- 2099 #Use prob of dying from last year of data
    #for all years > 2099
  }

  #Convert to cohort life expectancy remaining
  lifeEx <- matrix(NA, nrow = pYears+1, ncol = maxAge+1)
  colnames(lifeEx) <- 0:maxAge
  rownames(lifeEx) <- as.numeric(yearV):(as.numeric(yearV)+pYears)

  for(i in 1:(pYears+1))
  {
    lifeEx[i,] <- diag(tmplifeEx)
    tmplifeEx <- tmplifeEx[-1,]
  }

  return(lifeEx)
}

#MODEL FUNCTIONS

#' Find transition probabilities at each age from age of vaccination for each state
#'
#' @param probD probability of disease at age without vaccine
#' @param probMort all-cause mortality for cohort at age
#'
#' @return matrix
#' @export
transProb <- function(probD, probMort)
{
  trProb <- cbind(probD, probMort)
  colnames(trProb)[ncol(trProb)] <- "Deceased"

  probWell <- apply(trProb, 1, function(x) 1-sum(x))
  trProb <- cbind(as.matrix(probWell), trProb)
  colnames(trProb)[1] <- "Well"

  return(trProb)
}

#' Find proportion of population immune
#'
#' @param waning assume linear waning immunity, TRUE or FALSE
#' @param durability if linear waning, number of years taken to reach 50% immunity,
#'                 else number of years of full immunity (after which immunity drops to 0)
#' @param efficacy the resulting efficacy proportion, efficacy x coverage
#'
#' @return vector
#' @export
immunity <- function(waning, durability, efficacy)
{
  if(waning == FALSE || durability <2)
  {
    I <- rep(efficacy, durability)
  }else{

    Year <- c(1, durability)
    ImmuneProp <- c(1, 0.5)

    linearWaning <- lm(ImmuneProp ~ Year)
    I <- efficacy*predict(linearWaning, data.frame(Year = 1:(2*durability-1)))
  }

  return(I)
}

#' Find pre-vaccination incidents for a particular cohort using the population
#' size at birth. Function also returns the modeled population values at each age.
#'
#' @param probM matrix of transition probabilities over model time period
#' @param InitPop population size at age of vaccination for cohort of interest
#' @param vAge vaccination age
#' @param maxAge maximum age of cohort
#'
#' @return list(matrix, matrix, matrix)
#' @export
preVaxxModel <- function(probM, InitPop, vAge = 0, maxAge = 99)
{
  counts <- matrix(0, nrow = maxAge + 1 - vAge, ncol = 3)
  colnames(counts) <- colnames(probM)

  pop <- InitPop
  for(i in 1:(maxAge+1-vAge))
  {
    counts[i,] <- probM[i,]*pop
    pop <- pop - counts[i,"Deceased"]
  }

  pop <- counts[, "Well"] + counts[, 2] #Well + Condition

  return(list(t(counts[,2]), pop))
}

#' Scale pre-vaccination incident counts to get counts for a particular
#' vaccination scenario, taking into account immunity and coverage assumptions
#'
#' @param preVacc matrix of pre-vaccination incident counts
#' @param waning assume linear waning immunity, TRUE or FALSE
#' @param durability if linear waning, number of years taken to reach 50%
#'                   immunity, else number of years of full immunity (after
#'                   which immunity drops to 0)
#' @param efficacy the resulting efficacy proportion, efficacy x coverage
#' @param ramp assume linear 10 year ramp up to maximum efficacy from year of introduction
#' @param vaccAge age of vaccination. Default 0
#' @param maxAge maximum age of cohort. Default 99
#'
#' @return matrix
#' @export
propVModel <- function(preVacc, waning, durability, efficacy, vaccAge = 0,
                       ramp, maxAge = 99)
{
  vacc <- preVacc
  immProp <- immunity(waning = waning, durability = durability, efficacy = efficacy)
  totalDur <- ifelse(length(immProp) < maxAge+1-vaccAge,
                                          length(immProp), maxAge+1-vaccAge)

  if(ramp == TRUE) #if ramping up coverage to maximum over 10 years
  {
    rampVec <- seq(0.1, 1, 0.1)
    rampMatrix <- matrix(rampVec, nrow = totalDur , ncol = 10, byrow = T)
    immMatrix <- matrix(immProp, nrow = totalDur, ncol = 10, byrow = F)
    effImm <-immMatrix*rampMatrix
    vacc[1:totalDur, 1:10] <- (1-effImm)*vacc[1:totalDur, 1:10]

    if(ncol(vacc) > 10)
    {
      vacc[1:totalDur, 11:ncol(vacc)] <-
        (1-immProp[1:totalDur])*vacc[1:totalDur, 11:ncol(vacc)]
    }

  }else{

    vacc[1:totalDur,] <- (1-immProp[1:totalDur])*vacc[1:totalDur,]
  }

  return(vacc)

}

#' Calculate DALYs for both pre-vaccination and vaccination scenario. If
#' relevant, also return numbers of deaths for both scenarios
#'
#' @param condition condition name
#' @param noVacc_counts number of cases of condition pre-vaccination
#' @param vacc_counts number of cases of condition after vaccination
#' @param daly_weights used in finding the YLD component of the DALYs it is the
#'                     "adjusted" disability weighting taking into account
#'                     duration of illness. Not used for RHD
#' @param location country that matches UN data
#' @param yearV year of vaccine introduction
#' @param pYears no. of years to project
#' @param vaccAge age of vaccination. Default 0
#' @param maxAge maximum age of cohort
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#'
#' @return list(matrix, matrix, matrix, matrix)
#' @export
findDalys <- function(condition, noVacc_counts, vacc_counts, daly_weights,
                      location, yearV, pYears, vaccAge = 0, maxAge = 99, impType)
{
  code <- countrycode::countrycode(location, "country.name", "iso3c")

  if(condition == "Rheumatic Heart Disease")
  { #Calculate DALYs

    lifeEx <- getLifeExData(location = location, yearV = yearV, pYears = pYears,
                            ageV = vaccAge, impType = impType)[,(vaccAge+1):(maxAge+1)]
    isHIC <- ifelse(data.region[data.region$Code == code, "IncomeGroup"] == "High income", 1, 0)
    return_RHD <- dalysRHD(noVacc_counts, vacc_counts, lifeEx, isHIC)
    return(return_RHD)

  }else{

    if(condition == "Invasive infection")
    {
      probDeath <- probDeath_invasive[probDeath_invasive$TimeSince == 0,]$pDeath
      noVacc_deaths <- probDeath*noVacc_counts
      vacc_deaths <- probDeath*vacc_counts

      lifeEx <- getLifeExData(location = location, yearV = yearV, pYears = pYears,
                              ageV = vaccAge, impType = impType)[,(vaccAge+1):(maxAge+1)]

      noVacc_yll <- noVacc_deaths*t(lifeEx)
      vacc_yll <- vacc_deaths*t(lifeEx)

      #don't add in YLD component from the number that die
      noVacc_yld <- apply(noVacc_counts-noVacc_deaths, 2, function(x) x*daly_weights)
      vacc_yld <- apply(vacc_counts-vacc_deaths, 2, function(x) x*daly_weights)

      noVacc_dalys <- noVacc_yld + noVacc_yll
      vacc_dalys <- vacc_yld + vacc_yll

      return(list(noVacc_dalys, vacc_dalys, noVacc_deaths, vacc_deaths,
                  noVacc_yll, vacc_yll, noVacc_yld, vacc_yld))

    }else{

      noVacc_dalys <- apply(noVacc_counts, 2, function(x) x*daly_weights)
      vacc_dalys <- apply(vacc_counts, 2, function(x) x*daly_weights)

    }

    return(list(noVacc_dalys, vacc_dalys))
  }
}

#' Main function to produce results for vaccine impact scenario
#'
#' @param location country that matches UN data
#' @param condition condition name
#' @param inc incidence rates for each condition
#' @param rate rate used for inc. Default 100,000
#' @param mortality all-cause mortality rate at age
#' @param maxAge maximum age of cohort
#' @param yearV year of vaccine introduction
#' @param vaccAge age of vaccination. Default 0
#' @param vaccEff effectiveness of vaccine, as a percentage
#' @param vaccDur no. of years that vaccine is effective
#' @param waning assume linear waning immunity, TRUE or FALSE
#' @param ramp assume linear 10 year ramp up to maximum efficacy from year of introduction
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#' @param pYears no. of years to project
#' @param initPop population at birth for cohorts of interest
#'
#' @return list
#' @export
runModel<- function(location, condition, inc, rate = 100000, mortality,
                    maxAge = 99, yearV, vaccAge = 0, vaccEff = 100,
                    vaccDur = 10, waning = FALSE, ramp = FALSE, impType,
                    pYears = 10, initPop)
{
  numAgeGroups <- nrow(inc)

  inc <- as.matrix(inc[match(age_groups$Label[1:numAgeGroups], inc$age), "val"])
  rownames(inc) <- age_groups$Label[1:numAgeGroups]
  colnames(inc) <- condition

  vaccEff <- vaccEff/100

  #Incidence matrix as a probability
  IncProb <-inc/rate

  tIndex <- rep(1, age_groups$Years[1])
  for(i in 2:numAgeGroups) tIndex <- c(tIndex, rep(i, age_groups$Years[i]))

  #create matrix with partial DALYs for all ages - NOT USED FOR RHD.
  #This is used in finding the YLD component of the DALYs it is the "adjusted"
  #disability weighting taking into account duration of illness (assuming
  #duration < 1 year). Needs to be multiplied by number of cases
  DW <- disability_weights[disability_weights$Condition == condition,]$DW
  dur <- duration[duration$Condition == condition,]$Days /365.25 #condition duration in years
  dalys <- matrix(DW*dur, nrow = maxAge + 1 - vaccAge, ncol = 1)
  colnames(dalys) <- condition
  rownames(dalys) <- vaccAge:maxAge

  #Create matrix of incident probabilities for all ages, no vaccination
  IncProb <- as.matrix(IncProb[tIndex,])
  rownames(IncProb) <- 0:maxAge
  IncProb <- as.matrix(IncProb[(vaccAge+1):(maxAge+1),])

  allStates <- c("Well", condition, "Deceased")

  #Results matrices
  noVacc_counts <- matrix(NA, nrow = maxAge+1-vaccAge, ncol = pYears+1)
  rownames(noVacc_counts) <- vaccAge:maxAge
  colnames(noVacc_counts) <- yearV:(yearV+pYears)

  vacc_counts <- matrix(NA, nrow = maxAge+1-vaccAge, ncol = pYears+1)
  rownames(vacc_counts) <- vaccAge:maxAge
  colnames(vacc_counts) <- yearV:(yearV+pYears)

  noVacc_dalys <- matrix(NA, nrow = maxAge+1-vaccAge, ncol = pYears+1)
  rownames(noVacc_dalys) <- vaccAge:maxAge
  colnames(noVacc_dalys) <- yearV:(yearV+pYears)

  vacc_dalys <- matrix(NA, nrow = maxAge+1-vaccAge, ncol = pYears+1)
  rownames(vacc_dalys) <- vaccAge:maxAge
  colnames(vacc_dalys) <- yearV:(yearV+pYears)

  noVacc_deaths <- matrix(NA, nrow = maxAge+1-vaccAge, ncol = pYears+1)
  rownames(noVacc_deaths) <- vaccAge:maxAge
  colnames(noVacc_deaths) <- yearV:(yearV+pYears)

  vacc_deaths <- matrix(NA, nrow = maxAge+1-vaccAge, ncol = pYears+1)
  rownames(vacc_deaths) <- vaccAge:maxAge
  colnames(vacc_deaths) <- yearV:(yearV+pYears)

  noVacc_pop <- matrix(NA, nrow = maxAge+1-vaccAge, ncol = pYears+1)
  rownames(noVacc_pop) <- vaccAge:maxAge
  colnames(noVacc_pop) <- yearV:(yearV+pYears)

  for(i in 1:(pYears+1))
  {
    prob0 <- transProb(probD = IncProb,
                       probMort = mortality[i,][(vaccAge+1):(maxAge+1)])
    colnames(prob0) <- allStates

    noVacc_mod <- preVaxxModel(probM = prob0, InitPop = initPop[i,2],
                               maxAge = maxAge, vAge = vaccAge)

    noVacc_counts[,i] <- noVacc_mod[[1]]
    noVacc_pop[,i] <- noVacc_mod[[2]] #pop at end of each year

  }#end for pYears

  #Now find vaccination scenario counts
  vacc_counts <- propVModel(preVacc = noVacc_counts, waning = waning,
                            durability = vaccDur, efficacy = vaccEff,
                            vaccAge = vaccAge, ramp = ramp, maxAge = maxAge)

  return_DALYs <- findDalys(condition = condition, noVacc_counts = noVacc_counts,
                              vacc_counts = vacc_counts, daly_weights = dalys,
                              location = location, yearV = yearV, pYears = pYears,
                              vaccAge = vaccAge, maxAge = maxAge, impType = impType)

  noVacc_dalys <- return_DALYs[[1]]
  vacc_dalys <- return_DALYs[[2]]

  if(condition == "Rheumatic Heart Disease" || condition == "Invasive infection")
  {
    noVacc_deaths <- return_DALYs[[3]]
    vacc_deaths <- return_DALYs[[4]]
    noVacc_yll <- return_DALYs[[5]]
    vacc_yll <- return_DALYs[[6]]
    noVacc_yld <- return_DALYs[[7]]
    vacc_yld <- return_DALYs[[8]]
  }else{
    noVacc_yld <- noVacc_dalys
    vacc_yld <- vacc_dalys
    noVacc_yll <- matrix(0, nrow = maxAge+1-vaccAge, ncol = pYears+1)
    vacc_yll <- noVacc_yll
  }

  rownames(noVacc_dalys) <- vaccAge:maxAge
  rownames(vacc_dalys) <- vaccAge:maxAge
  rownames(noVacc_deaths) <- vaccAge:maxAge
  rownames(vacc_deaths) <- vaccAge:maxAge
  rownames(noVacc_yll) <- vaccAge:maxAge
  rownames(vacc_yll) <- vaccAge:maxAge
  rownames(noVacc_yld) <- vaccAge:maxAge
  rownames(vacc_yld) <- vaccAge:maxAge

  return(list(noVacc_counts, vacc_counts, noVacc_dalys, vacc_dalys,
              noVacc_deaths, vacc_deaths, noVacc_pop,
              noVacc_yll, vacc_yll, noVacc_yld, vacc_yld))
}

#' Find DALYs for RHD
#'
#' @param noVacc_counts pre-vaccine numbers of incidents
#' @param vacc_counts vaccine scenario numbers of incidents
#' @param lifeEx remaining life expectancy by age and year
#' @param HIC_flag if country is a High Income Country value is 1, else 0
#'
#' @return list(matrix, matrix)
#' @export
dalysRHD <- function(noVacc_counts, vacc_counts, lifeEx, HIC_flag)
{
  years <- colnames(noVacc_counts)
  ageRange <- nrow(noVacc_counts)

  rownames(lifeEx) <- years #if ageV !=0, rownames of lifeEx will differ as
                            #are year of birth rather than year of vaccination

  noVacc_yll <- matrix(0, nrow = ageRange, ncol = length(years))
  vacc_yll <- matrix(0, nrow = ageRange, ncol = length(years))
  colnames(noVacc_yll) <- years
  colnames(vacc_yll) <- years

  noVacc_yld <- matrix(0, nrow = ageRange, ncol = length(years))
  vacc_yld <- matrix(0, nrow = ageRange, ncol = length(years))
  colnames(noVacc_yld) <- years
  colnames(vacc_yld) <- years

  noVacc_deaths <- matrix(0, nrow = ageRange, ncol = length(years))
  vacc_deaths <- matrix(0, nrow = ageRange, ncol = length(years))
  colnames(noVacc_deaths) <- years
  colnames(vacc_deaths) <- years


  dw <- disability_weights[disability_weights$Condition == "Rheumatic Heart Disease", ]$DW
  if(HIC_flag == 1)
  {
    prDeath <- probDeath_RHD$pDeath.HIC
  }else{
    prDeath <- probDeath_RHD$pDeath.LMIC
  }

  #for each cohort
  for(y in years)
  {
    for(a in 1:ageRange)
    {
      death_vec_NoV <- noVacc_counts[a,y]*prDeath
      death_vec_V <- vacc_counts[a,y]*prDeath

      end_index <- ifelse((a+9) < ageRange, a+9, ageRange)
      len_vec <- length(a:end_index)

      noVacc_deaths[a:end_index,y] <- noVacc_deaths[a:end_index,y] + death_vec_NoV[1:len_vec]
      vacc_deaths[a:end_index,y] <- vacc_deaths[a:end_index,y] + death_vec_V[1:len_vec]

      #YLL <- deaths[,i]*life[,"le"]
      noVacc_yll[a:end_index,y] <- noVacc_yll[a:end_index,y]+death_vec_NoV[1:len_vec]*lifeEx[y, a:end_index]
      vacc_yll[a:end_index,y] <- vacc_yll[a:end_index,y]+death_vec_V[1:len_vec]*lifeEx[y, a:end_index]

      #YLD  = no.episodes x disability weight
      #YLD attributed to year of incident + remaining years life expectancy
      end_yld_index <- ifelse((a+trunc(lifeEx[y,a])) < ageRange, a+trunc(lifeEx[y,a]), ageRange)
      len_lifeEx <- length(a:end_yld_index)

      partial_yld <- lifeEx[y,a]-trunc(lifeEx[y,a]) #partial life at end_yld_index+1

      # YLD - for those that don't die from RHD incident at year y, at age a-1
      #attribute dw to current year and then future years for each year of remaining life
      #First, full years of life remaining
      noVacc_yld[a:end_yld_index,y] <- noVacc_yld[a:end_yld_index,y] +
                        rep((noVacc_counts[a,y] - sum(death_vec_NoV))*dw, len_lifeEx)
      vacc_yld[a:end_yld_index,y] <- vacc_yld[a:end_yld_index,y] +
                        rep((vacc_counts[a,y] - sum(death_vec_V))*dw, len_lifeEx)
      if(end_yld_index != ageRange)
      { #Add in partial year weight
        noVacc_yld[end_yld_index+1,y] <- noVacc_yld[end_yld_index+1,y] +
                        partial_yld*(noVacc_counts[a,y] - sum(death_vec_NoV))*dw
        vacc_yld[end_yld_index+1,y] <- vacc_yld[end_yld_index+1,y] +
                        partial_yld*(vacc_counts[a,y] - sum(death_vec_V))*dw
      }

      #Add in YLD for those that die from RHD incident occurring in year y,
      #for those years (within 10 years from incident) for which they are alive
      #No contribution to YLD in the year they die in, but YLD for years living
      #in period up to death within 10 years from incident
      end_yld_index <- ifelse(a+9 < ageRange, a+9, ageRange)
      len_vec <- length(a:end_index)
      for(i in 1:(len_vec-1))
      {
        noVacc_yld[(a+i-1),y] <- noVacc_yld[(a+i-1),y] +
                                              sum(death_vec_NoV[(i+1):len_vec])*dw
        vacc_yld[(a+i-1),y] <- vacc_yld[(a+i-1),y] +
                                              sum(death_vec_V[(i+1):len_vec])*dw
      }

    }
  }

  noVacc_dalys <- noVacc_yll + noVacc_yld
  vacc_dalys <- vacc_yll + vacc_yld

  return(list(noVacc_dalys, vacc_dalys, noVacc_deaths, vacc_deaths,
              noVacc_yll, vacc_yll, noVacc_yld, vacc_yld))
}

#' Restructure output to get in calendar year format including cohorts
#' born from 2020
#'
#' @param model2020 list of model results for cohorts from 2020
#' @param modelVaccYear list of model results for cohorts from
#'                       year of vaccine introduction
#'
#' @return list
#' @export
findCalendarYear <- function(modelResults2020, modelResultsFromVaccYear)
{
  noVacc_counts <- modelResults2020[[1]]
  vacc_counts <- modelResultsFromVaccYear[[2]]

  counts <- formatCalendarY(modelResults2020[[1]], modelResultsFromVaccYear[[2]])

  dalys <- formatCalendarY(modelResults2020[[3]], modelResultsFromVaccYear[[4]])

  deaths <- formatCalendarY(modelResults2020[[5]], modelResultsFromVaccYear[[6]])

  pop <- formatCalendarY(modelResults2020[[7]], modelResultsFromVaccYear[[7]])

  yll <- formatCalendarY(modelResults2020[[8]], modelResultsFromVaccYear[[9]])

  yld <- formatCalendarY(modelResults2020[[10]], modelResultsFromVaccYear[[11]])


  return(list(counts[[1]], counts[[2]], dalys[[1]], dalys[[2]],
              deaths[[1]], deaths[[2]], pop[[1]],
              yll[[1]], yll[[2]], yld[[1]], yld[[2]]))

}

#' Restructure output to get in calendar year format.
#'
#' @param no_vacc_data matrix results (counts, DALYs, deaths, pop, yll or yld)
#'                     for 2020 no vaccination scenario
#' @param vacc_data matrix results matrix (counts, DALYs, deaths, pop, yll or yld)
#'                  for vaccination scenario from year of vaccination
#'
#' @return list(matrix, matrix)
#' @export
formatCalendarY <- function(noVacc_data, vacc_data)
{
 new_noVacc <- matrix(NA, nrow = nrow(vacc_data), ncol = ncol(vacc_data))
 new_vacc <- matrix(NA, nrow = nrow(vacc_data), ncol = ncol(vacc_data))
 rownames(new_noVacc) <- rownames(vacc_data)
 colnames(new_noVacc) <- colnames(vacc_data)
 rownames(new_vacc) <- rownames(vacc_data)
 colnames(new_vacc) <- colnames(vacc_data)

 old_vacc <- vacc_data[, ncol(vacc_data):1]
 old_noVacc <- noVacc_data[, ncol(noVacc_data):1]
 for(i in 1:ncol(vacc_data))
 {
  yearDataVacc <- diag(old_vacc)
  yearDataNoVacc <- diag(old_noVacc)

  if(length(yearDataNoVacc) != length(yearDataVacc))
  { # year of vaccination intro > 2020
    yearDataVacc <- c(yearDataVacc, yearDataNoVacc[(length(yearDataVacc)+1):length(yearDataNoVacc)])
  }

  new_vacc[1:length(yearDataVacc),(ncol(new_vacc)-i+1)] <- yearDataVacc
  new_noVacc[1:length(yearDataNoVacc),(ncol(new_noVacc)-i+1)] <- yearDataNoVacc

  if(i != ncol(vacc_data))
  {
    old_vacc <- as.matrix(old_vacc[,-1])
    old_noVacc <- as.matrix(old_noVacc[,-1])
  }
 }

 return(list(new_noVacc, new_vacc))

}



#PRESENTATION OF RESULTS FUNCTIONS

#' Present results in age groups.
#'
#' Note: Currently only working if maximum age of cohort is the upper
#' limit on an age group, with age groups as defined in lookup table
#'
#' @param age_data cases, DALYs, deaths results by yearly age
#'
#' @return data.frame
#' @export
groupResults <- function(age_data)
{
  years <- colnames(age_data)
  numAges <- nrow(age_data)
  numGroups <- which(cumsum(age_groups$Years) == numAges)
  group_data <- matrix(NA, nrow = numGroups, ncol = length(years))
  colnames(group_data) <- paste("Year:", years)
  rownames(group_data) <- age_groups$Label[1:numGroups]

  #Change back to age_groups
  j <- 1
  for(i in 1:numGroups)
  {
    if(age_groups$Years[i] > 1)
    {
      group_data[i,] <- apply(age_data[j:(j+age_groups$Years[i]-1),], 2, sum)
    }else{
      group_data[i,] <- age_data[j,]
    }

    j <- j+age_groups$Years[i]
  }

  return(group_data)
}

#' Create pairs of barplots comparing vaccine scenarios values to pre-vaccine values
#'
#' @param noVacc_data counts, deaths OR DALYs for pre-vaccine scenario
#' @param vacc_data counts, deaths OR DALYs for vaccine scenario
#' @param ylabel label for y-axis
#' @param vAge vaccination age
#' @param maxAge maximum age of cohort
#' @param vYear year of vaccine introduction
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#' @param pYears no. of years to project
#'
#' @return ggplot
#' @export
makePlot <- function(noVacc_data, vacc_data, ylabel, vAge, maxAge = 99, vYear,
                     impType, pYears)
{
  yearStart <- ifelse(impType == "Year of birth", vYear-vAge, vYear)

  rownames(noVacc_data) <- vAge:maxAge
  colnames(noVacc_data) <- yearStart:(yearStart+pYears)
  rownames(vacc_data) <- vAge:maxAge
  colnames(vacc_data) <- yearStart:(yearStart+pYears)

  noVacc_data[which(is.na(noVacc_data))] <- 0
  vacc_data[which(is.na(vacc_data))] <- 0

  totalNoVacc <- apply(noVacc_data, 2, sum) #pre-vaxx total no. incidents, deaths or dalys

  ymax <- max(totalNoVacc)*1.01

  meltNoVacc <- reshape2::melt(noVacc_data)
  colnames(meltNoVacc) <- c("Age", "Year", "Numbers")

  meltVacc <- reshape2::melt(vacc_data)
  colnames(meltVacc) <- c("Age", "Year", "Numbers")

  xlabel <- impType
  ylabel_noVacc <- paste(ylabel, "(pre-vacc)")
  ylabel_vacc <- paste(ylabel, "(vacc)")

  xtick_lab <- as.character(yearStart:(yearStart+pYears))
  ticks <- 1:(pYears+1)
  if(pYears > 10) xtick_lab[ticks[-seq(from = 1, to = pYears+1, by = 5)]] <- ""

  p_noVacc <- ggplot2::ggplot(data=meltNoVacc, ggplot2::aes(x=factor(Year), y=Numbers, fill=Age )) +
    ggplot2::geom_bar(position="stack", stat="identity")+
    ggplot2::ylab(ylabel_noVacc)+
    ggplot2::xlab(xlabel)+
    ggplot2::scale_x_discrete(labels=xtick_lab)+
    ggplot2::scale_fill_gradientn(colours = grDevices::rainbow(18))+
    ggplot2::ylim(0, ymax)

  p_vacc <- ggplot2::ggplot(data=meltVacc, ggplot2::aes(x=factor(Year), y=Numbers, fill=Age )) +
    ggplot2::geom_bar(position="stack", stat="identity")+
    ggplot2::ylab(ylabel_vacc)+
    ggplot2::xlab(xlabel)+
    ggplot2::scale_x_discrete(labels=xtick_lab)+
    ggplot2::scale_fill_gradientn(colours = grDevices::rainbow(18))+
    ggplot2::ylim(0, ymax)


  return(list(p_noVacc, p_vacc))
}

#' Create bar plot (with uncertainty values) of current (2019) data
#'
#' Incidence, DALYs or deaths per 100,000 persons
#'
#' @param plot_data data frame with age group, val
#'             and upper and lower 95% CI of one of Incidence, DALYs or deaths
#' @param ylabel y-axis label
#' @param colFill colour of bars
#'
#' @return ggplot
#' @export
makeBarPlot <- function(plot_data, ylabel, colFill = "steelblue")
{
  plot_data <- plot_data[match(age_groups$Label, plot_data$age),]
  plot_data$age <- factor(plot_data$age, levels = plot_data$age)

  intFlag <-"lower" %in% colnames(plot_data) #check if data includes confidence Intervals

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x=age, y=val)) +
    ggplot2::geom_bar(position=ggplot2::position_dodge(),
                      stat="identity", fill = colFill) +
    {if(intFlag) ggplot2::geom_errorbar(ggplot2::aes(ymin=lower, ymax=upper),
                                        width = 0.2, position=ggplot2::position_dodge(0.9))} +
    ggplot2::ylab(ylabel) +
    ggplot2::xlab("Age")

  return(p)
}

#' Create plots of counts, deaths or DALYs averted
#'
#' @param noVacc_data counts, deaths OR DALYs for pre-vaccine scenario
#' @param vacc_data counts, deaths OR DALYs for vaccine scenario
#' @param ylabel label for y-axis
#' @param vAge vaccination age
#' @param maxAge maximum age of cohort
#' @param vYear year of vaccine introduction
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#' @param pYears no. of years to project
#'
#' @return ggplot
#' @export
makePlotAvert <- function(noVacc_data, vacc_data, ylabel, vAge, maxAge = 99,
                     vYear, impType, pYears)
{
  yearStart <- ifelse(impType == "Year of birth", vYear-vAge, vYear)

  rownames(noVacc_data) <- vAge:maxAge
  colnames(noVacc_data) <- yearStart:(yearStart+pYears)
  rownames(vacc_data) <- vAge:maxAge
  colnames(vacc_data) <- yearStart:(yearStart+pYears)

  avert <- noVacc_data - vacc_data
  totalAverted <- apply(avert, 2, sum) #pre-vaxx total no. incidents, deaths or dalys

  ymax <- max(totalAverted)*1.01

  melt_avert <- reshape2::melt(avert)
  colnames(melt_avert) <- c("Age", "Year", "Numbers")

  xlabel <- impType
  ylabel <- paste(ylabel, "averted")

  xtick_lab <- as.character(yearStart:(yearStart+pYears))
  ticks <- 1:(pYears+1)
  if(pYears > 10) xtick_lab[ticks[-seq(from = 1, to = pYears+1, by = 5)]] <- ""

  p_avert <- ggplot2::ggplot(data=melt_avert, ggplot2::aes(x=factor(Year), y=Numbers, fill=Age )) +
    ggplot2::geom_bar(position="stack", stat="identity")+
    ggplot2::ylab(ylabel)+
    ggplot2::xlab(xlabel)+
    ggplot2::scale_x_discrete(labels=xtick_lab)+
    ggplot2::scale_fill_gradientn(colours = grDevices::rainbow(18))+
    ggplot2::ylim(0, ymax)

  return(p_avert)
}


