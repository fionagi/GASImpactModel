#' Retrieve all countries in a particular region
#'
#' @param region UN region
#'
#' @return vector
#' @export
getCountries <- function(region)
{
  if(region == "All")
  {
    countries <- intersect(data.region$Country, unique(data.rhd2019$location))
  } else{
    countries <- intersect(data.region$Country[which(data.region$Region == region)], unique(data.rhd2019$location))
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

    data <- get(condData)
    inc <- data[data$location==location & data$measure=="Incidence" & data$metric==metric,]
    deaths <- data[data$location==location & data$measure=="Deaths" & data$metric==metric,]
    dalys <- data[data$location==location & data$measure=="DALYs (Disability-Adjusted Life Years)" & data$metric==metric,]

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
#' @param impType the type of impact analysis, Calendar year, Year of birth or Year of Vaccination
#'
#' @return data.frame
#' @export
getInitPop <- function(location, yearV, pYears, ageV, impType)
{

  if(impType == "Year of vaccination" || impType == "Calendar year") yearV <- yearV - ageV

  #find birth numbers for each year of birth of interest
  pop <- matrix(NA, nrow = pYears+1, ncol = 2)
  colnames(pop) <- c("Birth year", "Pop at birth")
  pop[,1] <- yearV:(yearV+pYears)

  year <- as.numeric(yearV)
  for(i in 1:(pYears+1))
  {

    if(year < 2020)
    {
      select <- data.popbyage.1950_to_2020$Country == location &
        data.popbyage.1950_to_2020$Year == year
      pop[i,2] <- as.numeric(data.popbyage.1950_to_2020[select, "0"])*1000 #pop in thousands

    }else{

      select <- data.popbyage.pred$Country == location &
        data.popbyage.pred$Year == year
      pop[i,2] <- as.numeric(data.popbyage.pred[select, "0"])*1000 #pop in thousands

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

  age <- as.character(0:maxAge)

  year <- as.numeric(yearV)
  for(i in 1:(pYears+1))
  {

    if(year < 2020)
    {
      select <- data.popbyage.1950_to_2020$Country == location &
        data.popbyage.1950_to_2020$Year == year
      pop[,i] <- as.numeric(data.popbyage.1950_to_2020[select, age])*1000 #pop in thousands

    }else{

      select <- data.popbyage.pred$Country == location &
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
      select <- data.mortality.1950_to_2020$Location == location &
        data.mortality.1950_to_2020$Period == period
      tmpMor[i,] <- as.numeric(data.mortality.1950_to_2020[select, "Probability of dying q(x,n)"])[tIndex]
      tmpMor[i,] <- 1 - exp(log(1-tmpMor[i,])/interval) #change to single year probability

    } else {

      if(year < 2050)
      {

        index <- trunc(((year-2020)/5)+1)
        period <- unique(data.mortality.pred2050$Period)[index]
        select <- data.mortality.pred2050$Location == location &
          data.mortality.pred2050$Period == period
        tmpMor[i,] <- as.numeric(data.mortality.pred2050[select, "Probability of dying q(x,n)"])[tIndex]
        tmpMor[i,] <- 1 - exp(log(1-tmpMor[i,])/interval) #change to single year probability

      } else {

        index <- trunc(((year-2050)/5)+1)
        period <- unique(data.mortality.pred2100$Period)[index]
        select <- data.mortality.pred2100$Location == location &
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
      select <- data.lifeEx.1950_to_2020$Location == location &
        data.lifeEx.1950_to_2020$Period == period
      tmplifeEx[i,] <- as.numeric(data.lifeEx.1950_to_2020[select, -(1:2)])[tIndex]

    } else {

      index <- trunc(((year-2020)/5)+1)
      period <- unique(data.lifeEx.pred2100$Period)[index]
      select <- data.lifeEx.pred2100$Location == location &
        data.lifeEx.pred2100$Period == period
      tmplifeEx[i,] <- as.numeric(data.lifeEx.pred2100[select, -(1:2)])[tIndex]

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

#' Find transition probabilities for Markov model
#'
#' @param prD probability of disease without vaccine
#' @param lifeTable all-cause mortality
#' @param maxAge number of years over which transition probs. calculated
#' @param V_Eff vaccine effectiveness, reduce prob. by this proportion
#' @param V_Age age when vaccine is administered
#' @param V_Dur length of time that vaccine is effective for
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#'
#' @return matrix
#' @export
transProb <- function(prD, lifeTable, maxAge, V_Eff, V_Age, V_Dur, impType)
{
  trProb <- prD

  if(V_Eff != 0) #if vaccinating
  {
    #if(impType == "Year of birth" || impType == "Year of vaccination")
    #{
    vTime <- (1+V_Age):(V_Age+V_Dur) #from time of vacc to end of duration of effectiveness
    trProb[vTime,] <- (1-V_Eff)*trProb[vTime,] #reduce prob. of incidents due to vaccine
    #}
  }

  trProb <- cbind(trProb, as.numeric(lifeTable[1:(maxAge+1)]))
  colnames(trProb)[ncol(trProb)] <- "Deceased"

  PrWell <- apply(trProb, 1, function(x) 1-sum(x))
  trProb <- cbind(as.matrix(PrWell), trProb)
  colnames(trProb)[1] <- "Well"

  return(trProb)
}

#' Find proportion of population immmune
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

#' Heemod alternative
#'
#' @param probM matrix of transition probabilities over model time period
#' @param dalyWeights matrix of partial DALY (pre-count calculation) for each age and condition
#'                    Note: This will be NA for RHD as separate function used
#' @param Initpop initial population size
#' @param ageInit inital age
#' @param maxAge maximum age of cohort
#'
#' @return list(matrix, matrix, matrix)
#' @export
propModel <- function(probM, daly_weights, InitPop, initialAge = 0, maxAge = 99)
{
  counts <- matrix(0, nrow = maxAge + 1, ncol = 3)
  colnames(counts) <- colnames(probM)

  pop <- InitPop
  for(i in 1:(maxAge+1))
  {
    counts[i,] <- probM[i,]*pop
    pop <- pop - counts[i,"Deceased"]
  }

  dalys <- counts[,2]*daly_weights
  pop <- counts[, "Well"] + counts[, 2] #Well + Condition

  return(list(t(counts[,2]), dalys, pop))
}


#' Run Markov models for vaccine impact scenario
#' NOTE: Cost, DALYS and discounting not currently used.
#' Only written for one condition though some programming started to allow for
#' multiple condition to be modeled at the same time
#'
#' @description Run Markov models for vaccine impact scenario and produce table
#' of comparison metrics. This function calls transProb and heemodModel
#'
#' @param location country that matches UN data
#' @param condition condition name
#' @param inc incidence rates for each condition
#' @param propA proportion of incidents attributable to Strep A. Default 1.
#' @param rate rate used for inc. Default 100,000
#' @param mortality all-cause mortality rate at age
#' @param dRate discount rate as a percentage. Default 0%
#' @param maxAge maximum age of cohort
#' @param yearV year of vaccine introduction
#' @param vaccAge age of vaccination. Default 0
#' @param vaccEff effectiveness of vaccine, as a percentage
#' @param vaccDur no. of years that vaccine is effective
#' @param waning assume linear waning immunity, TRUE or FALSE
#' @param ramp assume linear 10 year ramp up to maximum efficacy from year of introduction
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#' @param pYears no. of years to project
#' @param initPop used for modeling if metric is Number
#'
#' @return list
#' @export
runModel<- function(location, condition, inc, propA = 1, rate = 100000,
                    mortality, dRate = 0, maxAge = 99, yearV, vaccAge = 0,
                    vaccEff = 100, vaccDur = 10, waning = FALSE, ramp = FALSE,
                    impType, pYears = 10, initPop)
{
  inc <- as.matrix(inc[match(age_groups$Label, inc$age), "val"])
  rownames(inc) <- age_groups$Label
  colnames(inc) <- condition

  numAgeGroups <- nrow(age_groups)

  dRate <- dRate/100
  vaccEff <- vaccEff/100

  #Not using costs, just put in dummy data
  costs <- matrix(rep(1, nrow(age_groups)*length(condition)),
                  nrow = nrow(age_groups), ncol = length(condition) )
  rownames(costs) <- age_groups$Label
  colnames(costs) <- condition

  #Incidence matrix as a probability
  IncidenceProb <-inc/rate
  IncidenceProb <- IncidenceProb*propA #Multiply by proportion attributable to Strep A

  tIndex <- rep(1, age_groups$Years[1])
  for(i in 2:numAgeGroups) tIndex <- c(tIndex, rep(i, age_groups$Years[i]))

  #create matrix with all costs (per incident) for all ages
  CostPerIncid <- costs/IncidenceProb
  CostPerIncid[which(is.na(CostPerIncid)|is.infinite(CostPerIncid))] <- 0
  cost <- as.matrix(CostPerIncid[tIndex,])
  rownames(cost) <- 0:maxAge

  #create matrix with all DALYs for all ages - NOT USED FOR RHD
  #Currently does not allow for deaths due to the condition, i.e. assumes YLL=0
  #Below is NOT full YLD, just the "adjusted" weighting taking into account duration
  #of illness (assuming duration < 1 year). Needs to be multiplied by number of cases
  DW <- disability_weights[disability_weights$Condition == condition,]$DW
  dur <- duration[duration$Condition == condition,]$Days /365.25 #condition duration in years
  dalys <- matrix(DW*dur, nrow = maxAge + 1, ncol = 1)
  colnames(dalys) <- condition
  rownames(dalys) <- 0:maxAge

  #Create matrix with all transition probabilities for all ages
  #base transition prob, no vaccination
  prob <- as.matrix(IncidenceProb[tIndex,])
  rownames(prob) <- 0:maxAge
  allStates <- c("Well", condition, "Deceased")

  #Results matrices
  noVacc_counts <- matrix(NA, nrow = maxAge+1, ncol = pYears+1)
  rownames(noVacc_counts) <- 0:maxAge
  colnames(noVacc_counts) <- yearV:(yearV+pYears)

  vacc_counts <- matrix(NA, nrow = maxAge+1, ncol = pYears+1)
  rownames(vacc_counts) <- 0:maxAge
  colnames(vacc_counts) <- yearV:(yearV+pYears)

  noVacc_dalys <- matrix(NA, nrow = maxAge+1, ncol = pYears+1)
  rownames(noVacc_dalys) <- 0:maxAge
  colnames(noVacc_dalys) <- yearV:(yearV+pYears)

  vacc_dalys <- matrix(NA, nrow = maxAge+1, ncol = pYears+1)
  rownames(vacc_dalys) <- 0:maxAge
  colnames(vacc_dalys) <- yearV:(yearV+pYears)

  noVacc_deaths <- matrix(NA, nrow = maxAge+1, ncol = pYears+1)
  rownames(noVacc_deaths) <- 0:maxAge
  colnames(noVacc_deaths) <- yearV:(yearV+pYears)

  vacc_deaths <- matrix(NA, nrow = maxAge+1, ncol = pYears+1)
  rownames(vacc_deaths) <- 0:maxAge
  colnames(vacc_deaths) <- yearV:(yearV+pYears)

  noVacc_pop <- matrix(NA, nrow = maxAge+1, ncol = pYears+1)
  rownames(noVacc_pop) <- 0:maxAge
  colnames(noVacc_pop) <- yearV:(yearV+pYears)

  for(i in 1:(pYears+1))
  {
    prob0 <- transProb(prob, mortality[i,], maxAge, 0)
    colnames(prob0) <- allStates

    noVacc_mod <- propModel(prob0, dalys, initPop[i,2], 0, maxAge)

    pop_end_of_year <- noVacc_mod$eval_strategy_list$standard$counts$Disease1 + noVacc_mod$eval_strategy_list$standard$counts$Well #population

    noVacc_counts[,i] <- noVacc_mod[[1]]
    noVacc_dalys[,i] <- noVacc_mod[[2]]
    noVacc_pop[,i] <- noVacc_mod[[3]] #pop at end of each year

  }#end for pYears

  #Now scale values to get required vaccination scenario results
  vacc_counts <- noVacc_counts
  immProp <- immunity(waning = waning, durability = vaccDur, efficacy = vaccEff)
  totalDur <- ifelse(vaccAge+length(immProp) < maxAge+1, length(immProp), maxAge+1-vaccAge)

  if(ramp == TRUE) #if ramping up coverage to maximum over 10 years
  {
   rampVec <- seq(0.1, 1, 0.1)
   rampMatrix <- matrix(rampVec, nrow = totalDur , ncol = 10, byrow = T)
   immMatrix <- matrix(immProp, nrow = totalDur, ncol = 10, byrow = F)
   effImm <-immMatrix*rampMatrix
   vacc_counts[(vaccAge+1):(vaccAge+totalDur), 1:10] <-
          (1-effImm)*vacc_counts[(vaccAge+1):(vaccAge+totalDur), 1:10]
   if(ncol(vacc_counts) > 10)
   {
    vacc_counts[(vaccAge+1):(vaccAge+totalDur), 11:ncol(vacc_counts)] <-
          (1-immProp[1:totalDur])*vacc_counts[(vaccAge+1):(vaccAge+totalDur), 11:ncol(vacc_counts)]
   }
  }else{
    vacc_counts[(vaccAge+1):(vaccAge+totalDur),] <- (1-immProp[1:totalDur])*vacc_counts[(vaccAge+1):(vaccAge+totalDur),]
  }

  if(impType != "Calendar year")
  {
    if(condition != "Rheumatic Heart Disease")
    {
      vacc_dalys <- noVacc_dalys
      vacc_dalys[(vaccAge+1):(vaccAge+totalDur),] <- (1-immProp[1:totalDur])*vacc_dalys[(vaccAge+1):(vaccAge+totalDur),]
    }else{
      lifeEx <- getLifeExData(location = location, yearV = yearV, pYears = pYears,
                              ageV = vaccAge, impType = impType)
      isHIC <- ifelse(data.region[data.region$Country == location, "IncomeGroup"] == "High income", 1, 0)
      return_RHD <- dalysRHD(noVacc_counts, vacc_counts, lifeEx, isHIC)
      noVacc_dalys <- return_RHD[[1]]
      vacc_dalys <- return_RHD[[2]]
      noVacc_deaths <- return_RHD[[3]]
      vacc_deaths <- return_RHD[[4]]
    }
  }else{#then above counts are for cohorts that are affected by
    # vaccination but need to find the affect by calendar year
    pop <- getPopData(location = location, yearV = yearV, pYears = pYears, maxAge = maxAge)
    noVacc_counts_cal <- apply(pop, 2, function(x) x*prob)
    vacc_counts_cal <- noVacc_counts_cal

    for(i in 1:(pYears +1))
    {
      newValues <- vacc_counts[vaccAge+1,i]

      if(i > 1)
      {
        row <- vaccAge+2
        for(j in (i-1):1)
        {
          newValues <- c(newValues, vacc_counts[row, j])
          row <- row +1
        }
      }

      select <- (vaccAge+1):(vaccAge+i)
      vacc_counts_cal[select, i] <- newValues
    }

    vacc_counts <- vacc_counts_cal
    noVacc_counts <- noVacc_counts_cal

    if(condition != "Rheumatic Heart Disease")
    {
      vacc_dalys <- apply(vacc_counts, 2, function(x) x*dalys)
      noVacc_dalys <- apply(noVacc_counts, 2, function(x) x*dalys)
    }

  }#end if(impType == "Calendar year")

  if(condition == "Invasive infection")
  {
    probDeath <- probDeath_invasive[probDeath_invasive$TimeSince == 0,]$pDeath
    noVacc_deaths <- probDeath*noVacc_counts
    vacc_deaths <- probDeath*vacc_counts

    lifeEx <- getLifeExData(location = location, yearV = yearV, pYears = pYears,
                            ageV = vaccAge, impType = impType)

    noVacc_yll <- noVacc_deaths*t(lifeEx)
    vacc_yll <- vacc_deaths*t(lifeEx)

    noVacc_dalys <- noVacc_dalys + noVacc_yll
    vacc_dalys <- vacc_dalys + vacc_yll
  }

  return(list(noVacc_counts, vacc_counts, noVacc_dalys, vacc_dalys,
              noVacc_deaths, vacc_deaths, noVacc_pop))
}

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
#' @param vDur vaccination durability
#' @param vYear year of vaccine introduction
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#' @param pYears no. of years to project
#'
#' @return ggplot
#' @export
makePlot <- function(noVacc_data, vacc_data, ylabel, vAge, maxAge = 99, vDur,
                     vYear, impType, pYears)
{
  yearStart <- ifelse(impType == "Year of birth", vYear-vAge, vYear)

  rownames(noVacc_data) <- 0:maxAge
  colnames(noVacc_data) <- yearStart:(yearStart+pYears)
  rownames(vacc_data) <- 0:maxAge
  colnames(vacc_data) <- yearStart:(yearStart+pYears)

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


#' Find DALYs for RHD
#'
#' @param noVacc_counts pre-vaccine numbers of incidents
#' @param vacc_counts vaccine scenario numbers of incidents
#'
#' @return list(matrix, matrix)
#' @export
dalysRHD <- function(noVacc_counts, vacc_counts, lifeEx, HIC_flag)
{
  years <- colnames(noVacc_counts)
  maxAge <- nrow(noVacc_counts) - 1

  rownames(lifeEx) <- years #if ageV !=0, rownames of lifeEx will differ as
                            #as are year of birth rather than year of vaccination

  noVacc_yll <- matrix(0, nrow = maxAge + 1, ncol = length(years))
  vacc_yll <- matrix(0, nrow = maxAge + 1, ncol = length(years))
  colnames(noVacc_yll) <- years
  colnames(vacc_yll) <- years

  noVacc_yld <- matrix(0, nrow = maxAge + 1, ncol = length(years))
  vacc_yld <- matrix(0, nrow = maxAge + 1, ncol = length(years))
  colnames(noVacc_yld) <- years
  colnames(vacc_yld) <- years

  noVacc_deaths <- matrix(0, nrow = maxAge + 1, ncol = length(years))
  vacc_deaths <- matrix(0, nrow = maxAge + 1, ncol = length(years))
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
    for(a in 1:(maxAge+1))
    {
      death_vec_NoV <- noVacc_counts[a,y]*prDeath
      death_vec_V <- vacc_counts[a,y]*prDeath

      end_index <- ifelse((a+9) < (maxAge+1), a+9, maxAge+1)
      len_vec <- length(a:end_index)

      noVacc_deaths[a:end_index,y] <- noVacc_deaths[a:end_index,y] + death_vec_NoV[1:len_vec]
      vacc_deaths[a:end_index,y] <- vacc_deaths[a:end_index,y] + death_vec_V[1:len_vec]

      #YLL <- deaths[,i]*life[,"le"]
      noVacc_yll[a:end_index,y] <- noVacc_yll[a:end_index,y]+death_vec_NoV[1:len_vec]*lifeEx[y, a:end_index]
      vacc_yll[a:end_index,y] <- vacc_yll[a:end_index,y]+death_vec_V[1:len_vec]*lifeEx[y, a:end_index]

      #YLD  = no.episodes x disability weight
      #YLD attributed to year of incident + remaining years life expectancy
      # i.e. lifeEx + 1
      end_yld_index <- ifelse((a+trunc(lifeEx[y,a])+1) < (maxAge+1), a+trunc(lifeEx[y,a]+1), maxAge+1)
      len_lifeEx <- length(a:end_yld_index)

      partial_yld <- lifeEx[y,a]-trunc(lifeEx[y,a]) #partial life at end_yld_index+1

      # YLD - for those that don't die from RHD incident at year y, at age a-1
      #attribute dw to current year and then future years for each year of remaining life
      #First, full years of life remaining
      noVacc_yld[a:end_yld_index,y] <- noVacc_yld[a:end_yld_index,y] +
                        rep((noVacc_counts[a,y] - sum(death_vec_NoV))*dw, len_lifeEx)
      vacc_yld[a:end_yld_index,y] <- vacc_yld[a:end_yld_index,y] +
                        rep((vacc_counts[a,y] - sum(death_vec_V))*dw, len_lifeEx)
      if(end_yld_index != maxAge+1)
      { #Add in partial year weight
        noVacc_yld[end_yld_index+1,y] <- noVacc_yld[end_yld_index+1,y] +
                        partial_yld*(noVacc_counts[a,y] - sum(death_vec_NoV))*dw
        vacc_yld[end_yld_index+1,y] <- vacc_yld[end_yld_index+1,y] +
                        partial_yld*(vacc_counts[a,y] - sum(death_vec_V))*dw
      }

      #Add in YLD for those that die from RHD incident at year y. Assume whole
      #year of YLD in the year they die in, plus YLD for years living in period
      #up to death within 10 years from incident
      end_yld_index <- ifelse(a+9 < (maxAge+1), a+9, maxAge+1)
      len_vec <- length(a:end_index)
      for(i in 1:len_vec)
      {
        noVacc_yld[(a+i-1),y] <- noVacc_yld[(a+i-1),y] +
                                              sum(death_vec_NoV[i:len_vec])*dw
        vacc_yld[(a+i-1),y] <- vacc_yld[(a+i-1),y] +
                                              sum(death_vec_V[i:len_vec])*dw
      }

    }
  }

  noVacc_dalys <- noVacc_yll + noVacc_yld
  vacc_dalys <- vacc_yll + vacc_yld

  return(list(noVacc_dalys, vacc_dalys, noVacc_deaths, vacc_deaths))
}

