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

  age <- as.character(0:84)

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
#' Follow a cohort from age 0 in year of vaccine introduction for
#' a period of 85 years, retrieving probability of death for each year of life
#' @param location country that matches UN data
#' @param yearV year of vaccine introduction
#' @param pYears number of years to project from year of vaccine introduction
#' @param ageV vaccination age
#' @param impType the type of impact analysis, Calendar year, Year of birth or Year of Vaccination
#'
#' @return data.frame
#' @export
getMorData <- function(location, yearV, pYears, ageV = 0, impType)
{
  tIndex <- rep(1, age_groups$Years[1])
  interval <- rep(age_groups$Years[1], age_groups$Years[1])
  for(i in 2:nrow(age_groups))
  {
    tIndex <- c(tIndex, rep(i, age_groups$Years[i]))
    interval <- c(interval, rep(age_groups$Years[i], age_groups$Years[i]))
  }

  #adjust so can be treated as "Year of birth"
  if(impType == "Year of vaccination" || impType == "Calendar year") yearV <- yearV - ageV

  tmpMor <- matrix(NA, nrow = (pYears+1)+85, ncol = 85)
  colnames(tmpMor) <- 0:84
  rownames(tmpMor) <- as.numeric(yearV):(as.numeric(yearV)+pYears+85)

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

  mor <- matrix(NA, nrow = pYears+1, ncol = 85)
  colnames(mor) <- 0:84
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
#' Follow a cohort from age 0 in year of vaccine introduction for
#' a period of 85 years, retrieving expected years of life left at age
#' @param location country that matches UN data
#' @param yearV year of vaccine introduction
#' @param pYears number of years to project from year of vaccine introduction
#' @param ageV vaccination age
#' @param impType the type of impact analysis, Calendar year, Year of birth or Year of Vaccination
#'
#' @return data.frame
#' @export
getLifeExData <- function(location, yearV, pYears, ageV = 0, impType)
{
  tIndex <- rep(1, age_groups$Years[1])
  interval <- rep(age_groups$Years[1], age_groups$Years[1])
  for(i in 2:nrow(age_groups))
  {
    tIndex <- c(tIndex, rep(i, age_groups$Years[i]))
    interval <- c(interval, rep(age_groups$Years[i], age_groups$Years[i]))
  }


  #adjust so can be treated as "Year of birth"
  if(impType == "Year of vaccination" || impType == "Calendar year") yearV <- yearV - ageV

  #tmpLifeEx gives "Calendar year" life expectancy remaining
  tmplifeEx <- matrix(NA, nrow = (pYears+1)+85, ncol = 85)
  colnames(tmplifeEx) <- 0:84
  rownames(tmplifeEx) <- as.numeric(yearV):(as.numeric(yearV)+pYears+85)

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
  lifeEx <- matrix(NA, nrow = pYears+1, ncol = 85)
  colnames(lifeEx) <- 0:84
  rownames(lifeEx) <- as.numeric(yearV):(as.numeric(yearV)+pYears)

  for(i in 1:(pYears+1))
  {
    lifeEx[i,] <- diag(tmplifeEx)
    tmplifeEx <- tmplifeEx[-1,]
  }

  return(lifeEx)
}

#' Find incidence probabilities for vaccination scenario
#'
#' @param prI probability of incidence at age without vaccine
#' @param V_Eff vaccine effectiveness, reduce probability of incidence by this proportion
#' @param V_Age age when vaccine is administered
#' @param V_Dur length of time that vaccine is effective for
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#' @param pYears number of years to project from year of vaccine introduction
#'
#' @return matrix
#' @export
incProb <- function(prI, maxAge, V_Eff, V_Age, V_Dur, impType, pYears)
{
  incP <- matrix(rep(prI, pYears +1), nrow = maxAge +1, ncol = pYears + 1, byrow = F)
  colnames(incP) <- 0:pYears
  rownames(incP) <- 0:maxAge

  if(V_Eff != 0) #if vaccinating
  {
    if(impType == "Year of birth" || impType == "Year of vaccination")
    {
      vTime <- (1+V_Age):(V_Age+V_Dur) #from time of vacc to end of duration of effectiveness
      incP[vTime,] <- (1-V_Eff)*incP[vTime,] #reduce prob. of incidents due to vaccine
    } else {
      #impType == "Calendar year"
      d <- 1
      for(i in 1:(pYears +1))
      {
        vTime <- (1+V_Age):(V_Age+d) #from time of vacc to end of duration of effectiveness
        incP[vTime, i] <- (1-V_Eff)*incP[vTime, i] #reduce prob. of incidents due to vaccine
        d <- d+1
        if(d > V_Dur) d <- V_Dur
      }
    }
  }

  return(incP)
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
#' @param duration if linear waning, number of years taken to reach 50% immunity,
#'                 else number of years of full immunity (after which immunity drops to 0)
#' @param efficacy the resulting efficacy proportion, efficacy x coverage
#'
#' @return vector
#' @export
immunity <- function(waning, duration, efficacy)
{
  if(waning == FALSE || duration <2)
  {
    I <- rep(efficacy, duration)
  }else{

    Year <- c(1, duration)
    ImmuneProp <- c(1, 0.5)

    linearWaning <- lm(ImmuneProp ~ Year)
    I <- efficacy*predict(linearWaning, data.frame(Year = 1:(2*duration-1)))
  }

  return(I)
}

#' Set up and run heemod Markov model
#' @description Set up and run heemod Markov model for a maximum of 10 disease states
#'
#' @param probM matrix of transition probabilities over model time period
#' @param dalysM matrix of DALYS for each age and condition
#' @param dR discount rate as a percentage
#' @param costM matrix of costs for each age and condition
#' @param Initpop initial population size
#' @param ageInit age when vaccine administered
#' @param cycleT age at end of model run
#'
#' @return heemod model
#' @export
heemodModel <-function(probM, dalysM, dR, costM, Initpop, ageInit, cycleT)
{
  numStates <- ncol(probM) #Always Well and Deceased State, plus diseased states
  maxDStates <-10 #max. diseased states
  noDummyStates <- maxDStates-numStates+2 #2 non-diseased states : Well, Deceased

  StateN <- "Well"
  for(i in 1:maxDStates) StateN <- c(StateN, paste("Disease", i, sep=''))
  StateN <- c(StateN, "Deceased")

  if(noDummyStates>0)
  {
    dummyMat <- matrix(0, nrow=nrow(probM), ncol=noDummyStates)
    probM <- cbind(probM[,-which(colnames(probM)=="Deceased")], dummyMat, probM[,"Deceased"])
    dalysM <- cbind(dalysM, dummyMat)
    costM <- cbind(costM, dummyMat)
  }

  #At the moment, maximum of 12 states in total

  model_param <- heemod::define_parameters(
    age = ageInit + markov_cycle,
    prWell = probM[age, 1],
    prDis1 = probM[age, 2],
    prDis2 = probM[age, 3],
    prDis3 = probM[age, 4],
    prDis4 = probM[age, 5],
    prDis5 = probM[age, 6],
    prDis6 = probM[age, 7],
    prDis7 = probM[age, 8],
    prDis8 = probM[age, 9],
    prDis9 = probM[age, 10],
    prDis10 = probM[age, 11],
    prDec = probM[age, 12]
  )

  model_trans <- heemod::define_transition(
    state_names = StateN,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    prWell, prDis1, prDis2, prDis3, prDis4, prDis5, prDis6, prDis7, prDis8, prDis9, prDis10, prDec,
    0,0,0,0,0,0,0,0,0,0,0,1
  )

  model_strat<-heemod::define_strategy(
    transition = model_trans,
    Well = heemod::define_state(utility=0,cost=0),
    Disease1 = heemod::define_state(utility=heemod::discount(dalysM[age,1], dR), cost=heemod::discount(costM[age,1], dR)),
    Disease2 = heemod::define_state(utility=heemod::discount(dalysM[age,2], dR), cost=heemod::discount(costM[age,2], dR)),
    Disease3 = heemod::define_state(utility=heemod::discount(dalysM[age,3], dR), cost=heemod::discount(costM[age,3], dR)),
    Disease4 = heemod::define_state(utility=heemod::discount(dalysM[age,4], dR), cost=heemod::discount(costM[age,4], dR)),
    Disease5 = heemod::define_state(utility=heemod::discount(dalysM[age,5], dR), cost=heemod::discount(costM[age,5], dR)),
    Disease6 = heemod::define_state(utility=heemod::discount(dalysM[age,6], dR), cost=heemod::discount(costM[age,6], dR)),
    Disease7 = heemod::define_state(utility=heemod::discount(dalysM[age,7], dR), cost=heemod::discount(costM[age,7], dR)),
    Disease8 = heemod::define_state(utility=heemod::discount(dalysM[age,8], dR), cost=heemod::discount(costM[age,8], dR)),
    Disease9 = heemod::define_state(utility=heemod::discount(dalysM[age,9], dR), cost=heemod::discount(costM[age,9], dR)),
    Disease10 = heemod::define_state(utility=heemod::discount(dalysM[age,10], dR), cost=heemod::discount(costM[age,10], dR)),
    Deceased = heemod::define_state(utility=0, cost=0)
  )

  model <- heemod::run_model(
    standard = model_strat,
    init=c(as.numeric(Initpop),0,0,0,0,0,0,0,0,0,0,0),
    parameters = model_param,
    cycles = cycleT-ageInit,
    cost = cost,
    effect = utility,
    method = "end"
  )

  return(model)
}

#' Find number of deaths under simple scaling vaccine scenario
#' NOTE: Not currently used in app
#'
#' @param noVaccDeaths age-specific death rates per 100,000 persons
#' @param conditions condition names
#' @param vaccAge age of vaccination. Default 0
#' @param vaccEff effectiveness of vaccine, as a percentage
#' @param vaccDur no. of years that vaccine is effective
#'
#' @return numeric
#' @export
findDeaths <- function(noVaccDeaths, conditions, vaccAge =0, vaccEff, vaccDur)
{
  #Currently only for one condition, comparison between no vaccine and
  #one vaccine scenario

  noV_deaths <- as.matrix(noVaccDeaths[match(age_groups$Label, noVaccDeaths$age), "val"])
  rownames(noV_deaths) <- age_groups$Label
  colnames(noV_deaths) <- conditions

  vaccEff <- vaccEff/100

  #Average deaths per year of age group
  tIndex <- rep(1, age_groups$Years[1])
  for(i in 2:nrow(age_groups)) tIndex <- c(tIndex, rep(i, age_groups$Years[i]))
  deaths <-matrix(NA, nrow = sum(age_groups$Years), ncol = 2)
  deaths[,1] <- (noV_deaths/age_groups$Years)[tIndex,]

  #Scale deaths from age of vacc for duration by vaccEff
  deaths[,2] <-deaths[,1]
  deaths[(vaccAge+1):(vaccAge+vaccDur),2] <- (1-vaccEff)*deaths[(vaccAge+1):(vaccAge+vaccDur),2]

  rownames(deaths) <- 0:(nrow(deaths)-1)

  return(deaths)
}

#' Run Markov models for vaccine impact scenario
#' NOTE: Cost, DALYS and discounting not currently used. Change nyears to maxAge?
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
#' @param nyears maximum age of cohort
#' @param yearV year of vaccine introduction
#' @param vaccAge age of vaccination. Default 0
#' @param vaccEff effectiveness of vaccine, as a percentage
#' @param vaccDur no. of years that vaccine is effective
#' @param waning assume linear waning immunity, TRUE or FALSE
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#' @param pYears no. of years to project
#' @param initPop used for modeling if metric is Number
#'
#' @return list
#' @export
runModel<- function(location, condition, inc, propA = 1, rate = 100000,
                    mortality, dRate = 0, nyears = 85, yearV, vaccAge = 0,
                    vaccEff = 100, vaccDur = 10, waning = FALSE, impType,
                    pYears = 10, initPop)
{
  inc <- as.matrix(inc[match(age_groups$Label, inc$age), "val"])
  rownames(inc) <- age_groups$Label
  colnames(inc) <- condition

  numAgeGroups <- nrow(age_groups)
  maxAge <- sum(age_groups$Years)-1
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
  noVacc_counts <- matrix(NA, nrow = nyears, ncol = pYears+1)
  rownames(noVacc_counts) <- 0:maxAge
  colnames(noVacc_counts) <- yearV:(yearV+pYears)

  vacc_counts <- matrix(NA, nrow = nyears, ncol = pYears+1)
  rownames(vacc_counts) <- 0:maxAge
  colnames(vacc_counts) <- yearV:(yearV+pYears)

  noVacc_dalys <- matrix(NA, nrow = nyears, ncol = pYears+1)
  rownames(noVacc_dalys) <- 0:maxAge
  colnames(noVacc_dalys) <- yearV:(yearV+pYears)

  vacc_dalys <- matrix(NA, nrow = nyears, ncol = pYears+1)
  rownames(vacc_dalys) <- 0:maxAge
  colnames(vacc_dalys) <- yearV:(yearV+pYears)

  noVacc_deaths <- matrix(NA, nrow = nyears, ncol = pYears+1)
  rownames(noVacc_deaths) <- 0:maxAge
  colnames(noVacc_deaths) <- yearV:(yearV+pYears)

  vacc_deaths <- matrix(NA, nrow = nyears, ncol = pYears+1)
  rownames(vacc_deaths) <- 0:maxAge
  colnames(vacc_deaths) <- yearV:(yearV+pYears)

  noVacc_pop <- matrix(NA, nrow = nyears, ncol = pYears+1)
  rownames(noVacc_pop) <- 0:maxAge
  colnames(noVacc_pop) <- yearV:(yearV+pYears)

  for(i in 1:(pYears+1))
  {
    prob0 <- transProb(prob, mortality[i,], maxAge, 0)
    colnames(prob0) <- allStates

    noVacc_mod <- heemodModel(prob0, dalys, dRate, cost, initPop[i,2], 0, nyears)

    pop_end_of_year <- noVacc_mod$eval_strategy_list$standard$counts$Disease1 + noVacc_mod$eval_strategy_list$standard$counts$Well #population

    noVacc_counts[,i] <- noVacc_mod$eval_strategy_list$standard$counts$Disease1 #condition counts
    noVacc_dalys[,i] <- noVacc_mod$eval_strategy_list$standard$values$utility #dalys
    noVacc_pop[,i] <- c(initPop[i,2], pop_end_of_year[-length(pop_end_of_year)])

  }#end for pYears

  #Now scale values to get required vaccination scenario results
  vacc_counts <- noVacc_counts
  immProp <- immunity(waning = waning, duration = vaccDur, efficacy = vaccEff)
  totalDur <- ifelse(vaccAge+length(immProp) < maxAge+1, length(immProp), maxAge+1-vaccAge)
  vacc_counts[(vaccAge+1):(vaccAge+totalDur),] <- (1-immProp[1:totalDur])*vacc_counts[(vaccAge+1):(vaccAge+totalDur),]

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

#' Present results in age groups
#'
#' @param age_data cases, DALYs, deaths results by yearly age
#'
#' @return data.frame
#' @export
groupResults <- function(age_data)
{
  years <- colnames(age_data)
  group_data <- matrix(NA, nrow = nrow(age_groups), ncol = length(years))
  colnames(group_data) <- paste("Year:", years)
  rownames(group_data) <- age_groups$Label

  #Change back to age_groups
  j <- 1
  for(i in 1:nrow(age_groups))
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
#' @param vDur vaccination durability
#' @param vYear year of vaccine introduction
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#' @param pYears no. of years to project
#'
#' @return ggplot
#' @export
makePlot <- function(noVacc_data, vacc_data, ylabel, maxAge = 84, vAge, vDur,
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

#' #' Find incidence rates with 95% uncertainty intervals
#' #' NOTE: Not currently used in app
#' #'
#' #' @param careEp  raw care episodes-at-age
#' #' @param pop population-at-age that care episodes relate to
#' #' @param propAttr prop of episodes-at-age that are attributable to GAS for each disease
#' #' @param rate eg. per 100 persons, per 100k
#' #' @param years how many years does data cover
#' #' @param grps data frame used to combine and present results in user provided groups
#' #'
#' #' @return matrix
#' #' @export
#' incRates <- function(careEp, pop, propAttr, rate=100000, years=1, grps=1)
#' {
#'   age <- careEp[,1]
#'   careEp <- careEp[,-1]
#'   careEp <- careEp/years #find yearly average
#'   rownames(careEp) <- age
#'   colnames(pop) <- c("Age", "n")
#'
#'   if(is.data.frame(grps))
#'   { #if grps is a data frame with disease groups, aggregate careEp data
#'
#'     newCareEp <- matrix(NA, nrow=length(age), ncol=ncol(grps))
#'     rownames(newCareEp) <- age
#'     colnames(newCareEp) <- colnames(grps)
#'
#'     for(i in 1:ncol(grps))
#'     {
#'       diseases <- grps[which(grps[,i]!=""),i] #diseases in group
#'
#'
#'       epXprop <- propAttr[,diseases]*careEp[,diseases] #mult. each ep. with relevant prop.
#'
#'       if(length(diseases)>1)
#'       {
#'         newCareEp[,colnames(grps)[i]] <- apply(epXprop, 1, sum)
#'       }else{
#'         newCareEp[,colnames(grps)[i]] <- epXprop
#'       }
#'
#'     }
#'     newPropAttr <- matrix(1, nrow=length(age), ncol=ncol(grps))
#'     colnames(newPropAttr) <- colnames(grps)
#'     propAttr <- newPropAttr
#'     careEp <- newCareEp
#'   }
#'
#'   Epi_categories <- colnames(careEp)
#'
#'   Epi_headings <- c(Epi_categories[1], paste(Epi_categories[1], "Lower"), paste(Epi_categories[1], "Upper"))
#'   if(length(Epi_categories)>1)
#'   {
#'     for(i in 2:length(Epi_categories))
#'       Epi_headings <- c(Epi_headings, Epi_categories[i], paste(Epi_categories[i], "Lower"), paste(Epi_categories[i], "Upper"))
#'   }
#'
#'   Epi_incidents <- matrix(NA, nrow=length(age), ncol=length(Epi_headings))
#'   Epi_incidents_Total <- matrix(NA, nrow=1, ncol=length(Epi_headings))
#'   rownames(Epi_incidents) <- age
#'   colnames(Epi_incidents) <- Epi_headings
#'   colnames(Epi_incidents_Total) <- Epi_headings
#'
#'   #incident rate=propAttrGASx(NoIncidentsPerYear/SizeOfIncidentPop)x perNoPeople (no. of episodes per 100 people)
#'   for(i in Epi_categories)
#'   {
#'     Epi_incidents[,i] <- propAttr[,i]*(careEp[,i]/pop[,"n"])*rate
#'     Epi_incidents_Total[,i] <- sum(Epi_incidents[,i])
#'
#'     for(j in 1:length(age))
#'     { #uncertainty interval
#'       rSampleInc <- stats::rgamma(1000, shape =careEp[j,i], rate = pop[j,"n"])
#'       Epi_incidents[j, paste(i, "Lower")] <- propAttr[j, i]*stats::quantile(rSampleInc, 0.025)*rate
#'       Epi_incidents[j, paste(i, "Upper")] <- propAttr[j, i]*stats::quantile(rSampleInc, 0.975)*rate
#'     }
#'     rSampleInc <- stats::rgamma(1000, shape =sum(careEp[,i]), rate = sum(pop[,"n"]))
#'   }
#'
#'   return(Epi_incidents)
#' }

#' #' Find DALYs with 95% uncertainty intervals
#' #' NOTE: Not currently used in app
#' #'
#' #' @param incR incidence rates
#' #' @param rate used for incR eg. per 100 persons, per 100k
#' #' @param pop population-at-age, same age groups as incR
#' #' @param life life-expectancy-at-age, same age groups as incR
#' #' @param dw_A acute disability weights with uncertainty
#' #' @param dw_C chronic disability weights with uncertainty
#' #' @param prog probability of progression from acute to chronic
#' #' @param dRate discount rate as a percent
#' #' @param duration duration of disease
#' #' @param deaths death rates
#' #' @param deathRate rate used for deaths
#' #'
#' #' @return matrix
#' #' @export
#' dalys<-function(incR, rate, pop, life, dw_A, dw_C, prog, dRate, duration, deaths, deathRate)
#' {
#'   age <- incR[,1]
#'   incR <- incR[,-1]/rate
#'   rownames(incR) <- age
#'   colnames(pop) <- c("Age", "n")
#'   deaths <- deaths[,-1]/deathRate
#'   dRate <- dRate/100
#'   rownames(dw_A) <- dw_A[,1]
#'   dw_A <- dw_A[,-1]
#'   colnames(dw_A) <- c("w", "min", "max")
#'   rownames(dw_C) <- dw_C[,1]
#'   dw_C <- dw_C[,-1]
#'   colnames(dw_C) <- c("w", "min", "max")
#'
#'   daly_categories <- colnames(incR)
#'
#'   daly_headings <- c(daly_categories[1], paste(daly_categories[1], "Lower"), paste(daly_categories[1], "Upper"))
#'   if(length(daly_categories)>1)
#'   {
#'     for(i in 2:length(daly_categories))
#'       daly_headings <- c(daly_headings, daly_categories[i], paste(daly_categories[i], "Lower"), paste(daly_categories[i], "Upper"))
#'   }
#'
#'   dalyT <- matrix(NA, nrow=length(age), ncol=length(daly_headings))
#'   daly_Total <- matrix(NA, nrow=1, ncol=length(daly_headings))
#'   rownames(dalyT) <- age
#'   colnames(dalyT) <- daly_headings
#'   colnames(daly_Total) <- daly_headings
#'
#'   #if discounting, change duration values to present value
#'   #duration should be disease dependent, for those conditions
#'   #that are chronic or can progress to a chronic condition
#'   if(dRate>0)
#'   {
#'     for(j in rownames((dw_C)))
#'     {
#'       for(i in 1:length(age))
#'         duration[i,j] <- -FinCal::pv(dRate,duration[i, j],0,1,1)
#'     }
#'   }
#'
#'   #Assume first that all have DALY=YLD+YLL
#'   for(i in daly_categories)
#'   {
#'     #YLD_acute = no.episodes x disability weight
#'     #YLD_chronic = prop. that progress to chronic x no. episodes (rate/person x no. people)
#'     #             x disability weight x duration (either with or without discounting)
#'     YLD_acute <- 0
#'     YLD_chronic <- 0
#'
#'     if(i %in% rownames(dw_A))
#'       YLD_acute <- incR[,i]*pop[,"n"]*dw_A[i,"w"]
#'     if(i %in% rownames(dw_C))
#'       YLD_chronic <- prog[,i]*incR[,i]*pop[,"n"]*dw_C[i,"w"]*duration[, i]
#'
#'     YLD <- YLD_acute+YLD_chronic
#'
#'     #uncertainty
#'     for(j in 1:length(age))
#'     {
#'       #Note: gamma distribution parameters are an approximation if the incidence
#'       #rates were based on a care population smaller than country population
#'       dalyT[j, paste(i, "Lower")] <- 0
#'       dalyT[j, paste(i, "Upper")] <- 0
#'
#'       rSampleInc <- stats::rgamma(1000, shape =incR[j,i]*pop[j, "n"], rate = pop[j, "n"])
#'
#'       if(i %in% rownames(dw_A)){
#'         rSampleDW_A  <- triangle::rtriangle(1000, dw_A[i,"min"], dw_A[i,"max"], dw_A[i,"w"])
#'         Lower_A <- stats::quantile(rSampleInc*rSampleDW_A, 0.025)*pop[j, "n"]
#'         Upper_A <- stats::quantile(rSampleInc*rSampleDW_A, 0.975)*pop[j, "n"]
#'         dalyT[j, paste(i, "Lower")] <- dalyT[j, paste(i, "Lower")]+as.numeric(Lower_A)
#'         dalyT[j, paste(i, "Upper")] <- dalyT[j, paste(i, "Upper")]+as.numeric(Upper_A)
#'       }
#'       if(i %in% rownames(dw_C)){
#'         rSampleDW_C  <- triangle::rtriangle(1000, dw_C[i,"min"], dw_C[i,"max"], dw_C[i,"w"])
#'         Lower_C <- prog[i]*stats::quantile(rSampleInc*rSampleDW_C, 0.025)*pop[j, "n"]*duration[j, i]
#'         Upper_C <- prog[i]*stats::quantile(rSampleInc*rSampleDW_C, 0.975)*pop[j, "n"]*duration[j, i]
#'         dalyT[j, paste(i, "Lower")] <- dalyT[j, paste(i, "Lower")]+as.numeric(Lower_C)
#'         dalyT[j, paste(i, "Upper")] <- dalyT[j, paste(i, "Upper")]+as.numeric(Upper_C)
#'       }
#'     }
#'
#'     YLL <- deaths[,i]*life[,"le"]
#'
#'     dalyT[,i] <- YLD+YLL
#'     dalyT[, paste(i, "Lower")] <- dalyT[, paste(i, "Lower")]+YLL
#'     dalyT[, paste(i, "Upper")] <- dalyT[, paste(i, "Upper")]+YLL
#'
#'   }
#'
#'   return(dalyT)
#' }

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


  #lifeEx nrow = maxAge +1, ncol = years

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
      end_yld_index <- ifelse((a+trunc(lifeEx[y,a])) < (maxAge+1), a+trunc(lifeEx[y,a]), maxAge+1)
      len_lifeEx <- length(a:end_yld_index)
      noVacc_yld[a:end_yld_index,y] <- noVacc_yld[a:end_yld_index,y] +
                        rep((noVacc_counts[a,y] - sum(death_vec_NoV))*dw, len_lifeEx) #those that don't die due to RHD
      vacc_yld[a:end_yld_index,y] <- vacc_yld[a:end_yld_index,y] +
                        rep((vacc_counts[a,y] - sum(death_vec_V))*dw, len_lifeEx) #those that don't die due to RHD

      for(i in 1:10)
      { #those that die in year i-1, assume whole year of disability
        #in the year they die in
        end_yld_index <- ifelse((a+i-1) < (maxAge+1), a+i-1, maxAge+1)
        len_vec <- length(a:end_yld_index)
        noVacc_yld[a:end_yld_index,y] <- noVacc_yld[a:end_yld_index,y] +
                                              rep(death_vec_NoV[i]*dw, len_vec)
        vacc_yld[a:end_yld_index,y] <- vacc_yld[a:end_yld_index,y] +
                                              rep(death_vec_V[i]*dw, len_vec)
      }

    }
  }

  noVacc_dalys <- noVacc_yll + noVacc_yld
  vacc_dalys <- vacc_yll + vacc_yld

  return(list(noVacc_dalys, vacc_dalys, noVacc_deaths, vacc_deaths))
}

