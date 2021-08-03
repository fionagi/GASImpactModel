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
    countries <- data.region$Country
  } else{
    countries <- data.region$Country[which(data.region$Region == region)]
  }
  return(countries)
}

#' Retrieve location and condition specific incidence and deaths data
#'
#' @param location country that matches UN data
#' @param condition either RHD or cellulitis
#'
#' @return list(data.frame, data.frame)
#' @export
getRateData <- function(location, condition)
{
  label <- lookup_condition[lookup_condition$Condition==condition ,]$Label
  condData <- paste("data.", label, "2019", sep='')

  data <- get(condData)
  inc <- data[data$location==location & data$measure=="Incidence",]
  deaths <- data[data$location==location & data$measure=="Deaths",]

  return(list(inc, deaths))
}

#' Retrieve location population data
#'
#' @param location country that matches UN data
#'
#' @return data.frame
#' @export
getPopData <- function(location)
{
  pop <- data.popbyage2020[data.popbyage2020$Country==location, ]

  return(pop)
}

#' Retrieve probability of all-cause mortality
#'
#' @param location country that matches UN data
#'
#' @return data.frame
#' @export
getMorData <- function(location)
{
  data <- stringr::str_replace(paste("data.life.", tolower(location), "2019", sep=''), " ", "." )
  mor <- get(data)
  mor <- mor[-nrow(mor),] #remove 85+ row to be compatible with incidence data

  return(mor)
}


#' Find incident rates with 95% uncertainty intervals
#'
#' @param careEp  raw care episodes-at-age
#' @param pop population-at-age that care episodes relate to
#' @param propAttr prop of episodes-at-age that are attributable to GAS for each disease
#' @param rate eg. per 100 persons, per 100k
#' @param years how many years does data cover
#' @param grps data frame used to combine and present results in user provided groups
#'
#' @return matrix
#' @export
incRates <- function(careEp, pop, propAttr, rate=100000, years=1, grps=1)
{
  age <- careEp[,1]
  careEp <- careEp[,-1]
  careEp <- careEp/years #find yearly average
  rownames(careEp) <- age
  colnames(pop) <- c("Age", "n")

  if(is.data.frame(grps))
  { #if grps is a data frame with disease groups, aggregate careEp data

    newCareEp <- matrix(NA, nrow=length(age), ncol=ncol(grps))
    rownames(newCareEp) <- age
    colnames(newCareEp) <- colnames(grps)

    for(i in 1:ncol(grps))
    {
      diseases <- grps[which(grps[,i]!=""),i] #diseases in group


      epXprop <- propAttr[,diseases]*careEp[,diseases] #mult. each ep. with relevant prop.

      if(length(diseases)>1)
      {
        newCareEp[,colnames(grps)[i]] <- apply(epXprop, 1, sum)
      }else{
        newCareEp[,colnames(grps)[i]] <- epXprop
      }

    }
    newPropAttr <- matrix(1, nrow=length(age), ncol=ncol(grps))
    colnames(newPropAttr) <- colnames(grps)
    propAttr <- newPropAttr
    careEp <- newCareEp
  }

  Epi_categories <- colnames(careEp)

  Epi_headings <- c(Epi_categories[1], paste(Epi_categories[1], "Lower"), paste(Epi_categories[1], "Upper"))
  if(length(Epi_categories)>1)
  {
    for(i in 2:length(Epi_categories))
      Epi_headings <- c(Epi_headings, Epi_categories[i], paste(Epi_categories[i], "Lower"), paste(Epi_categories[i], "Upper"))
  }

  Epi_incidents <- matrix(NA, nrow=length(age), ncol=length(Epi_headings))
  Epi_incidents_Total <- matrix(NA, nrow=1, ncol=length(Epi_headings))
  rownames(Epi_incidents) <- age
  colnames(Epi_incidents) <- Epi_headings
  colnames(Epi_incidents_Total) <- Epi_headings

  #incident rate=propAttrGASx(NoIncidentsPerYear/SizeOfIncidentPop)x perNoPeople (no. of episodes per 100 people)
  for(i in Epi_categories)
  {
    Epi_incidents[,i] <- propAttr[,i]*(careEp[,i]/pop[,"n"])*rate
    Epi_incidents_Total[,i] <- sum(Epi_incidents[,i])

    for(j in 1:length(age))
    { #uncertainty interval
      rSampleInc <- stats::rgamma(1000, shape =careEp[j,i], rate = pop[j,"n"])
      Epi_incidents[j, paste(i, "Lower")] <- propAttr[j, i]*stats::quantile(rSampleInc, 0.025)*rate
      Epi_incidents[j, paste(i, "Upper")] <- propAttr[j, i]*stats::quantile(rSampleInc, 0.975)*rate
    }
    rSampleInc <- stats::rgamma(1000, shape =sum(careEp[,i]), rate = sum(pop[,"n"]))
  }

  return(Epi_incidents)
}

#' Find DALYs with 95% uncertainty intervals
#'
#' @param incR incident rates
#' @param rate used for incR eg. per 100 persons, per 100k
#' @param pop population-at-age, same age groups as incR
#' @param life life-expectancy-at-age, same age groups as incR
#' @param dw_A acute disability weights with uncertainty
#' @param dw_C chronic disability weights with uncertainty
#' @param prog probability of progression from acute to chronic
#' @param dRate discount rate as a percent
#' @param duration duration of disease
#' @param deaths death rates
#' @param deathRate rate used for deaths
#'
#' @return matrix
#' @export
dalys<-function(incR, rate, pop, life, dw_A, dw_C, prog, dRate, duration, deaths, deathRate)
{
  age <- incR[,1]
  incR <- incR[,-1]/rate
  rownames(incR) <- age
  colnames(pop) <- c("Age", "n")
  deaths <- deaths[,-1]/deathRate
  dRate <- dRate/100
  rownames(dw_A) <- dw_A[,1]
  dw_A <- dw_A[,-1]
  colnames(dw_A) <- c("w", "min", "max")
  rownames(dw_C) <- dw_C[,1]
  dw_C <- dw_C[,-1]
  colnames(dw_C) <- c("w", "min", "max")

  daly_categories <- colnames(incR)

  daly_headings <- c(daly_categories[1], paste(daly_categories[1], "Lower"), paste(daly_categories[1], "Upper"))
  if(length(daly_categories)>1)
  {
    for(i in 2:length(daly_categories))
      daly_headings <- c(daly_headings, daly_categories[i], paste(daly_categories[i], "Lower"), paste(daly_categories[i], "Upper"))
  }

  dalyT <- matrix(NA, nrow=length(age), ncol=length(daly_headings))
  daly_Total <- matrix(NA, nrow=1, ncol=length(daly_headings))
  rownames(dalyT) <- age
  colnames(dalyT) <- daly_headings
  colnames(daly_Total) <- daly_headings

  #if discounting, change duration values to present value
  #duration should be disease dependent, for those conditions
  #that are chronic or can progress to a chronic condition
  if(dRate>0)
  {
    for(j in rownames((dw_C)))
    {
      for(i in 1:length(age))
        duration[i,j] <- -FinCal::pv(dRate,duration[i, j],0,1,1)
    }
  }

  #Assume first that all have DALY=YLD+YLL
  for(i in daly_categories)
  {
    #YLD_acute = no.episodes x disability weight
    #YLD_chronic = prop. that progress to chronic x no. episodes (rate/person x no. people)
    #             x disability weight x duration (either with or without discounting)
    YLD_acute <- 0
    YLD_chronic <- 0

    if(i %in% rownames(dw_A))
      YLD_acute <- incR[,i]*pop[,"n"]*dw_A[i,"w"]
    if(i %in% rownames(dw_C))
      YLD_chronic <- prog[,i]*incR[,i]*pop[,"n"]*dw_C[i,"w"]*duration[, i]

    YLD <- YLD_acute+YLD_chronic

    #uncertainty
    for(j in 1:length(age))
    {
      #Note: gamma distribution parameters are an approximation if the incident
      #rates were based on a care population smaller than country population
      dalyT[j, paste(i, "Lower")] <- 0
      dalyT[j, paste(i, "Upper")] <- 0

      rSampleInc <- stats::rgamma(1000, shape =incR[j,i]*pop[j, "n"], rate = pop[j, "n"])

      if(i %in% rownames(dw_A)){
        rSampleDW_A  <- triangle::rtriangle(1000, dw_A[i,"min"], dw_A[i,"max"], dw_A[i,"w"])
        Lower_A <- stats::quantile(rSampleInc*rSampleDW_A, 0.025)*pop[j, "n"]
        Upper_A <- stats::quantile(rSampleInc*rSampleDW_A, 0.975)*pop[j, "n"]
        dalyT[j, paste(i, "Lower")] <- dalyT[j, paste(i, "Lower")]+as.numeric(Lower_A)
        dalyT[j, paste(i, "Upper")] <- dalyT[j, paste(i, "Upper")]+as.numeric(Upper_A)
      }
      if(i %in% rownames(dw_C)){
        rSampleDW_C  <- triangle::rtriangle(1000, dw_C[i,"min"], dw_C[i,"max"], dw_C[i,"w"])
        Lower_C <- prog[i]*stats::quantile(rSampleInc*rSampleDW_C, 0.025)*pop[j, "n"]*duration[j, i]
        Upper_C <- prog[i]*stats::quantile(rSampleInc*rSampleDW_C, 0.975)*pop[j, "n"]*duration[j, i]
        dalyT[j, paste(i, "Lower")] <- dalyT[j, paste(i, "Lower")]+as.numeric(Lower_C)
        dalyT[j, paste(i, "Upper")] <- dalyT[j, paste(i, "Upper")]+as.numeric(Upper_C)
      }
    }

    YLL <- deaths[,i]*life[,"le"]

    dalyT[,i] <- YLD+YLL
    dalyT[, paste(i, "Lower")] <- dalyT[, paste(i, "Lower")]+YLL
    dalyT[, paste(i, "Upper")] <- dalyT[, paste(i, "Upper")]+YLL

  }

  return(dalyT)
}

#' Change incidence probabilities to being unconditional
#'
#' @param Pr vector/row of combined incid prob and all-cause mortality
#'
#' @return vector
#' @export
probUncondition <- function(Pr)
{
  pos_m <- length(Pr)
  m <- Pr[pos_m]
  incProb <- Pr[1:(pos_m-1)]
  newProb <- c(incProb*(1-m), m)

  return(newProb)
}

#' Find transition probabilities for Markov model
#'
#' @param prM probability of disease without vaccine
#' @param lifeTable all-cause mortality
#' @param maxAge number of years over which transition probs. calculated
#' @param V_Eff vaccine effectiveness, reduce prob. by this proportion
#' @param V_Age age when vaccine is administered
#' @param V_Dur length of time that vaccine is effective for
#'
#' @return matrix
#' @export
transProb <- function(prM, lifeTable, maxAge, V_Eff, V_Age, V_Dur)
{
  trProb <- prM

  if(V_Eff != 0) #if vaccinating
  {
    vTime <- (1+V_Age):(V_Age+V_Dur) #from time of vacc to end of duration of effectiveness
    trProb[vTime,] <- (1-V_Eff)*trProb[vTime,] #reduce prob. of incidents due to vaccine
  }

  trProb <- cbind(trProb, as.numeric(lifeTable[1:(maxAge+1)]))
  colnames(trProb)[ncol(trProb)] <- "Deceased"

  trProb <- t(apply(trProb, 1, probUncondition))

  PrWell <- apply(trProb, 1, function(x) 1-sum(x))
  trProb <- cbind(as.matrix(PrWell), trProb)
  colnames(trProb)[1] <- "Well"

  return(trProb)
}

#' Find value of vaccine
#'
#' @param c0 cost of disease with no vaccine
#' @param cV cost of disease with vaccine
#' @param e0 DALYs lost without vaccine
#' @param eV DALYs lost with vaccine
#' @param popS population size
#' @param ICER incremental cost-effectiveness ratio (ICER)
#'
#' @return numeric
#' @export
findVacValue <- function(c0, cV, e0, eV, popS, ICER)
{
  VV <- (c0-cV + ICER*(e0-eV))/popS
  return(VV)
}

#' Find incremental cost-effectiveness ratio (ICER)
#'
#' @param c0 cost of disease with no vaccine
#' @param cV cost of disease with vaccine
#' @param e0 DALYs lost without vaccine
#' @param eV DALYs lost with vaccine
#' @param popS population size
#' @param VV value of vaccine
#'
#' @return numeric
#' @export
findICER <- function(c0, cV, e0, eV, popS, VV)
{
  ICER <-((VV*popS)+cV-c0)/(e0-eV)
  return(ICER)
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
    dummyMat<-matrix(0, nrow=nrow(probM), ncol=noDummyStates)
    probM<-cbind(probM[,-which(colnames(probM)=="Deceased")], dummyMat, probM[,"Deceased"])
    dalysM<-cbind(dalysM, dummyMat)
    costM<-cbind(costM, dummyMat)
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
    init=c(Initpop,0,0,0,0,0,0,0,0,0,0,0),
    parameters = model_param,
    cycles = cycleT-ageInit,
    cost = cost,
    effect = utility,
    method = "end"
  )

  return(model)
}


#' Run Markov models for vaccine impact scenario
#' @description Run Markov models for vaccine impact scenario and produce table
#' of comparison metrics. This function calls transProb and heemodModel
#'
#' @param age age group names and year range
#' @param inc incident rates for each condition
#' @param rate rate used for inc
#' @param costs costs per person
#' @param dalys DALYs for each condition
#' @param dalyRate rate used for DALYs
#' @param mortality all-cause mortality rate at age
#' @param initPop initial population size
#' @param dRate discount rate as a percentage
#' @param nyears no. of years to project
#' @param vaccAge age of vaccination
#' @param vaccEff effectiveness of vaccine, as a percentage
#' @param vaccDur no. of years that vaccine is effective
#' @param valueVac value of the vaccine
#'
#' @return table of comparison values
#' @export
markovModel<- function(age, inc, rate, costs, dalys, dalyRate, mortality, initPop,
                       dRate, nyears, vaccAge, vaccEff, vaccDur, valueVac)
{
  rownames(inc)<-age[,1]
  inc<-as.matrix(inc[,-1])
  rownames(costs)<-age[,1]
  costs<-as.matrix(costs[,-1])
  rownames(dalys)<-age[,1]
  dalys<-as.matrix(dalys[,-1])
  yearsPerAgeGroup <- age[,2]
  numAgeGroups <- nrow(age)
  maxAge <- sum(yearsPerAgeGroup)-1
  dRate <- dRate/100
  vaccEff <- vaccEff/100

  #Incidence matrix as a probability
  IncidenceProb<-inc/rate

  tIndex<-rep(1, yearsPerAgeGroup[1])
  for(i in 2:numAgeGroups) tIndex<- c(tIndex, rep(i, yearsPerAgeGroup[i]))

  #create matrix with all costs (per incident) for all ages
  CostPerIncid<-costs/IncidenceProb
  CostPerIncid[which(is.na(CostPerIncid))]<-0
  cost<-CostPerIncid[tIndex,]
  rownames(cost)<-0:maxAge

  #create matrix with all DALYs for all ages
  DALYSPerIncid<-(dalys/dalyRate)/IncidenceProb #divide by 100k to get per person over population
  DALYSPerIncid[which(is.na(DALYSPerIncid))]<-0
  dalys<-DALYSPerIncid[tIndex,] #DALYS per incident
  rownames(dalys)<-0:maxAge

  ##############################################################################
  #TRANSITION PROB MATRICES FOR EACH SCENARIO - At this stage, just looking
  #at one scenario and comparing to no vaccine
  #Create matrix with all transition probabilities for all ages

  #base transition prob, no vaccination
  prob<-IncidenceProb[tIndex,]
  rownames(prob)<-0:maxAge
  diseaseStates<-colnames(prob)
  allStates<-c("Well", diseaseStates, "Deceased")

  #no vaccine
  #transProb defines the Well and Deceased states, removes probability conditioning
  #Col names of resulting matrix will have all state names
  prob0<-transProb(prob, mortality[,2], maxAge, 0)
  #vaccine
  probVacc<-transProb(prob, mortality[,2], maxAge, vaccEff, vaccAge, vaccDur)

  ##################################################################################
  #RUN ALL SCENARIO MODELS

  #no vaccine
  noVacc_mod<-heemodModel(prob0, dalys, dRate, cost, initPop, 0, nyears)
  #vaccine
  vacc_mod<-heemodModel(probVacc, dalys, dRate, cost, initPop, vaccAge, nyears)

  #####################################################################################
  # #MODEL OUTPUTS
  #Summary stats

  #get all costs/dalys columns each age/year
  outputCols<-grep("dots", colnames(data.frame(noVacc_mod$eval_strategy_list$standard$states)))
  costCols<-grep("cost", colnames(data.frame(noVacc_mod$eval_strategy_list$standard$states)))
  dalyCols<-grep("utility", colnames(data.frame(noVacc_mod$eval_strategy_list$standard$states)))
  numModelStates<-length(intersect(costCols, outputCols))
  nonZeroCols<-c(1:(length(diseaseStates)+1), numModelStates)

  #incident counts per age
  noVacc_counts<-noVacc_mod$eval_strategy_list$standard$counts[,nonZeroCols]
  colnames(noVacc_counts)<-allStates
  vacc_counts<-vacc_mod$eval_strategy_list$standard$counts[,nonZeroCols]
  colnames(vacc_counts)<-allStates

  #costs per age (will be different to what user has given if discounting,
  #               but same for noVacc to vacc)
  costs_per_age<-data.frame(noVacc_mod$eval_strategy_list$standard$states)[,intersect(costCols, outputCols)][,nonZeroCols]

  #dalys per age (as above)
  dalys_per_age<-data.frame(noVacc_mod$eval_strategy_list$standard$states)[,intersect(dalyCols, outputCols)][,nonZeroCols]

  #########################################################################################
  #TABLE results
  #Incidents (noVacc and vacc), DALYs (noVacc and Vacc), Cost (noVacc and Vacc) by age
  #                 with discounting if user chosen
  #
  #Incidence nos.
  noVacc<-apply(data.frame(noVacc_counts[,diseaseStates]), 2, sum)
  vacc<-apply(data.frame(vacc_counts[,diseaseStates]), 2, sum)

  row1<-c(noVacc, sum(noVacc))
  row2<-c(vacc, sum(vacc))
  row3<-100*c((noVacc-vacc)/noVacc, (sum(noVacc)-sum(vacc))/sum(noVacc)) #% diff

  #Health burden
  noVacc_dalys<-apply((noVacc_counts*dalys_per_age)[,diseaseStates], 2, sum)
  vacc_dalys<-apply((vacc_counts*dalys_per_age)[,diseaseStates], 2, sum)

  row4<-c(noVacc_dalys,sum(noVacc_dalys))
  row5<-c(vacc_dalys,sum(vacc_dalys))
  row6<-100*c((noVacc_dalys-vacc_dalys)/noVacc_dalys, (sum(noVacc_dalys)-sum(vacc_dalys))/sum(noVacc_dalys))

  #Economic burden
  noVacc_costs<-apply((noVacc_counts*costs_per_age)[,diseaseStates], 2, sum)
  vacc_costs<-apply((vacc_counts*costs_per_age)[,diseaseStates], 2, sum)

  row7<-c(noVacc_costs, sum(noVacc_costs))
  row8<-c(vacc_costs, sum(vacc_costs))
  row9<-100*c((noVacc_costs-vacc_costs)/noVacc_costs, (sum(noVacc_costs)-sum(vacc_costs))/sum(noVacc_costs))

  #ICER
  ICER<-findICER(noVacc_costs, vacc_costs, noVacc_dalys, vacc_dalys, initPop, valueVac)
  totalICER<-findICER(sum(noVacc_costs), sum(vacc_costs), sum(noVacc_dalys), sum(vacc_dalys), initPop, valueVac)

  row10<-c(ICER, totalICER)

  impactT<-rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10)
  colnames(impactT)<-c(diseaseStates,"Total")
  rownames(impactT)<-c("Incidents (No Vacc)", "Incidents (Vacc)", "Incidents %",
                       "DALYs (No Vacc)", "DALYs (Vacc)", "DALYs %",
                       "Cost (No Vacc)", "Cost (Vacc)", "Cost %",
                       "ICER")
  return(impactT)

}

#' Run Markov models for vaccine impact scenario
#' @description Run Markov models for vaccine impact scenario and produce table
#' of comparison metrics. This function calls transProb and heemodModel
#'
#' @param conditions condition names
#' @param inc incident rates for each condition
#' @param rate rate used for inc. Default 100,000
#' @param costs costs per person for each condition and age group.
#'              nAgeGroups x (nConditions + 1) matrix, col1 = ageGroup,
#'              If set to 1, all age groups and conditions set to cost 1/person
#' @param dalys DALYs for each condition and age group.
#'              nAgeGroups x (nConditions + 1) matrix, col1 = ageGroup,
#'              If set to 1, all age groups and conditions set to DALYs 1
#' @param dalyRate rate used for DALYs. Default 100,000
#' @param mortality all-cause mortality rate at age
#' @param initPop initial population size. Default 100,000
#' @param dRate discount rate as a percentage. Default 0%
#' @param nyears no. of years to project
#' @param vaccAge age of vaccination. Default 0
#' @param vaccEff effectiveness of vaccine, as a percentage
#' @param vaccDur no. of years that vaccine is effective
#'
#' @return list
#' @export
runModel<- function(conditions, inc, rate = 100000, costs = 1, dalys = 1,
                    dalyRate =100000, mortality, initPop = 100000, dRate = 0,
                    nyears = -1, vaccAge = 0, vaccEff = 100, vaccDur = 10)
{
  inc <- as.matrix(inc[match(age_groups$Label, inc$age), "val"])
  rownames(inc) <- age_groups$Label
  colnames(inc) <- conditions

  if(costs == 1)
  {
    costs <- matrix(rep(1, nrow(age_groups)*length(conditions)),
                    nrow = nrow(age_groups), ncol = length(conditions) )
  }else{
    costs <- as.matrix(costs[,-1])
  }
  rownames(costs) <- age_groups$Label
  colnames(costs) <- conditions

  if(dalys == 1)
  {
    dalys <-matrix(rep(1, nrow(age_groups)*length(conditions)),
                   nrow = nrow(age_groups), ncol = length(conditions) )
  }else{
    dalys <- as.matrix(dalys[,-1])
  }
  rownames(dalys) <- age_groups$Label
  colnames(dalys) <- conditions

  numAgeGroups <- nrow(age_groups)
  maxAge <- sum(age_groups$Years)-1
  dRate <- dRate/100
  vaccEff <- vaccEff/100
  if(nyears == -1) nyears <- maxAge - vaccAge + 1

  #Incidence matrix as a probability
  IncidenceProb <-inc/rate

  tIndex <- rep(1, age_groups$Years[1])
  for(i in 2:numAgeGroups) tIndex <- c(tIndex, rep(i, age_groups$Years[i]))

  #create matrix with all costs (per incident) for all ages
  CostPerIncid <- costs/IncidenceProb
  CostPerIncid[which(is.na(CostPerIncid)|is.infinite(CostPerIncid))] <- 0
  cost <- as.matrix(CostPerIncid[tIndex,])
  rownames(cost) <- 0:maxAge

  #create matrix with all DALYs for all ages
  DALYSPerIncid <- (dalys/dalyRate)/IncidenceProb #divide by 100k to get per person over population
  DALYSPerIncid[which(is.na(DALYSPerIncid)|is.infinite(DALYSPerIncid))] <- 0
  dalys <- as.matrix(DALYSPerIncid[tIndex,]) #DALYS per incident
  rownames(dalys) <- 0:maxAge

  #create matrix with mortality for all ages
  #check if mortality has same no. of rows as maxAge+1, else assume
  #grouped in same way as other data
  if(nrow(mortality)!= maxAge) mortality <- mortality[tIndex,]
  mortality[,1] <- 0:maxAge
  mortality <- mortality[, c("Age Group", "Both sexes")]

  #Create matrix with all transition probabilities for all ages
  #base transition prob, no vaccination
  prob <- as.matrix(IncidenceProb[tIndex,])
  rownames(prob) <- 0:maxAge
  allStates <- c("Well", conditions, "Deceased")

  #no vaccine
  #transProb defines the Well and Deceased states, removes probability conditioning
  #Col names of resulting matrix will have all state names
  prob0 <- transProb(prob, mortality[,2], maxAge, 0)
  #vaccine
  probVacc <- transProb(prob, mortality[,2], maxAge, vaccEff, vaccAge, vaccDur)

  #run models
  #no vaccine
  noVacc_mod <- heemodModel(prob0, dalys, dRate, cost, initPop, 0, nyears)
  #vaccine
  vacc_mod <- heemodModel(probVacc, dalys, dRate, cost, initPop, vaccAge, nyears)

  return(list(noVacc_mod, vacc_mod))
}

#' Create table of comparison statistics
#'
#' @param noVacc_mod heemod model for no vaccine scenario
#' @param vacc_mod heemod model for vaccine scenario
#' @param conditions conditions modeled
#' @param initPop initial population size. Default 100,000
#' @param valueVac value of vaccine
#'
#' @return data.frame
#' @export
makeTable <- function(noVacc_mod, vacc_mod, conditions, initPop = 100000,
                      valueVac = -1)
{
  allStates <- c("Well", conditions, "Deceased")

  #get all costs/dalys columns each age/year
  outputCols <- grep("dots", colnames(data.frame(noVacc_mod$eval_strategy_list$standard$states)))
  costCols <- grep("cost", colnames(data.frame(noVacc_mod$eval_strategy_list$standard$states)))
  dalyCols <- grep("utility", colnames(data.frame(noVacc_mod$eval_strategy_list$standard$states)))
  numModelStates <- length(intersect(costCols, outputCols))
  nonZeroCols <- c(1:(length(conditions)+1), numModelStates)

  #incident counts per age
  noVacc_counts <- noVacc_mod$eval_strategy_list$standard$counts[,nonZeroCols]
  colnames(noVacc_counts) <- allStates
  vacc_counts <- vacc_mod$eval_strategy_list$standard$counts[,nonZeroCols]
  colnames(vacc_counts) <- allStates

  #costs per age (will be different to what user has given if discounting,
  #               but same for noVacc to vacc)
  costs_per_age <- data.frame(noVacc_mod$eval_strategy_list$standard$states)[,intersect(costCols, outputCols)][,nonZeroCols]

  #dalys per age (as above)
  dalys_per_age <- data.frame(noVacc_mod$eval_strategy_list$standard$states)[,intersect(dalyCols, outputCols)][,nonZeroCols]

  #Incidence nos.
  noVacc <- apply(data.frame(noVacc_counts[,conditions]), 2, sum)
  vacc <- apply(data.frame(vacc_counts[,conditions]), 2, sum)

  row1 <- c(noVacc, sum(noVacc))
  row2 <- c(vacc, sum(vacc))
  row3 <- 100*c((noVacc-vacc)/noVacc, (sum(noVacc)-sum(vacc))/sum(noVacc)) #% diff

  #Health burden
  noVacc_dalys <- apply(as.matrix((noVacc_counts*dalys_per_age)[,conditions]), 2, sum)
  vacc_dalys <- apply(as.matrix((vacc_counts*dalys_per_age)[,conditions]), 2, sum)

  row4 <- c(noVacc_dalys,sum(noVacc_dalys))
  row5 <- c(vacc_dalys,sum(vacc_dalys))
  row6 <- 100*c((noVacc_dalys-vacc_dalys)/noVacc_dalys, (sum(noVacc_dalys)-sum(vacc_dalys))/sum(noVacc_dalys))

  #Economic burden
  noVacc_costs <- apply(as.matrix((noVacc_counts*costs_per_age)[,conditions]), 2, sum)
  vacc_costs <- apply(as.matrix((vacc_counts*costs_per_age)[,conditions]), 2, sum)

  row7 <- c(noVacc_costs, sum(noVacc_costs))
  row8 <- c(vacc_costs, sum(vacc_costs))
  row9 <- 100*c((noVacc_costs-vacc_costs)/noVacc_costs, (sum(noVacc_costs)-sum(vacc_costs))/sum(noVacc_costs))

  #ICER
  ICER<-findICER(noVacc_costs, vacc_costs, noVacc_dalys, vacc_dalys, initPop, valueVac)
  totalICER<-findICER(sum(noVacc_costs), sum(vacc_costs), sum(noVacc_dalys), sum(vacc_dalys), initPop, valueVac)

  row10<-c(ICER, totalICER)

  impactT <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10)
  colnames(impactT) <- c(conditions,"Total")
  rownames(impactT) <- c("Incidents (No Vacc)", "Incidents (Vacc)", "Incidents %",
                     "DALYs (No Vacc)", "DALYs (Vacc)", "DALYs %",
                     "Cost (No Vacc)", "Cost (Vacc)", "Cost %",
                     "ICER")
  return(impactT)
}

#' Create line plot comparison between vaccine and no vaccine scenarios
#'
#' @param noVacc_mod heemod model for no vaccine scenario
#' @param vacc_mod heemod model for vaccine scenario
#' @param conditions conditions modeled
#'
#' @return ggplot
#' @export
makePlot <- function(noVacc_mod, vacc_mod, conditions)
{
  #Currently written for comparison between vacc and no vacc scenario of only
  #one condition

  allStates <- c("Well", conditions, "Deceased")

  outputCols <- grep("dots", colnames(data.frame(noVacc_mod$eval_strategy_list$standard$states)))
  costCols <- grep("cost", colnames(data.frame(noVacc_mod$eval_strategy_list$standard$states)))
  numModelStates <- length(intersect(costCols, outputCols))
  nonZeroCols <- c(1:(length(conditions)+1), numModelStates)

  noVacc_counts <- noVacc_mod$eval_strategy_list$standard$counts[,nonZeroCols]
  colnames(noVacc_counts) <- allStates
  vacc_counts <- vacc_mod$eval_strategy_list$standard$counts[,nonZeroCols]
  colnames(vacc_counts) <- allStates

  counts <- cbind(noVacc_counts[, conditions], vacc_counts[, conditions])
  counts<-apply(counts, 2, cumsum)
  colnames(counts) <- c(paste(conditions, "No Vacc"), paste(conditions, "Vacc"))

  meltCounts <- reshape2::melt(counts)
  meltCounts[,1] <- rep(0:(nrow(counts)-1),2)
  colnames(meltCounts) <- c("Age", "VaccStatus", "NumCases")

  p <- ggplot2::ggplot(data=meltCounts, ggplot2::aes(x=Age, y=NumCases, group=VaccStatus )) +
                      ggplot2::geom_line(ggplot2::aes(colour=VaccStatus)) +
                      ggplot2::ylab("Cumulative no. of cases per 100,000 persons") +
                      ggplot2::theme(legend.title = ggplot2::element_blank())
  return(p)
}




