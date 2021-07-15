#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(ggplot2)
library(reshape2)
library(FinCal)
library(triangle)
library(heemod)
library(ggpubr)
library(rCAT)

##########################################################################################
#FUNCTIONS

#INCIDENT RATES w UNCERTAINTY INTERVALS
#Find incident rates
incRates<-function(careEp, pop, propAttr, perPopSize=100000, years=1, grps=1)
  #careEp: raw care episodes-at-age, pop: population-at-age that care episodes relate to
  #propAttr: prop of episodes-at-age that are attributable to GAS for each disease
  #perPopSize: eg. per 100 persons, per 100k, years: how many years does data cover
  #grps: combine and present results in user provided groups
{
  age<-careEp[,1]
  careEp<-careEp[,-1]
  careEp<-careEp/years #find yearly average
  rownames(careEp)<-age
  colnames(pop)<-c("Age", "n")
  
  if(is.data.frame(grps))
  { #if grps is a data frame with disease groups, aggregate careEp data
    
    newCareEp<-matrix(NA, nrow=length(age), ncol=ncol(grps))
    rownames(newCareEp)<-age
    colnames(newCareEp)<-colnames(grps)
    
    for(i in 1:ncol(grps))
    {
      diseases<-grps[which(grps[,i]!=""),i] #diseases in group
      
      
      epXprop<-propAttr[,diseases]*careEp[,diseases] #mult. each ep. with relevant prop.
      
      if(length(diseases)>1)
      {
        newCareEp[,colnames(grps)[i]]<-apply(epXprop, 1, sum)
      }else{
        newCareEp[,colnames(grps)[i]]<-epXprop
      }
      
    }
    newPropAttr<-matrix(1, nrow=length(age), ncol=ncol(grps))
    colnames(newPropAttr)<-colnames(grps)
    propAttr<-newPropAttr
    careEp<-newCareEp
  }
  
  Epi_categories<-colnames(careEp)
  
  Epi_headings<-c(Epi_categories[1], paste(Epi_categories[1], "Lower"), paste(Epi_categories[1], "Upper"))
  if(length(Epi_categories)>1)
  {
    for(i in 2:length(Epi_categories))
      Epi_headings<-c(Epi_headings, Epi_categories[i], paste(Epi_categories[i], "Lower"), paste(Epi_categories[i], "Upper"))
  }
  
  Epi_incidents<-matrix(NA, nrow=length(age), ncol=length(Epi_headings))
  Epi_incidents_Total<-matrix(NA, nrow=1, ncol=length(Epi_headings))
  rownames(Epi_incidents)<-age
  colnames(Epi_incidents)<-Epi_headings
  colnames(Epi_incidents_Total)<-Epi_headings
  
  #incident rate=propAttrGASx(NoIncidentsPerYear/SizeOfIncidentPop)x perNoPeople (no. of episodes per 100 people)
  for(i in Epi_categories)
  {
    Epi_incidents[,i]<-propAttr[,i]*(careEp[,i]/pop[,"n"])*perPopSize
    Epi_incidents_Total[,i]<-sum(Epi_incidents[,i])
    
    for(j in 1:length(age))
    { #uncertainty interval
      rSampleInc <- rgamma(1000, shape =careEp[j,i], rate = pop[j,"n"])
      Epi_incidents[j, paste(i, "Lower")] <- propAttr[j, i]*quantile(rSampleInc, 0.025)*perPopSize
      Epi_incidents[j, paste(i, "Upper")] <- propAttr[j, i]*quantile(rSampleInc, 0.975)*perPopSize
    }
    rSampleInc <- rgamma(1000, shape =sum(careEp[,i]), rate = sum(pop[,"n"]))
    #How to combine as proportion is different for different ages?
    #GP_incidents_Total[, paste(i, "Lower")] <- GP_propAttr_GAS[j, i]*quantile(rSampleInc, 0.025)*100
    #GP_incidents[j, paste(i, "Upper")] <- GP_propAttr_GAS[j, i]*quantile(rSampleInc, 0.975)*100
  }
  
  return(Epi_incidents)
}

#DALYS w UNCERTAINTY INTERVALS
dalys<-function(incR, rate, pop, life, dw_A, dw_C, prog, dRate, duration, deaths, deathRate)
  #incident rates (per person), population, life expectancies at age, 
  # acute disability weights and chronic disability weights (with uncertainty), discount rate
  # progression to chronic, life expectancy / duration of disease, death rate (per person)
{
  # incR<-read.csv("..\\Input data\\GP_incR.csv", check.names = F)
  # pop<-read.csv("..\\Input data\\pop.csv", check.names = F)
  # duration<-read.csv("..\\Input data\\lifeEx.csv", check.names = F)
  # dw<-read.csv("..\\Input data\\GP_dw.csv", check.names = F)
  # deaths<-read.csv("..\\Input data\\GP_deaths.csv", check.names = F)
  # dRate<-3.5
  # rate<-100
  
  age<-incR[,1]
  incR<-incR[,-1]/rate
  rownames(incR)<-age
  colnames(pop)<-c("Age", "n")
  deaths<-deaths[,-1]/deathRate
  dRate<-dRate/100
  rownames(dw_A)<-dw_A[,1]
  dw_A<-dw_A[,-1]
  colnames(dw_A)<-c("w", "min", "max")
  rownames(dw_C)<-dw_C[,1]
  dw_C<-dw_C[,-1]
  colnames(dw_C)<-c("w", "min", "max")
  
  daly_categories<-colnames(incR)
  
  daly_headings<-c(daly_categories[1], paste(daly_categories[1], "Lower"), paste(daly_categories[1], "Upper"))
  if(length(daly_categories)>1)
  {
    for(i in 2:length(daly_categories))
      daly_headings<-c(daly_headings, daly_categories[i], paste(daly_categories[i], "Lower"), paste(daly_categories[i], "Upper"))
  }
  
  dalyT<-matrix(NA, nrow=length(age), ncol=length(daly_headings))
  daly_Total<-matrix(NA, nrow=1, ncol=length(daly_headings))
  rownames(dalyT)<-age
  colnames(dalyT)<-daly_headings
  colnames(daly_Total)<-daly_headings
  
  #if discounting, change duration values to present value
  #duration should be disease dependent, for those conditions
  #that are chronic or can progress to a chronic condition
  if(dRate>0)
  { 
    for(j in rownames((dw_C))) 
    { 
      for(i in 1:length(age))
        duration[i,j]<--pv(dRate,duration[i, j],0,1,1)
    }  
  }
  
  #Assume first that all have DALY=YLD+YLL
  for(i in daly_categories)
  {
    #YLD_acute = no.episodes x disability weight
    #YLD_chronic = prop. that progress to chronic x no. episodes (rate/person x no. people) 
    #             x disability weight x duration (either with or without discounting)
    YLD_acute<-0
    YLD_chronic<-0
    
    if(i %in% rownames(dw_A))
      YLD_acute<-incR[,i]*pop[,"n"]*dw_A[i,"w"]
    if(i %in% rownames(dw_C))
      YLD_chronic<-prog[,i]*incR[,i]*pop[,"n"]*dw_C[i,"w"]*duration[, i]
    
    YLD <- YLD_acute+YLD_chronic 
    
    #uncertainty
    for(j in 1:length(age))
    {
      #Note: gamma distribution parameters are an approximation if the incident 
      #rates were based on a care population smaller than country population
      dalyT[j, paste(i, "Lower")]<-0
      dalyT[j, paste(i, "Upper")]<-0
      
      rSampleInc <- rgamma(1000, shape =incR[j,i]*pop[j, "n"], rate = pop[j, "n"]) 
      
      if(i %in% rownames(dw_A)){
        rSampleDW_A  <- rtriangle(1000, dw_A[i,"min"], dw_A[i,"max"], dw_A[i,"w"])
        Lower_A <- quantile(rSampleInc*rSampleDW_A, 0.025)*pop[j, "n"]
        Upper_A <- quantile(rSampleInc*rSampleDW_A, 0.975)*pop[j, "n"]
        dalyT[j, paste(i, "Lower")]<-dalyT[j, paste(i, "Lower")]+as.numeric(Lower_A)
        dalyT[j, paste(i, "Upper")]<-dalyT[j, paste(i, "Upper")]+as.numeric(Upper_A)
      }  
      if(i %in% rownames(dw_C)){
        #browser()
        rSampleDW_C  <- rtriangle(1000, dw_C[i,"min"], dw_C[i,"max"], dw_C[i,"w"])
        Lower_C <- prog[i]*quantile(rSampleInc*rSampleDW_C, 0.025)*pop[j, "n"]*duration[j, i]
        Upper_C <- prog[i]*quantile(rSampleInc*rSampleDW_C, 0.975)*pop[j, "n"]*duration[j, i]
        dalyT[j, paste(i, "Lower")]<-dalyT[j, paste(i, "Lower")]+as.numeric(Lower_C)
        dalyT[j, paste(i, "Upper")]<-dalyT[j, paste(i, "Upper")]+as.numeric(Upper_C)
      }  
    }
    
    YLL<-deaths[,i]*life[,"le"]
    
    dalyT[,i]<-YLD+YLL
    dalyT[, paste(i, "Lower")]<-dalyT[, paste(i, "Lower")]+YLL
    dalyT[, paste(i, "Upper")]<-dalyT[, paste(i, "Upper")]+YLL
    
  }  
  
  
  return(dalyT)
}

#change incidence probabilities to being unconditional
probUncondition <- function(Pr) 
{
  #Pr      : vector/row of combined incid prob and all-cause mortality  
  #incProb : incident probabilities conditional on being alive
  #m       : probability of all-cause mortality
  pos_m<-length(Pr)
  m<-Pr[pos_m]
  incProb<-Pr[1:(pos_m-1)]
  newProb<-c(incProb*(1-m), m)
  return(newProb)
}

transProb <- function(prM, lifeTable, maxAge, V_Eff, V_Age, V_Dur)
  #prM: prob of disease|not dead,  before adjustment for vaccine
  #lifeTable: all-cause mortality 
  #maxAge: give no. of years over which transition probs. calculated
  #V_Eff: vaccine effectiveness, reduce prob. by this proportion
  #V_Age: age when have vaccine
  #V_Dur: length of time that vaccine is effective for
{
  trProb<-prM
  
  if(V_Eff != 0) #if vaccinating
  {  
    vTime<-(1+V_Age):(V_Age+V_Dur) #from time of vacc to end of duration of effectiveness
    trProb[vTime,]<-(1-V_Eff)*trProb[vTime,] #reduce prob. of incidents due to vaccine
  }
  
  trProb<-cbind(trProb, lifeTable[1:(maxAge+1)])
  colnames(trProb)[ncol(trProb)]<-"Deceased"
  
  trProb<-t(apply(trProb, 1, probUncondition)) 
  
  PrWell<-apply(trProb, 1, function(x) 1-sum(x))
  trProb<-cbind(as.matrix(PrWell), trProb)
  colnames(trProb)[1]<-"Well"
  
  return(trProb)
}

#find value of vaccine
findVacValue <- function(c0, cV, e0, eV, popS, ICER) 
{
  VV <- (c0-cV + ICER*(e0-eV))/popS
  return(VV)
}

#find incremental cost-effectiveness ratio (ICER)
#CHECK
findICER <- function(c0, cV, e0, eV, popS, VV) 
{
  ICER <-((VV*popS)+cV-c0)/(e0-eV)
  return(ICER)
}


#set up and run heemod markov model
heemodModel <-function(probM, dalysM, dR, costM, Initpop, ageInit, cycleT) 
  #probM: matrix of transition probs over model time period
  #dalysM/costM: dalys/cost for each age
  #Initpop: pop size of cohort,  ageInit: age when have vaccine
  #cycleT: run model until this age
{

  numStates <- ncol(probM) #Always Well and Deceased State, plus diseased states
  maxDStates<-10 #max. diseased states
  noDummyStates<-maxDStates-numStates+2 #2 non-diseased states : Well, Deceased
  
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
  
  model_param <- define_parameters(
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
  
  model_trans <- define_transition(
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
  
  model_strat<-define_strategy(
    transition = model_trans,
    Well = define_state(utility=0,cost=0),
    Disease1 = define_state(utility=discount(dalysM[age,1], dR), cost=discount(costM[age,1], dR)),
    Disease2 = define_state(utility=discount(dalysM[age,2], dR), cost=discount(costM[age,2], dR)),
    Disease3 = define_state(utility=discount(dalysM[age,3], dR), cost=discount(costM[age,3], dR)),
    Disease4 = define_state(utility=discount(dalysM[age,4], dR), cost=discount(costM[age,4], dR)),
    Disease5 = define_state(utility=discount(dalysM[age,5], dR), cost=discount(costM[age,5], dR)),
    Disease6 = define_state(utility=discount(dalysM[age,6], dR), cost=discount(costM[age,6], dR)),
    Disease7 = define_state(utility=discount(dalysM[age,7], dR), cost=discount(costM[age,7], dR)),
    Disease8 = define_state(utility=discount(dalysM[age,8], dR), cost=discount(costM[age,8], dR)),
    Disease9 = define_state(utility=discount(dalysM[age,9], dR), cost=discount(costM[age,9], dR)),
    Disease10 = define_state(utility=discount(dalysM[age,10], dR), cost=discount(costM[age,10], dR)),
    Deceased = define_state(utility=0, cost=0)
  )
  
  model <- run_model(
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


#Markov model
markovModel<- function(age, inc, rate, costs, dalys, dalyRate, mortality, initPop,
                       dRate, nyears, vaccAge, vaccEff, vaccDur, valueVac)
#Age group names and year range, Incidents and related rate, costs per person, DALYs and related rate,
#mortality rates at age, initial population, no. years to project over, age of vaccination, 
#effectiveness of vaccine, duration that vaccine is effective, value of vaccine
{
  # age<-read.csv("C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\GAS Model\\R Code\\Shiny App\\Input data\\age_groups.csv", check.names=FALSE)
  # inc<-read.csv("C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\GAS Model\\R Code\\Shiny App\\Input data\\AUS_Incidents.csv", check.names=FALSE)
  # rate<-100000
  # costs<-read.csv("C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\GAS Model\\R Code\\Shiny App\\Input data\\AUS_Costs.csv", check.names=FALSE)
  # dalys<-read.csv("C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\GAS Model\\R Code\\Shiny App\\Input data\\AUS_Burden.csv", check.names=FALSE)
  # dalyRate<-100000
  # mortality<-read.csv("C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\GAS Model\\R Code\\Shiny App\\Input data\\mortAtAge.csv", check.names=FALSE)
  # InitPop<-18537
  # dRate<-5
  # nyears<-85
  # vaccAge<-0
  # vaccEff<-70
  # vaccDur<-10
  # valueVac<-100

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

##################################################################################################
#User interface code

ui <- navbarPage(title = "",

    tabPanel(title = "Incident rates",
             
     img(src = "Savac-logo.png", height = 140, width = 400),
     
     sidebarLayout(
        sidebarPanel(
            h4("Health care"),
            fileInput('care_incidents_raw', 'Care episodes',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            fileInput('care_pop', 'Care population',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            fileInput('care_prop', 'Proportion of episodes attributable to GAS',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            fileInput('care_group', 'Display results using disease groups',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            numericInput(inputId = "care_yrs", label = "Episodes over how many years?", value = 1, min=1),
            selectInput(inputId = "care_rate", label = "Rate (incidents per n persons):",
                        choices = c("100", "100,000")),
            actionButton("submitButton1", "Find incident rates", class = "btn-success"),
            downloadButton("saveTable", "Save table"),
            downloadButton("savePlot", "Save plot")
        ),


        mainPanel(
            tableOutput('incRatesTable'),
            plotOutput('incRatesPlot')
         
        )
      ),#end sidebarLayout
    ),#end tabPanel
    
   tabPanel(title = "Cost",
            img(src = "Savac-logo.png", height = 140, width = 400),
   ),
   
   tabPanel(title = "Health burden",
            
    img(src = "Savac-logo.png", height = 140, width = 400),
            
    sidebarLayout(
        sidebarPanel(
            
            h4("Population"),
            fileInput('pop_age', 'Population',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            fileInput('life_ex', 'Life expectancy',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            fileInput('inc_rates', 'Incident rates',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            selectInput(inputId = "i_rate", label = "Rate used (incidents per n persons):",
                        choices = c("100", "100,000")),
            fileInput('deaths', 'Death rates',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            selectInput(inputId = "death_rate", label = "Rate used (deaths per n persons):",
                        choices = c("100", "100,000")),
            h4("Disease"),
            fileInput('dw_acute', 'Acute disability weights',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            fileInput('dw_chronic', 'Chronic disability weights',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            fileInput('dis_prog', 'Proportion of episodes that progress to chronic condition',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            fileInput('dis_dur', 'Disease duration',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            numericInput(inputId = "d_rate", label = "Discount rate %", value = 0, min=0, max=10, step=0.5),
            fileInput('daly_group', 'Display results using disease groups',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            
            actionButton("submitButton2", "Find DALYs", class = "btn-success"),
            downloadButton("saveDALYTable", "Save table"),
            downloadButton("saveDALYPlot", "Save plot")
    ),
   
        mainPanel(
            tableOutput('dalyTable'),
            plotOutput('dalyPlot')
        )
    
    ),#end sidebarLayout
   ),#end tabPanel
   
   
   tabPanel(title= "Impact analysis",
            img(src = "Savac-logo.png", height = 140, width = 400),
            
            sidebarLayout(
              sidebarPanel(
                
                fileInput('imp_age', 'Age groups',
                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                fileInput('imp_inc_rates', 'Incident rates',
                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                selectInput(inputId = "imp_rate", label = "Rate used (incidents per n persons):",
                            choices = c("100", "100,000")),
                fileInput('imp_cost', 'Cost per person',
                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                fileInput('imp_burden', 'DALYs',
                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                selectInput(inputId = "imp_bur_rate", label = "Rate used (DALYs per n persons):",
                            choices = c("100", "100,000")),
                fileInput('imp_mor', 'Mortality rates at age',
                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                numericInput(inputId = "imp_popsize", label = "Initial population size", value=100000, min=100),
                numericInput(inputId = "imp_cycle", label = "Project over n years", value = 10, min=10, max=85, step=5),
                numericInput(inputId = "imp_d_rate", label = "Discount rate %", value = 0, min=0, max=10, step=0.5),
                numericInput(inputId = "imp_vaccEff", label = "Vaccine effectiveness %", value = 50, min=0, max=100, step=5),
                numericInput(inputId = "imp_vaccDur", label = "Duration of effectiveness", value = 1, min=1, max=15, step=1),
                numericInput(inputId = "imp_vaccAge", label = "Age of vaccination", value = 0, min=0, max=65, step=1),
                numericInput(inputId = "imp_valueVacc", label = "Value of vaccine", value = 0, min=0, max=200, step=1),
                numericInput(inputId = "imp_ICER", label = "Maximum acceptable ICER", value = 50000, min=0, max=100000, step=10000),
                
                actionButton("submitButton3", "Run analysis", class = "btn-success"),
                downloadButton("saveImpactTable", "Save table"),
                downloadButton("saveImpactPlot", "Save plot")
                
              ),
              
              mainPanel(
                tableOutput('impactTable'),
                plotOutput('impactPlot')
              )
              
            ),#end sidebarLayout
  )#end tabpanel
)#end navbarPage


server <- function(input, output) {

#Incident rates tab    
incRatesPlot <- reactiveVal()

incRatesData <- reactive({

    input$submitButton1

    epFile <- isolate(input$care_incidents_raw)
    popFile <-isolate(input$care_pop)
    propFile <- isolate(input$care_prop)
    groupFile <- isolate(input$care_group)
    yrs <- isolate(input$care_yrs)
    rate <- isolate(as.numeric(str_replace(input$care_rate, ",", "")))

    if(is.null(epFile)||is.null(popFile)||is.null(propFile))
        return(NULL)

    ep <-read.csv(epFile$datapath, check.names=FALSE)
    pop <-read.csv(popFile$datapath, check.names=FALSE)
    prop <-read.csv(propFile$datapath, check.names=FALSE)
    group <-read.csv(groupFile$datapath, check.names=FALSE)

    incRatesT<-incRates(careEp = ep, pop = pop, propAttr = prop, perPopSize = rate, years = yrs, grps=group)
    
    if(is.data.frame(group))
    {   
        incRatesMeans<-t(incRatesT[, colnames(group)])   
    }else{   
        incRatesMeans<-t(incRatesT[, colnames(ep)[-1]])
    }    

    meltIncRates<-melt(incRatesMeans)
    colnames(meltIncRates)<-c("Condition", "Age", "Rate")
    incRatesPlot(ggplot(data=meltIncRates, aes(x=Age, y=Rate, fill=Condition )) +
                      geom_bar(position="stack", stat="identity"))
    
    incRatesT<-cbind(rownames(incRatesT), round(incRatesT, digits=2))
    colnames(incRatesT)[1]<-"Age"
    for(i in 1:length(colnames(incRatesT)))
    {
        if(length(grep("Lower", colnames(incRatesT)[i]))) colnames(incRatesT)[i]<-"L 95%"
        if(length(grep("Upper", colnames(incRatesT)[i]))) colnames(incRatesT)[i]<-"U 95%"
    }

    incRatesT
})

output$incRatesPlot <- renderPlot({
    incRatesPlot()
})

output$incRatesTable <- renderTable({
    incRatesData()
})


output$saveTable <- downloadHandler(
    filename = function() {
        paste("incidentRatesTable", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
        write.csv(incRatesData(), file, row.names = F)
    }
 )

output$savePlot <- downloadHandler(
    filename = function() {
        paste("incidentRatesPlot", Sys.Date(), ".jpeg", sep="")
    },
    content = function(file) {
        ggsave(file, incRatesPlot())
    }
)

##Health burden tab
dalyPlot <- reactiveVal()

dalyData <- reactive({
    
    input$submitButton2
    
    popFile <- isolate(input$pop_age)
    lifeFile <- isolate(input$life_ex)
    incFile <- isolate(input$inc_rates)
    rate <- isolate(as.numeric(str_replace(input$i_rate, ",", "")))
    dwAFile <- isolate(input$dw_acute)
    dwCFile <- isolate(input$dw_chronic)
    deathFile <- isolate(input$deaths)
    death_rate <- isolate(as.numeric(str_replace(input$death_rate, ",", "")))
    progFile <- isolate(input$dis_prog)
    durFile <- isolate(input$dis_dur)
    dRate <- isolate(input$d_rate)
    grpFile <- isolate(input$daly_group)
    
    if(is.null(popFile)||is.null(lifeFile)||is.null(incFile)||is.null(dwAFile)||
       is.null(dwCFile)||is.null(deathFile)||is.null(progFile)||is.null(durFile)||
       is.null(grpFile))
        return(NULL)

    pop <-read.csv(popFile$datapath, check.names=FALSE)
    lifeEx <-read.csv(lifeFile$datapath, check.names = FALSE)
    inc_rates <-read.csv(incFile$datapath, check.names=FALSE)
    dw_acute <-read.csv(dwAFile$datapath, check.names=FALSE)
    dw_chronic <-read.csv(dwCFile$datapath, check.names=FALSE)
    deaths <-read.csv(deathFile$datapath, check.names = FALSE)
    prog <-read.csv(progFile$datapath, check.names=FALSE)
    dur <-read.csv(durFile$datapath, check.names=FALSE)
    grp <-read.csv(grpFile$datapath, check.names=FALSE)
    
    dalysT<-dalys(incR = inc_rates, rate = rate, pop =pop, life = lifeEx, dw_A = dw_acute,
                  dw_C = dw_chronic, prog = prog, dRate = dRate, 
                  duration =dur, deaths = deaths, deathRate = death_rate)

    dalysT<-100000*dalysT/pop[,"n"] #get DALYs per 100,000
    dalyMeans<-t(dalysT[, colnames(inc_rates)[-1]])
        
    meltDALYS<-melt(dalyMeans)
    colnames(meltDALYS)<-c("Condition", "Age", "DALYS")
    dalyPlot(ggplot(data=meltDALYS, aes(x=Age, y=DALYS, fill=Condition )) +
                     geom_bar(position="stack", stat="identity")+
                     ylab("DALYs per 100,000 persons"))
    
    dalysT<-cbind(rownames(dalysT), round(dalysT, digits=2))
    colnames(dalysT)[1]<-"Age"
    for(i in 1:length(colnames(dalysT)))
    {
        if(length(grep("Lower", colnames(dalysT)[i]))) colnames(dalysT)[i]<-"L 95%"
        if(length(grep("Upper", colnames(dalysT)[i]))) colnames(dalysT)[i]<-"U 95%"
    }
    
    dalysT
})

output$dalyPlot <- renderPlot({
    dalyPlot()
})

output$dalyTable <- renderTable({
    dalyData()
})


output$saveDALYTable <- downloadHandler(
    filename = function() {
        paste("dalyTable", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
        write.csv(dalyData(), file, row.names = F)
    }
)

output$saveDALYPlot <- downloadHandler(
    filename = function() {
        paste("dalyPlot", Sys.Date(), ".jpeg", sep="")
    },
    content = function(file) {
        ggsave(file, dalyPlot())
    }
)

##Impact analysis tab
impactPlot <- reactiveVal()

impactData <- reactive({
  
  input$submitButton3
  
  imp_ageFile <- isolate(input$imp_age)
  imp_incFile <- isolate(input$imp_inc_rates)
  imp_rate <- isolate(as.numeric(str_replace(input$imp_rate, ",", "")))
  imp_costFile <- isolate(input$imp_cost)
  imp_burdenFile <- isolate(input$imp_burden)
  imp_bur_rate <- isolate(as.numeric(str_replace(input$imp_bur_rate, ",", "")))
  imp_morFile <- isolate(input$imp_mor)
  imp_popsize <- isolate(input$imp_popsize)
  imp_cycle <- isolate(input$imp_cycle)
  imp_d_rate <- isolate(input$imp_d_rate)
  imp_vaccEff <- isolate(input$imp_vaccEff)
  imp_vaccDur <- isolate(input$imp_vaccDur)
  imp_vaccAge <- isolate(input$imp_vaccAge)
  imp_valueVacc <- isolate(input$imp_valueVacc)
  imp_ICER <-isolate(input$imp_ICER)
  
  if(is.null(imp_ageFile)||is.null(imp_incFile)||is.null(imp_costFile)||
     is.null(imp_burdenFile)||is.null(imp_morFile))
    return(NULL)
  
  imp_age <- read.csv(imp_ageFile$datapath, check.names=FALSE)
  imp_inc_rates <-read.csv(imp_incFile$datapath, check.names=FALSE)
  imp_cost <-read.csv(imp_costFile$datapath, check.names=FALSE)
  imp_burden <-read.csv(imp_burdenFile$datapath, check.names=FALSE)
  imp_mor <-read.csv(imp_morFile$datapath, check.names=FALSE)

  impT<-markovModel(imp_age, imp_inc_rates, imp_rate, imp_cost, imp_burden, 
                    imp_bur_rate, imp_mor, imp_popsize, imp_d_rate, imp_cycle,
                    imp_vaccAge, imp_vaccEff, imp_vaccDur, imp_valueVacc)
  
  meltInc<-melt(impT[c("Incidents (No Vacc)", "Incidents (Vacc)"),-which(colnames(impT)=="Total")])
  colnames(meltInc)<-c("Scenario", "Condition", "NoInc")
  plot1<-ggplot(data=meltInc, aes(x=Condition, y=NoInc, fill=Scenario )) +
           geom_bar(position="stack", stat="identity")+
           scale_fill_discrete(name = "", labels = c("No vaccine", "Vaccine"))+
           ylab("Healthcare episodes")  
  
  meltBur<-melt(impT[c("DALYs (No Vacc)", "DALYs (Vacc)"),-which(colnames(impT)=="Total")])
  colnames(meltBur)<-c("Scenario", "Condition", "NoDALYS")
  plot2<-ggplot(data=meltBur, aes(x=Condition, y=NoDALYS, fill=Scenario )) +
    geom_bar(position="stack", stat="identity")+
    scale_fill_discrete(name = "", labels = c("No vaccine", "Vaccine"))+
    ylab("DALYs") 
  
  meltCost<-melt(impT[c("Cost (No Vacc)", "Cost (Vacc)"),-which(colnames(impT)=="Total")]/10000)
  colnames(meltCost)<-c("Scenario", "Condition", "Cost")
  plot3<-ggplot(data=meltCost, aes(x=Condition, y=Cost, fill=Scenario )) +
    geom_bar(position="stack", stat="identity")+
    scale_fill_discrete(name = "", labels = c("No vaccine", "Vaccine"))+
    ylab("Cost* (x 10,000)")+ 
    labs(caption = "* not taking into account cost of vaccine") 
  
  
  CostDiff<-imp_valueVacc*imp_popsize+impT["Cost (Vacc)",]-impT["Cost (No Vacc)",]
  DALYSGained<-impT["DALYs (No Vacc)",]-impT["DALYs (Vacc)",]
  ICERdata<-data.frame(CostDiff=CostDiff/100000, DALYSGained)
  
  m<-imp_ICER/100000
  
  ymax<-max(abs(ICERdata$CostDiff))
  xmax<-max(abs(ICERdata$DALYSGained))
  plot4<-ggplot(ICERdata, aes(x=DALYSGained, y=CostDiff, label=rownames(ICERdata)))+
    geom_point(aes(colour = factor(rownames(ICERdata))), size=3)+
    labs(colour="")+
    geom_abline(slope=m, linetype="dashed")+
    geom_text(x=-ymax/m, y=-ymax+ymax/10, label="Maximum acceptable ICER", size=4)+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept = 0)+
    xlim(-xmax, xmax)+
    ylim(-ymax, ymax)+
    xlab("DALYs gained")+
    ylab("Cost difference (x 100,000)")
  
  impactPlot(ggarrange(plot1, plot2, plot3, plot4,
                       ncol = 2, nrow = 2))
  
  
  impT<-round(impT, digits=0)

  impT
})

output$impactPlot <- renderPlot({
  impactPlot()
})

output$impactTable <- renderTable({
  impactData()
}, rownames = TRUE)


output$saveImpactTable <- downloadHandler(
  filename = function() {
    paste("impactTable", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(impactData(), file, row.names = F)
  }
)

output$saveImpactPlot <- downloadHandler(
  filename = function() {
    paste("impactPlot", Sys.Date(), ".jpeg", sep="")
  },
  content = function(file) {
    ggsave(file, impactPlot())
  }
)


}

# Run the application 
shinyApp(ui = ui, server = server)
