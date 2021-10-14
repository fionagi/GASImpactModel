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
#' @param impType type of impact analysis: Calendar year, Year of birth, Year of vaccination
#' @param pYears no. of years to project
#' @param initPop used for modeling if metric is Number
#'
#' @return list
#' @export
runModelBulk<- function(location, condition, inc, propA = 1, rate = 100000,
                    mortality, dRate = 0, nyears = 85, yearV, vaccAge = 0,
                    vaccEff = 100, vaccDur = 10, impType, pYears = 10, initPop)
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

  #create matrix with all DALYs for all ages
  #Currently does not allow for deaths due to the condition, i.e. assumes YLL=0
  #Below is NOT full YLD, just the "adjusted" weighting taking into account duration
  #of illness (assuming duration < 1 year). Needs to be multiplied by number of cases
  DW <- disability_weights[disability_weights$Condition == condition,]$DW
  dur <- duration[duration$Condition == condition,]$Days /365 #condition duration in years
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
    #noVacc_deaths[,i]
    noVacc_pop[,i] <- c(initPop[i,2], pop_end_of_year[-length(pop_end_of_year)])

   }#end for pYears

  #Now scale values to get required vaccination scenario results
  vacc_counts <- noVacc_counts
  vacc_counts[(vaccAge+1):(vaccAge+vaccDur),] <- (1-vaccEff)*vacc_counts[(vaccAge+1):(vaccAge+vaccDur),]

  vacc_dalys <- noVacc_dalys
  vacc_dalys[(vaccAge+1):(vaccAge+vaccDur),] <- (1-vaccEff)*vacc_dalys[(vaccAge+1):(vaccAge+vaccDur),]

  #vacc_deaths

  return(list(noVacc_counts, vacc_counts, noVacc_dalys, vacc_dalys,
              noVacc_deaths, vacc_deaths, noVacc_pop))
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
makeBPlot <- function(noVacc_data, vacc_data, ylabel, maxAge = 84, vAge, vDur,
                     vYear, impType, pYears)
{
  rownames(noVacc_data) <- 0:maxAge
  colnames(noVacc_data) <- vYear:(vYear+pYears)
  rownames(vacc_data) <- 0:maxAge
  colnames(vacc_data) <- vYear:(vYear+pYears)

  totalNoVacc <- apply(noVacc_data, 2, sum) #pre-vaxx total no. incidents, deaths or dalys

  ymax <- max(totalNoVacc)*1.01

  meltNoVacc <- reshape2::melt(noVacc_data)
  colnames(meltNoVacc) <- c("Age", "Year", "Numbers")

  meltVacc <- reshape2::melt(vacc_data)
  colnames(meltVacc) <- c("Age", "Year", "Numbers")

  xlabel <- impType
  ylabel_noVacc <- paste(ylabel, "(pre-vacc)")
  ylabel_vacc <- paste(ylabel, "(vacc)")

  xtick_lab <- as.character(vYear:(vYear+pYears))
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

#' Create table of comparison statistics
#'
#' @param noVacc_counts numbers of cases for pre-vaccine scenario
#' @param vacc_counts numbers of cases for vaccine scenario
#' @param noVacc_dalys DALYs for pre-vaccine scenario
#' @param vacc_dalys DALYs for vaccine scenario
#' @param noVacc_deaths numbers of deaths for pre-vaccine scenario
#' @param vacc_deaths numbers of deaths for vaccine scenario
#'
#' @return data.frame
#' @export
makeTable <- function(noVacc_counts, vacc_counts, noVacc_dalys, vacc_dalys,
                      noVacc_deaths, vacc_deaths)
{
  years <- colnames(noVacc_counts)
  noV_counts <- matrix(NA, nrow = nrow(age_groups), ncol = length(years))
  colnames(noV_counts) <- age_groups$Label
  rownames(noV_counts) <- years
  v_counts <- matrix(NA, nrow = nrow(age_groups), ncol = length(years))
  colnames(v_counts) <- age_groups$Label
  rownames(v_counts) <- years
  noV_dalys <- matrix(NA, nrow = nrow(age_groups), ncol = length(years))
  colnames(noV_dalys) <- age_groups$Label
  rownames(noV_dalys) <- years
  v_dalys <- matrix(NA, nrow = nrow(age_groups), ncol = length(years))
  colnames(v_dalys) <- age_groups$Label
  rownames(v_dalys) <- years
  noV_deaths <- matrix(NA, nrow = nrow(age_groups), ncol = length(years))
  colnames(noV_deaths) <- age_groups$Label
  rownames(noV_deaths) <- years
  v_deaths <- matrix(NA, nrow = nrow(age_groups), ncol = length(years))
  colnames(v_deaths) <- age_groups$Label
  rownames(v_deaths) <- years

  #Change back to age_groups
  j <- 1
  for(i in 1:nrow(age_groups))
  {
    if(age_groups$Years[i] > 1)
    {
      noV_counts[i,] <- apply(noVacc_counts[j:(j+age_groups$Years[i]-1),], 2, sum)
      v_counts[i,] <- apply(vacc_counts[j:(j+age_groups$Years[i]-1),], 2, sum)
      noV_dalys[i,] <- apply(noVacc_dalys[j:(j+age_groups$Years[i]-1),], 2, sum)
      v_dalys[i,] <- apply(vacc_dalys[j:(j+age_groups$Years[i]-1),], 2, sum)
    }else{
      noV_counts[i,] <- noVacc_counts[j,]
      v_counts[i,] <- vacc_counts[j,]
      noV_dalys[i,] <- noVacc_dalys[j,]
      v_dalys[i,] <- vacc_dalys[j,]
    }

    if(!is.na(noVacc_deaths)[1])
    {
      if(age_groups$Years[i] > 1)
      {
        noV_deaths[i,] <- apply(noVacc_deaths[j:(j+age_groups$Years[i]-1),], 2, sum)
        v_deaths[i,] <- apply(vacc_deaths[j:(j+age_groups$Years[i]-1),], 2, sum)
      }else{
        noV_deaths[i,] <- noVacc_deaths[j,]
        v_deaths[i,] <- vacc_deaths[j,]
      }
    }

    j <- j+age_groups$Years[i]
  }

  browser()

  return(list(noV_counts, v_counts, noV_dalys, v_dalys, noV_deaths, v_deaths))
}

