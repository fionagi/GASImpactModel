#Code to bulk run scenarios. Read in parameter values from bulk_input.xlsx,
#run the model using user provided parameter values and output values
#to csv output files.

library(GASImpactModel)
library(xlsx)
library(stringr)

inputDir <- "C:\\Users\\Jeff\\Documents\\GitHub\\GASImpactModel\\R"
outputDir <- "C:\\Users\\Jeff\\Documents\\GitHub\\GASImpactModel\\IVI output\\"
logFile <- paste(outputDir, "logFile", Sys.Date(), ".txt", sep = '')
write(paste("Log start time: ", Sys.time()), logFile)

#FIXED VALUES FOR ALL SCENARIOS
impType <- "Year of vaccination"
maxAge <- 99

inputFile <- xlsx::read.xlsx(paste(inputDir, "bulk_input_arf.xlsx", sep = '\\'), sheetName = "Scenarios")

numScenarios <- max(inputFile$Scenario)

for(s in 1:numScenarios )
{
  #READ IN PARAMETER VALUES FOR PARTICULAR SCENARIO
  condition <- inputFile$Condition[s]
  pAttr <- inputFile$PropAttr[s]
  vAge <- inputFile$Age[s]
  durability <- inputFile$Durability[s]
  waning <- inputFile$Waning[s]
  coverage <- inputFile$Coverage[s]
  ramp <- inputFile$Ramp[s]
  efficacy <- inputFile$Efficacy[s]
  introYear <- inputFile$IntroYear[s]
  projYears <- inputFile$ProjYears[s]

  results_preVaxx <- matrix(NA, nrow = length(data.region$Country)*projYears*ifelse(condition=="Acute Rheumatic Fever",7,6), ncol = (maxAge+1-vAge)+6)
  results_Vaxx <- matrix(NA, nrow = length(data.region$Country)*projYears*ifelse(condition=="Acute Rheumatic Fever",7,6), ncol = (maxAge+1-vAge)+6)
  results_averted <- matrix(NA, nrow = length(data.region$Country)*projYears*ifelse(condition=="Acute Rheumatic Fever",7,6), ncol = (maxAge+1-vAge)+6)

  covShift <- ifelse(coverage == "Shift", TRUE, FALSE)
  yearShift <- ifelse(introYear == "Shift", TRUE, FALSE)

  imm <- ifelse(waning, "Wan", "Uni")
  ram <- ifelse(ramp, "Ramp", '')
  shiftC <- ifelse(covShift, "SHCov", '')
  shiftY <- ifelse(yearShift, "SHYear", '')
  outputFileVaxx <- paste(condition, "Age", vAge, "Eff", efficacy,
                      "Cov", coverage, "Dur", durability, imm, ram, shiftC, shiftY, ".csv", sep = '')
  outputFileVaxxAverted <- paste(condition, "Age", vAge, "Eff", efficacy,
                          "Cov", coverage, "Dur", durability, imm, ram, shiftC, shiftY, "_averted.csv", sep = '')
  outputFilePreVaxx <- paste(condition, "Age", vAge, shiftY, "_preVaxx.csv", sep = '')

  #LOOP OVER ALL COUNTRIES
  i = 1 #result matrix index
  j=1 # country index

  print(paste("Scenario: ", s))
  write(paste("Scenario: ", s, "\n"), logFile, append = TRUE)

  for(country in data.region$Country)
  {

    #If Shift scenario used, get required values
    if(covShift)
    {
      coverage <- data.coveragebycountry[which(data.coveragebycountry$Country == country),]$Hib3.Coverage
      if(is.na(coverage) || length(coverage) == 0) coverage <- data.coveragebycountry[which(data.coveragebycountry$Country == country),]$DTP3.Coverage
      if(is.na(coverage) || length(coverage) == 0) {
        print(paste("No country", country, "in Shift Health table"))
        next
      }
    }
    coverage <- as.numeric(coverage)

    overallEff <- (efficacy*coverage)/100

    if(yearShift)
    {
      introYear <- data.coveragebycountry[which(data.coveragebycountry$Country == country),]$Timing.of.Introduction
      if(is.na(introYear) || length(introYear) == 0) {
        print(paste("No country", country, "in Shift Health table"))
        next
      }
    }
    introYear <- as.numeric(introYear)

    countryCode <-data.region[data.region$Country == country,]$Code

    print(paste(j, ": Country: ", country, "Condition: ", condition,
                "Prop. attr: ",  pAttr, "Years: ", introYear, "-", introYear+projYears-1,
                "Vaxx age: ", vAge, "Durability: ", durability, "Waning", waning,
                "Coverage: ", coverage, "Ramp: ", ramp, "Efficacy: ", efficacy,
                "Shift", yearShift || covShift))
    write(paste(j, ": Country: ", country, "Condition: ", condition,
              "Prop. attr: ",  pAttr, "Years: ", introYear, "-", introYear+projYears-1,
              "Vaxx age: ", vAge, "Durability: ", durability, "Waning", waning,
              "Coverage: ", coverage, "Ramp: ", ramp, "Efficacy: ", efficacy,
              "Shift", yearShift || covShift), logFile, append = TRUE)

    if(condition %in% c("Cellulitis", "Rheumatic Heart Disease")){
      incR <- getConditionData(country, condition, "Rate", pAttr)[[1]]
      rate <- 100000
    }else{
      incR <- getConditionData(country, condition, "Rate", pAttr)
       rate <- ifelse(condition=="Acute Rheumatic Fever", 100000, 1)
    }

    mProb <- getMorData(location = country, yearV = introYear, pYears = projYears-1,
                        ageV = vAge, maxAge = maxAge, impType = impType)

    initPop <- getInitPop(location = country, yearV = introYear,
                          pYears = projYears-1, ageV = vAge, birth = FALSE)

    impModels <- runModel(location = country, condition = condition, inc = incR,
                           rate = rate, mortality = mProb, yearV = introYear,
                           vaccAge = vAge, maxAge = maxAge, vaccEff = overallEff,
                           vaccDur = durability, waning = waning, ramp = ramp,
                           impType = impType, pYears = projYears-1,
                           initPop = initPop)

    noVacc_counts <- impModels[[1]]
    vacc_counts <- impModels[[2]]
    noVacc_dalys <- impModels[[3]]
    vacc_dalys <- impModels[[4]]
    noVacc_deaths <- impModels[[5]]
    vacc_deaths <- impModels[[6]]
    noVacc_pop <- impModels[[7]]
    noVacc_yll <- impModels[[8]]
    vacc_yll <- impModels[[9]]
    noVacc_yld <- impModels[[10]]
    vacc_yld <- impModels[[11]]

    #cases
    results_preVaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "Cases",
                                            introYear:(introYear+projYears-1),
                                            (introYear-vAge):(introYear-vAge+projYears-1),
                                            t(noVacc_counts))
    results_Vaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "Cases",
                                               introYear:(introYear+projYears-1),
                                               (introYear-vAge):(introYear-vAge+projYears-1),t(vacc_counts))
    results_averted[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "Cases",
                                                  introYear:(introYear+projYears-1),
                                                  (introYear-vAge):(introYear-vAge+projYears-1),
                                               t(noVacc_counts)-t(vacc_counts))
    i <- i+projYears

    #dalys
    results_preVaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "DALYs",
                                                   introYear:(introYear+projYears-1),
                                                   (introYear-vAge):(introYear-vAge+projYears-1),
                                                   t(noVacc_dalys))
    results_Vaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "DALYs",
                                                   introYear:(introYear+projYears-1),
                                                   (introYear-vAge):(introYear-vAge+projYears-1),
                                                   t(vacc_dalys))
    results_averted[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "DALYs",
                                                   introYear:(introYear+projYears-1),
                                                   (introYear-vAge):(introYear-vAge+projYears-1),
                                                  t(noVacc_dalys)-t(vacc_dalys))
    i <- i+projYears

    #deaths
    if(!is.na(noVacc_deaths)[1])
    {
     results_preVaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "Deaths",
                                                    introYear:(introYear+projYears-1),
                                                    (introYear-vAge):(introYear-vAge+projYears-1),
                                                    t(noVacc_deaths))
     results_Vaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "Deaths",
                                                  introYear:(introYear+projYears-1),
                                                 (introYear-vAge):(introYear-vAge+projYears-1),
                                                 t(vacc_deaths))
     results_averted[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "Deaths",
                                                introYear:(introYear+projYears-1),
                                                (introYear-vAge):(introYear-vAge+projYears-1),
                                                t(noVacc_deaths)-t(vacc_deaths))
     i <- i+projYears
    }

    #yll
    results_preVaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "YLL",
                                                  introYear:(introYear+projYears-1),
                                                  (introYear-vAge):(introYear-vAge+projYears-1),
                                                  t(noVacc_yll))
    results_Vaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "YLL",
                                               introYear:(introYear+projYears-1),
                                               (introYear-vAge):(introYear-vAge+projYears-1),
                                               t(vacc_yll))
    results_averted[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "YLL",
                                                  introYear:(introYear+projYears-1),
                                                  (introYear-vAge):(introYear-vAge+projYears-1),
                                                  t(noVacc_yll)-t(vacc_yll))
    i <- i+projYears

    #yld
    results_preVaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "YLD",
                                                  introYear:(introYear+projYears-1),
                                                  (introYear-vAge):(introYear-vAge+projYears-1),
                                                  t(noVacc_yld))
    results_Vaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "YLD",
                                               introYear:(introYear+projYears-1),
                                               (introYear-vAge):(introYear-vAge+projYears-1),
                                               t(vacc_yld))
    results_averted[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "YLD",
                                                  introYear:(introYear+projYears-1),
                                                  (introYear-vAge):(introYear-vAge+projYears-1),
                                                  t(noVacc_yld)-t(vacc_yld))
    i <- i+projYears


    #pop
    results_preVaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "pop",
                                                   introYear:(introYear+projYears-1),
                                                   (introYear-vAge):(introYear-vAge+projYears-1),
                                                   t(noVacc_pop))
    results_Vaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "pop",
                                                   introYear:(introYear+projYears-1),
                                                   (introYear-vAge):(introYear-vAge+projYears-1),
                                                   t(noVacc_pop)) #same pop values
    i <- i+projYears

    #ARF progression to RHD from end of vaccine efficacy
    if(condition=="Acute Rheumatic Fever")
    {
      ageEndVaxEff <- vAge+durability
      for(a1 in i:(i+projYears-1))
      {
        results_Vaxx[a1,] <- c(country, countryCode, condition, "RHD cases progressing from ARF",
                                  introYear+(a1-i),
                                  (introYear-vAge)+(a1-i),
                                  rep(0,ageEndVaxEff),
                                  t(vacc_counts)[a1-i+1,(ageEndVaxEff-8):ageEndVaxEff]%*%t(as.matrix(ARFprog[,-1])),
                                  rep(0,(maxAge+1)-(ageEndVaxEff+9)))

        results_averted[a1,] <- c(country, countryCode, condition, "RHD cases progressing from ARF",
                               introYear+(a1-i),
                               (introYear-vAge)+(a1-i),
                               rep(0,ageEndVaxEff),
                               (t(noVacc_counts)-t(vacc_counts))[a1-i+1,(ageEndVaxEff-8):ageEndVaxEff]%*%t(as.matrix(ARFprog[,-1])),
                               rep(0,(maxAge+1)-(ageEndVaxEff+9)))


      }

      i <- i+projYears

    }

    j <- j+1
  }

  colnames(results_preVaxx) <- c("Location", "Code", "Condition", "Metric", "Vaccination year", "Birth year", as.character(vAge:maxAge))
  colnames(results_Vaxx) <- c("Location", "Code", "Condition", "Metric", "Vaccination year", "Birth year", as.character(vAge:maxAge))
  colnames(results_averted) <- c("Location", "Code", "Condition", "Metric", "Vaccination year", "Birth year", as.character(vAge:maxAge))

  write.csv(results_preVaxx[complete.cases(results_preVaxx),],
                paste(outputDir, outputFilePreVaxx, sep = "\\"), row.names =  F )
  write.csv(results_Vaxx[complete.cases(results_Vaxx),],
                paste(outputDir, outputFileVaxx, sep = "\\"), row.names = F)
  write.csv(results_averted[complete.cases(results_averted),],
            paste(outputDir, outputFileVaxxAverted, sep = "\\"), row.names = F)
}


