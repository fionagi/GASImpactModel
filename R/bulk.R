# #Code to bulk run scenarios. Read in parameter values from bulk_input.xlsx,
# #run the model using user provided parameter values and output values
# #to csv output files.
#
# library(GASImpacModel)
# library(xlsx)
# library(stringr)
#
# inputDir <- "C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\GASImpactModel\\GASImpactModel\\R"
# outputDir <- "C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\IVI output\\"
# logFile <- paste(outputDir, "logFile", Sys.Date(), ".txt", sep = '')
# write(paste("Log start time: ", Sys.time()), logFile)
#
# #FIXED VALUES FOR ALL SCENARIOS
# impType <- "Year of vaccination"
# maxAge <- 99
#
# inputFile <- xlsx::read.xlsx(paste(inputDir, "bulk_input.xlsx", sep = '\\'), sheetName = "Scenarios")
#
# numScenarios <- max(inputFile$Scenario)
#
# for(s in 1:numScenarios )
# {
#   #READ IN PARAMETER VALUES FOR PARTICULAR SCENARIO
#   condition <- inputFile$Condition[s]
#   pAttr <- inputFile$PropAttr[s]
#   vAge <- inputFile$Age[s]
#   durability <- inputFile$Durability[s]
#   waning <- inputFile$Waning[s]
#   coverage <- inputFile$Coverage[s]
#   ramp <- inputFile$Ramp[s]
#   efficacy <- inputFile$Efficacy[s]
#   introYear <- inputFile$IntroYear[s]
#   projYears <- inputFile$ProjYears[s]
#
#   results_preVaxx <- matrix(NA, nrow = length(data.region$Country)*projYears*4, ncol = (maxAge+1)+5)
#   results_Vaxx <- matrix(NA, nrow = length(data.region$Country)*projYears*4, ncol = (maxAge+1)+5)
#
#   covShift <- ifelse(coverage == "Shift", TRUE, FALSE)
#   yearShift <- ifelse(introYear == "Shift", TRUE, FALSE)
#
#   imm <- ifelse(waning, "Wan", "Uni")
#   ram <- ifelse(ramp, "Ramp", '')
#   shift <- ifelse(coverage == "Shift", "Shift", '')
#   outputFileVaxx <- paste(condition, "Age", vAge, "Eff", efficacy,
#                       "Cov", coverage, "Dur", durability, imm, ram, shift, ".csv", sep = '')
#   outputFilePreVaxx <- paste(condition, "_preVaxx.csv", sep = '')
#
#   #LOOP OVER ALL COUNTRIES
#   i = 1 #result matrix index
#   j=1 # country index
#
#   print(paste("Scenario: ", s))
#   write(paste("Scenario: ", s, "\n"), logFile, append = TRUE)
#
#   for(country in data.region$Country)
#   {
#
#     #If Shift scenario used, get required values
#     if(covShift)
#     {
#       coverage <- data.coveragebycountry[which(data.coveragebycountry$Country == country),]$Coverage
#       if(is.na(coverage) || length(coverage) == 0) next
#     }
#     coverage <- as.numeric(coverage)
#
#     overallEff <- (efficacy*coverage)/100
#
#     if(yearShift)
#     {
#       introYear <- data.coveragebycountry[which(data.coveragebycountry$Country == country),]$Year.of.Introduction
#       if(is.na(introYear) || length(coverage) == 0) next
#     }
#     introYear <- as.numeric(introYear)
#
#     countryCode <-data.region[data.region$Country == country,]$Code
#
#     print(paste(j, ": Country: ", country, "Condition: ", condition,
#                 "Prop. attr: ",  pAttr, "Years: ", introYear, "-", introYear+projYears-1,
#                 "Vaxx age: ", vAge, "Durability: ", durability, "Waning", waning,
#                 "Coverage: ", coverage, "Ramp: ", ramp, "Efficacy: ", efficacy,
#                 "Shift", yearShift || covShift))
#     write(paste(j, ": Country: ", country, "Condition: ", condition,
#               "Prop. attr: ",  pAttr, "Years: ", introYear, "-", introYear+projYears-1,
#               "Vaxx age: ", vAge, "Durability: ", durability, "Waning", waning,
#               "Coverage: ", coverage, "Ramp: ", ramp, "Efficacy: ", efficacy,
#               "Shift", yearShift || covShift), logFile, append = TRUE)
#
#     if(condition == "Cellulitis" || condition == "Rheumatic Heart Disease"){
#       incR <- getConditionData(country, condition, "Rate", pAttr)[[1]]
#     }else{
#       incR <- getConditionData(country, condition, "Rate", pAttr)
#     }
#
#     mProb <- getMorData(location = country, yearV = introYear, pYears = projYears-1,
#                         ageV = vAge, maxAge = maxAge, impType = impType)
#
#     initPop <- getInitPop(location = country, yearV = introYear,
#                           pYears = projYears-1, ageV = vAge, impType = impType)
#
#     impModels <- runModel(location = country, condition = condition, inc = incR,
#                           mortality = mProb, yearV = introYear, vaccAge = vAge,
#                           maxAge = maxAge, vaccEff = overallEff, vaccDur = durability,
#                           waning = waning, impType = impType, pYears = projYears-1,
#                           initPop = initPop)
#
#     noVacc_counts <- impModels[[1]]
#     vacc_counts <- impModels[[2]]
#     noVacc_dalys <- impModels[[3]]
#     vacc_dalys <- impModels[[4]]
#     noVacc_deaths <- impModels[[5]]
#     vacc_deaths <- impModels[[6]]
#     noVacc_pop <- impModels[[7]]
#
#     #cases
#     results_preVaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "Cases",
#                                             introYear:(introYear+projYears-1),t(noVacc_counts))
#     results_Vaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "Cases",
#                                             introYear:(introYear+projYears-1),t(vacc_counts))
#     i <- i+projYears
#
#     #dalys
#     results_preVaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "DALYs",
#                                                           introYear:(introYear+projYears-1),t(noVacc_dalys))
#     results_Vaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "DALYs",
#                                                           introYear:(introYear+projYears-1),t(vacc_dalys))
#     i <- i+projYears
#
#     #deaths
#     if(!is.na(noVacc_deaths)[1])
#     {
#      results_preVaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "Deaths",
#                                                                     introYear:(introYear+projYears-1),t(noVacc_deaths))
#      results_Vaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "Deaths",
#                                                                     introYear:(introYear+projYears-1),t(vacc_deaths))
#      i <- i+projYears
#     }
#
#     #pop
#     results_preVaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "pop",
#                                                               introYear:(introYear+projYears-1),t(noVacc_pop))
#     results_Vaxx[i:(i+projYears-1), ] <- cbind(country, countryCode, condition, "pop",
#                                                               introYear:(introYear+projYears-1),t(noVacc_pop)) #same pop values
#     i <- i+projYears
#
#     j <- j+1
#   }
#
#   colnames(results_preVaxx) <- c("Location", "Code", "Condition", "Metric", "Year", as.character(0:maxAge))
#   colnames(results_Vaxx) <- c("Location", "Code", "Condition", "Metric", "Year", as.character(0:maxAge))
#
#   write.csv(results_preVaxx[complete.cases(results_preVaxx),],
#                 paste(outputDir, outputFilePreVaxx, sep = "\\"), row.names =  F )
#   write.csv(results_Vaxx[complete.cases(results_Vaxx),],
#                 paste(outputDir, outputFileVaxx, sep = "\\"), row.names = F)
# }
#
#
