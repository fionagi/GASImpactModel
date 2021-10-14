# #Code to bulk run scenarios. Eventually will take in input csv file
# #from user, run the model using user provided parameter values and
# #output all results to csv output files.
#
# #library(GASImpacModel) #R package needs to be installed and loaded
#
# #Parameter values
# #(will eventually be in csv file)
# #country <- "Australia"
#
# countryList <- unique(data.cellulitis2019$location) #Use the same set of countries as for Cellulitis
#
# condition <- "Pharyngitis"
# impType <- "Year of birth"
#
# pAttr <- 1 #proportion of incidents attributable to Strep A
#
# vAge <- 0
# durability <- 10 #10 years full coverage
# coverage <- 50 #percent
# efficacy <- 80 #percent
# overallEff <- (efficacy*coverage)/100 #as a percentage
#
# introYear <- 2022
# projYears <- 30
#
# maxAge <- 84
#
# incR <-
#
# results_preVaxx <- matrix(NA, nrow = length(unique(data.cellulitis2019$location))*projYears*4, ncol = (maxAge+1)+4)
# results_Vaxx <- matrix(NA, nrow = length(unique(data.cellulitis2019$location))*projYears*4, ncol = (maxAge+1)+4)
#
# #Loop over all countries
# i = 1 #result matrix index
# j=1
# for(country in countryList)
# {
#   print(paste(j, ": Country: ", country, "Condition: ", condition,
#               "Prop. attr Strep A: ",  pAttr, "Years: ",
#               introYear, "-", introYear+projYears-1, "Vaxx age: ", vAge,
#               "Durability: ", durability, "Coverage: ", coverage,
#               "Efficacy: ", efficacy))
#
#   incR <- getConditionData(country, condition, "Rate")[[1]]
#
#   mProb <- getMorData(location = country, yearV = introYear, pYears = projYears-1,
#                       impType = impType, ageV = vAge)
#
#   initPop <- getInitPop(location = country, yearV = introYear,
#                         pYears = projYears-1, ageV = vAge, impType = impType)
#
#   impModels <- runModelBulk(location = country, conditions = condition, inc = incR,
#                             propA = pAttr, mortality = mProb, nyears = 85,
#                             yearV = introYear, vaccAge = vAge, vaccEff = overallEff,
#                             vaccDur = durability, impType = impType,
#                             pYears = projYears-1, initPop = initPop)
#
#   #return(list(noVacc_counts, vacc_counts, noVacc_dalys, vacc_dalys,
#   #            noVacc_deaths, vacc_deaths, noVacc_pop))
#
#   noVacc_counts <- impModels[[1]]
#   vacc_counts <- impModels[[2]]
#   noVacc_dalys <- impModels[[3]]
#   vacc_dalys <- impModels[[4]]
#   noVacc_deaths <- impModels[[5]]
#   vacc_deaths <- impModels[[6]]
#   noVacc_pop <- impModels[[7]]
#
#   #cases
#   results_preVaxx[i:(i+projYears-1), ] <- cbind(country, condition, "Cases", introYear:(introYear+projYears-1),t(noVacc_counts))
#   results_Vaxx[i:(i+projYears-1), ] <- cbind(country, condition, "Cases", introYear:(introYear+projYears-1),t(vacc_counts))
#
#   #dalys
#   results_preVaxx[(i+projYears):(i+2*projYears-1), ] <- cbind(country, condition, "DALYs", introYear:(introYear+projYears-1),t(noVacc_dalys))
#   results_Vaxx[(i+projYears):(i+2*projYears-1), ] <- cbind(country, condition, "DALYs", introYear:(introYear+projYears-1),t(vacc_dalys))
#
#   #pop
#   results_preVaxx[(i+2*projYears):(i+3*projYears-1), ] <- cbind(country, condition, "pop", introYear:(introYear+projYears-1),t(noVacc_pop))
#   results_Vaxx[(i+2*projYears):(i+3*projYears-1), ] <- cbind(country, condition, "pop", introYear:(introYear+projYears-1),t(noVacc_pop)) #same pop values
#
#   i <- i+3*projYears
#   j <- j+1
# }
#
# colnames(results_preVaxx) <- c("Location", "Condition", "Metric", "Year", as.character(0:84))
# colnames(results_Vaxx) <- c("Location", "Condition", "Metric", "Year", as.character(0:84))
# write.csv(results_preVaxx[complete.cases(results_preVaxx),], "C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\IVI output\\Pharyngitis_preVaxx.csv", row.names =  F )
# write.csv(results_Vaxx[complete.cases(results_Vaxx),], "C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\IVI output\\Pharyngitis_Age0Eff80Cov50Dur10uni.csv", row.names = F)
#
# ##################################################################################################################################
# condition <- "Pharyngitis"
# impType <- "Year of birth"
#
# pAttr <- 1 #proportion of incidents attributable to Strep A
#
# vAge <- 5
# durability <- 10 #10 years full coverage
# coverage <- 50 #percent
# efficacy <- 80 #percent
# overallEff <- (efficacy*coverage)/100 #as a percentage
#
# introYear <- 2022
# projYears <- 30
#
# maxAge <- 84
#
# results_preVaxx <- matrix(NA, nrow = length(unique(data.cellulitis2019$location))*projYears*4, ncol = (maxAge+1)+4)
# results_Vaxx <- matrix(NA, nrow = length(unique(data.cellulitis2019$location))*projYears*4, ncol = (maxAge+1)+4)
#
# #Loop over all countries
# i = 1 #result matrix index
# j=1
# for(country in countryList)
# {
#   print(paste(j, ": Country: ", country, "Condition: ", condition,
#               "Prop. attr Strep A: ",  pAttr, "Years: ",
#               introYear, "-", introYear+projYears-1, "Vaxx age: ", vAge,
#               "Durability: ", durability, "Coverage: ", coverage,
#               "Efficacy: ", efficacy))
#
#   incR <- getConditionData(country, condition, "Rate")[[1]]
#
#   mProb <- getMorData(location = country, yearV = introYear, pYears = projYears-1,
#                       impType = impType, ageV = vAge)
#
#   initPop <- getInitPop(location = country, yearV = introYear,
#                         pYears = projYears-1, ageV = vAge, impType = impType)
#
#   impModels <- runModelBulk(location = country, conditions = condition, inc = incR,
#                             propA = pAttr, mortality = mProb, nyears = 85,
#                             yearV = introYear, vaccAge = vAge, vaccEff = overallEff,
#                             vaccDur = durability, impType = impType,
#                             pYears = projYears-1, initPop = initPop)
#
#   #return(list(noVacc_counts, vacc_counts, noVacc_dalys, vacc_dalys,
#   #            noVacc_deaths, vacc_deaths, noVacc_pop))
#
#   noVacc_counts <- impModels[[1]]
#   vacc_counts <- impModels[[2]]
#   noVacc_dalys <- impModels[[3]]
#   vacc_dalys <- impModels[[4]]
#   noVacc_deaths <- impModels[[5]]
#   vacc_deaths <- impModels[[6]]
#   noVacc_pop <- impModels[[7]]
#
#   #cases
#   results_preVaxx[i:(i+projYears-1), ] <- cbind(country, condition, "Cases", introYear:(introYear+projYears-1),t(noVacc_counts))
#   results_Vaxx[i:(i+projYears-1), ] <- cbind(country, condition, "Cases", introYear:(introYear+projYears-1),t(vacc_counts))
#
#   #dalys
#   results_preVaxx[(i+projYears):(i+2*projYears-1), ] <- cbind(country, condition, "DALYs", introYear:(introYear+projYears-1),t(noVacc_dalys))
#   results_Vaxx[(i+projYears):(i+2*projYears-1), ] <- cbind(country, condition, "DALYs", introYear:(introYear+projYears-1),t(vacc_dalys))
#
#   #pop
#   results_preVaxx[(i+2*projYears):(i+3*projYears-1), ] <- cbind(country, condition, "pop", introYear:(introYear+projYears-1),t(noVacc_pop))
#   results_Vaxx[(i+2*projYears):(i+3*projYears-1), ] <- cbind(country, condition, "pop", introYear:(introYear+projYears-1),t(noVacc_pop)) #same pop values
#
#   i <- i+3*projYears
#   j <- j+1
# }
#
# #colnames(results_preVaxx) <- c("Location", "Condition", "Metric", "Year", as.character(0:84))
# colnames(results_Vaxx) <- c("Location", "Condition", "Metric", "Year", as.character(0:84))
# #write.csv(results_preVaxx[complete.cases(results_preVaxx),], "C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\IVI output\\Pharyngitis_preVaxx.csv", row.names =  F )
# write.csv(results_Vaxx[complete.cases(results_Vaxx),], "C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\IVI output\\Pharyngitis_Age5Eff80Cov50Dur10uni.csv", row.names = F)
#
