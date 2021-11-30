# library(RColorBrewer)
# ##################################################################################################
# #CHAPTER 8
# colP <- brewer.pal(4,"Set1")
#
# #Cellulitis all ages incidence rates
# code <- rep(NA, nrow(data.cell.combined))
# income <- rep(NA, nrow(data.cell.combined))
#
# for(i in 1:nrow(data.cell.combined)){
#   if(data.cell.combined[i,]$location %in% data.region$Country)
#   {
#     code[i] <- data.region[data.region$Country == data.cell.combined[i,]$location,]$Code
#     income[i] <- data.region[data.region$Country == data.cell.combined[i,]$location,]$IncomeGroup
#   }else{
#     code[i] <- NA
#   }
# }
#
# cell_comb <- as.data.frame(cbind(code, income, data.cell.combined[, "val"]))
# cell_comb <- cell_comb[-which(is.na(cell_comb[,"code"])),]
# colnames(cell_comb) <- c("code", "income", "val")
# cell_comb <- cell_comb[sort(as.numeric(cell_comb$val), decreasing = TRUE, index.return = TRUE)$ix,]
#
# cell_comb$income <- factor(cell_comb$income, levels = c("High income",
#                               "Upper middle income", "Lower middle income", "Low income"))
#
# cell_comb1<- cell_comb[1:62,]
# cell_comb2<- cell_comb[63:124,]
# cell_comb3<- cell_comb[125:186,]
#
#
# cell_comb1$code <- factor(cell_comb1$code, levels = cell_comb1$code[sort(as.numeric(cell_comb1$val), decreasing = TRUE, index.return = TRUE)$ix])
# cell_comb2$code <- factor(cell_comb2$code, levels = cell_comb2$code[sort(as.numeric(cell_comb2$val), decreasing = TRUE, index.return = TRUE)$ix])
# cell_comb3$code <- factor(cell_comb3$code, levels = cell_comb3$code[sort(as.numeric(cell_comb3$val), decreasing = TRUE, index.return = TRUE)$ix])
#
#
# ggplot2::ggplot(data = as.data.frame(cell_comb1), ggplot2::aes(x=code, y=as.numeric(val), fill = income)) +
#   ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                     stat="identity")+
#   ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("Country (ISO code)")+
#   ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP[1:3])
#
#
# ggplot2::ggplot(data = as.data.frame(cell_comb2), ggplot2::aes(x=code, y=as.numeric(val), fill = income)) +
#   ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                     stat="identity")+
#   ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("Country (ISO code)")+
#   ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)
#
# ggplot2::ggplot(data = as.data.frame(cell_comb3), ggplot2::aes(x=code, y=as.numeric(val), fill = income)) +
#   ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                     stat="identity")+
#   ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("Country (ISO code)")+
#   ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)
#
#
# ###############################################################################
# #Global heat map
# library(maps)
# library(ggplot2)
#
# cell_data <- data.cell.combined
# rhd_data <- data.rhd.combined
#
# world_map <- map_data("world")
# world_map <- subset(world_map, region != "Antarctica")
#
#
# for(country in data.cell.combined$location)
# {
#   if(!(country %in% unique(world_map$region)))
#     print(country)
# }
#
# world_map$region[which(world_map$region == "Czech Republic")] <-  "Czechia"
# world_map$region[which(world_map$region == "Moldova")] <- "Republic of Moldova"
# world_map$region[which(world_map$region == "UK")] <- "United Kingdom"
# # "Côte d'Ivoire"
# world_map$region[which(world_map$region == "Trinidad")] <-  "Trinidad and Tobago"
# world_map$region[which(world_map$region == "Virgin Islands")] <-  "United States Virgin Islands"
# world_map$region[which(world_map$region == "Saint Vincent")] <-  "Saint Vincent and the Grenadines"
# # "Global"
# world_map$region[which(world_map$region == "North Korea")] <- "Dem. People's Republic of Korea"
# world_map$region[which(world_map$region == "Brunei")] <- "Brunei Darussalam"
# world_map$region[which(world_map$region == "Vietnam")] <-  "Viet Nam"
# world_map$region[which(world_map$region == "South Korea")] <-  "Republic of Korea"
# world_map$region[which(world_map$region == "Antigua")] <-  "Antigua and Barbuda"
# world_map$region[which(world_map$region == "Cape Verde")] <- "Cabo Verde"
# world_map$region[which(world_map$region == "Democratic Republic of the Congo")] <-  "Congo"
# world_map$region[which(world_map$region == "Bolivia")] <-  "Bolivia (Plurinational State of)"
# world_map$region[which(world_map$region == "Venezuela")] <-  "Venezuela (Bolivarian Republic of)"
# world_map$region[which(world_map$region == "Swaziland")] <-  "Eswatini"
# world_map$region[which(world_map$region == "Taiwan")] <-  "Taiwan (Province of China)"
# world_map$region[which(world_map$region == "Syria")] <- "Syrian Arab Republic"
# world_map$region[which(world_map$region == "Saint Kitts")] <-  "Saint Kitts and Nevis"
# world_map$region[which(world_map$region == "Tanzania")] <-  "United Republic of Tanzania"
# world_map$region[which(world_map$region == "Iran")] <-  "Iran (Islamic Republic of)"
# world_map$region[which(world_map$region == "USA")] <-  "United States of America"
# world_map$region[which(world_map$region == "Palestine")] <-  "State of Palestine"
# # "Tokelau"
# world_map$region[which(world_map$region == "Laos")] <-  "Lao People's Democratic Republic"
# # "Tuvalu"
# world_map$region[which(world_map$region == "Russia")] <-  "Russian Federation"
#
#
# mydata_cell <- data.cell.combined[-which(data.cell.combined$location %in% c("Côte d'Ivoire", "Global", "Tokelau", "Tuvalu")),]
# colnames(mydata_cell) <- c("location", "Incidence (per 100k)")
#
# ggplot(mydata_cell) +
#   geom_map(
#     dat = world_map, map = world_map, aes(map_id = region),
#     fill = "white", size = 0.25
#   ) +
#   geom_map(map = world_map, aes(map_id = location, fill = `Incidence (per 100k)`), size = 0.25) +
#   expand_limits(x = world_map$long, y = world_map$lat)
#
#
# mydata_rhd <- data.rhd.combined[-which(data.rhd.combined$location %in% c("Côte d'Ivoire", "Global", "Tokelau", "Tuvalu")),]
# colnames(mydata_rhd) <- c("location", "Incidence (per 100k)")
#
# ggplot(mydata_rhd) +
#   geom_map(
#     dat = world_map, map = world_map, aes(map_id = region),
#     fill = "white", size = 0.25
#   ) +
#   geom_map(map = world_map, aes(map_id = location, fill = `Incidence (per 100k)`), size = 0.25) +
#   expand_limits(x = world_map$long, y = world_map$lat)
#
# #################################################################################
#
# #RHD all ages incidence rates
# code <- rep(NA, nrow(data.rhd.combined))
# income <- rep(NA, nrow(data.rhd.combined))
#
# for(i in 1:nrow(data.rhd.combined)){
#   if(data.rhd.combined[i,]$location %in% data.region$Country)
#   {
#     code[i] <- data.region[data.region$Country == data.rhd.combined[i,]$location,]$Code
#     income[i] <- data.region[data.region$Country == data.rhd.combined[i,]$location,]$IncomeGroup
#   }else{
#     code[i] <- NA
#   }
# }
#
# rhd_comb <- as.data.frame(cbind(code, income, data.rhd.combined[, "val"]))
# rhd_comb <- rhd_comb[-which(is.na(rhd_comb[,"code"])),]
# colnames(rhd_comb) <- c("code", "income", "val")
# rhd_comb <- rhd_comb[sort(as.numeric(rhd_comb$val), decreasing = TRUE, index.return = TRUE)$ix,]
#
# rhd_comb$income <- factor(rhd_comb$income, levels = c("High income",
#                                                         "Upper middle income", "Lower middle income", "Low income"))
#
# rhd_comb1<- rhd_comb[1:62,]
# rhd_comb2<- rhd_comb[63:124,]
# rhd_comb3<- rhd_comb[125:186,]
#
#
# rhd_comb1$code <- factor(rhd_comb1$code, levels = rhd_comb1$code[sort(as.numeric(rhd_comb1$val), decreasing = TRUE, index.return = TRUE)$ix])
# rhd_comb2$code <- factor(rhd_comb2$code, levels = rhd_comb2$code[sort(as.numeric(rhd_comb2$val), decreasing = TRUE, index.return = TRUE)$ix])
# rhd_comb3$code <- factor(rhd_comb3$code, levels = rhd_comb3$code[sort(as.numeric(rhd_comb3$val), decreasing = TRUE, index.return = TRUE)$ix])
#
#
# ggplot2::ggplot(data = as.data.frame(rhd_comb1), ggplot2::aes(x=code, y=as.numeric(val), fill = income)) +
#   ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                     stat="identity")+
#   ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("Country (ISO code)")+
#   ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP[2:4])
#
#
# ggplot2::ggplot(data = as.data.frame(rhd_comb2), ggplot2::aes(x=code, y=as.numeric(val), fill = income)) +
#   ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                     stat="identity")+
#   ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("Country (ISO code)")+
#   ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)
#
# ggplot2::ggplot(data = as.data.frame(rhd_comb3), ggplot2::aes(x=code, y=as.numeric(val), fill = income)) +
#   ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                     stat="identity")+
#   ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("Country (ISO code)")+
#   ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)
#
# ##########################################################################################
# #CHAPTER 9
# setwd("C:\\Users\\FGiannini\\OneDrive\\TKI 2021\\IVI output\\")
#
# scenarios <- c("Scenario 1 - SHw10", "Scenario 2 - SHw20",
#                "Scenario 3 - HSPHw10", "Scenario 4 - HSPHw20",
#                "Scenario 5 - SH.HSPHw10", "Scenario 6 - SH.HSPHw20")
# condition <- c("Cellulitis", "Pharyngitis", "Impetigo", "Invasive infection",
#                "Rheumatic heart disease")
# age <- c("Age0", "Age5")
#
# income_table <- matrix(NA, nrow = 4, ncol = 36 )
# rownames(income_table) <- c("High income", "Upper middle income",
#                                 "Lower middle income", "Low income")
# colnames(income_table) <- c(paste(rep("CasesAvertedAge0", 6), 1:6, sep='_'),
#                                 paste(rep("DALYsAvertedAge0", 6), 1:6, sep='_'),
#                                 paste(rep("DeathsAvertedAge0", 6), 1:6, sep='_'),
#                                 paste(rep("CasesAvertedAge5", 6), 1:6, sep='_'),
#                                 paste(rep("DALYsAvertedAge5", 6), 1:6, sep='_'),
#                                 paste(rep("DeathsAvertedAge5", 6), 1:6, sep='_'))
#
# plotFlag <- 0
#
# for(c in condition)
# {
#  for(s in scenarios)
#  {
#   for(a in age)
#   {
#     all_files <- list.files(path = paste(c, "\\", s, "\\", sep =''))
#     file <- all_files[grepl(paste("*", a, ".*_averted.csv", sep=''), all_files)]
#     avert_output <- read.csv(paste(paste(c, "\\", s, "\\", file, sep ='')))
#
#     counts <- avert_output[avert_output$Metric == "Cases", ]
#
#     if("Deaths" %in% avert_output$Metric)
#     {
#       deaths <- avert_output[avert_output$Metric == "Deaths", ]
#     }
#
#    dalys <- avert_output[avert_output$Metric == "DALYs", ]
#
#    #Make tables
#    #Country, Country code, Vaccination Year, Cases averted, Deaths averted, DALYs averted
#    if("Deaths" %in% avert_output$Metric){
#      table_long <- cbind(counts$Location, counts$Code, counts$Vaccination.year,
#                          apply(counts[,7:ncol(counts)], 1, sum),
#                          apply(dalys[,7:ncol(dalys)],1, sum),
#                          apply(deaths[,7:ncol(deaths)], 1, sum))
#      colnames(table_long) <- c("Country", "Code", "Year",
#                                "Cases averted", "DALYs averted", "Deaths averted")
#
#      table <- matrix(NA, nrow = length(unique(avert_output$Location)), ncol = 6)
#      for(i in 1:length(unique(avert_output$Location)))
#      {
#        country <- unique(avert_output$Location)[i]
#        code <- unique(avert_output$Code)[i]
#        income <- data.region[data.region$Code == code, ]$IncomeGroup
#        table[i, ] <- cbind(country, code, income,
#                            sum(as.numeric(table_long[which(table_long[, "Country"] == country), "Cases averted"])),
#                            sum(as.numeric(table_long[which(table_long[, "Country"] == country), "DALYs averted"])),
#                            sum(as.numeric(table_long[which(table_long[, "Country"] == country), "Deaths averted"])))
#      }
#      colnames(table) <-c("Country", "Code", "Income", "Cases averted", "DALYs averted", "Deaths averted")
#
#    }else{
#      table_long <- cbind(counts$Location, counts$Code, counts$Vaccination.year,
#                          apply(counts[,7:ncol(counts)], 1, sum),
#                          apply(dalys[,7:ncol(dalys)],1, sum))
#      colnames(table_long) <- c("Country", "Code", "Year", "Cases averted", "DALYs averted" )
#
#      table <- matrix(NA, nrow = length(unique(avert_output$Location)), ncol = 5)
#      for(i in 1:length(unique(avert_output$Location)))
#      {
#        country <- unique(avert_output$Location)[i]
#        code <- unique(avert_output$Code)[i]
#        income <- data.region[data.region$Code == code, ]$IncomeGroup
#        table[i, ] <- cbind(country, code, income,
#                      sum(as.numeric(table_long[which(table_long[, "Country"] == country), "Cases averted"])),
#                      sum(as.numeric(table_long[which(table_long[, "Country"] == country), "DALYs averted"])))
#      }
#      colnames(table) <-c("Country", "Code", "Income", "Cases averted", "DALYs averted")
#    }
#
#   write.csv(table, paste("Output\\", c, a, s, "_table.csv", sep=''))
#
#   table <- as.data.frame(table)
#   #Cases
#   income_table["High income", paste("CasesAverted", a, "_", which(scenarios == s), sep= '')] <-
#               sum(as.numeric(table[table$Income == "High income",]$`Cases averted`))
#   income_table["Upper middle income", paste("CasesAverted", a, "_", which(scenarios == s), sep= '')] <-
#               sum(as.numeric(table[table$Income == "Upper middle income",]$`Cases averted`))
#   income_table["Lower middle income", paste("CasesAverted", a, "_", which(scenarios == s), sep= '')] <-
#               sum(as.numeric(table[table$Income == "Lower middle income",]$`Cases averted`))
#   income_table["Low income", paste("CasesAverted", a, "_", which(scenarios == s), sep= '')] <-
#               sum(as.numeric(table[table$Income == "Low income",]$`Cases averted`))
#   #DALYs
#   income_table["High income", paste("DALYsAverted", a, "_", which(scenarios == s), sep= '')] <-
#               sum(as.numeric(table[table$Income == "High income",]$`DALYs averted`))
#   income_table["Upper middle income", paste("DALYsAverted", a, "_", which(scenarios == s), sep= '')] <-
#               sum(as.numeric(table[table$Income == "Upper middle income",]$`DALYs averted`))
#   income_table["Lower middle income", paste("DALYsAverted", a, "_", which(scenarios == s), sep= '')] <-
#               sum(as.numeric(table[table$Income == "Lower middle income",]$`DALYs averted`))
#   income_table["Low income", paste("DALYsAverted", a, "_", which(scenarios == s), sep= '')] <-
#               sum(as.numeric(table[table$Income == "Low income",]$`DALYs averted`))
#   #Deaths
#   if("Deaths" %in% avert_output$Metric){
#     income_table["High income", paste("DeathsAverted", a, "_", which(scenarios == s), sep= '')] <-
#         sum(as.numeric(table[table$Income == "High income",]$`Deaths averted`))
#     income_table["Upper middle income", paste("DeathsAverted", a, "_", which(scenarios == s), sep= '')] <-
#         sum(as.numeric(table[table$Income == "Upper middle income",]$`Deaths averted`))
#     income_table["Lower middle income", paste("DeathsAverted", a, "_", which(scenarios == s), sep= '')] <-
#         sum(as.numeric(table[table$Income == "Lower middle income",]$`Deaths averted`))
#     income_table["Low income", paste("DeathsAverted", a, "_", which(scenarios == s), sep= '')] <-
#         sum(as.numeric(table[table$Income == "Low income",]$`Deaths averted`))
#   }
#
#
#   if(plotFlag ==1)
#   {
#    #Make plots
#    fileStart <- paste("Output\\", c, a, s, sep='')
#    #Counts
#    counts_plot <- table[, c("Code", "Cases averted")]
#    counts_plot <- counts_plot[sort(as.numeric(counts_plot[,"Cases averted"]), decreasing = TRUE, index.return = TRUE)$ix,]
#    counts_plot <- as.data.frame(counts_plot)
#
#    if(nrow(counts_plot) == 186)
#    {
#     counts_p1<- counts_plot[1:62,]
#     counts_p2<- counts_plot[63:124,]
#     counts_p3<- counts_plot[125:186,]
#    }else{
#     counts_p1<- counts_plot[1:60,]
#     counts_p2<- counts_plot[61:120,]
#     counts_p3<- counts_plot[121:181,]
#    }
#
#    counts_p1$Code <- factor(counts_p1$Code, levels = counts_p1$Code[sort(as.numeric(counts_p1$`Cases averted`), decreasing = TRUE, index.return = TRUE)$ix])
#    counts_p2$Code <- factor(counts_p2$Code, levels = counts_p2$Code[sort(as.numeric(counts_p2$`Cases averted`), decreasing = TRUE, index.return = TRUE)$ix])
#    counts_p3$Code <- factor(counts_p3$Code, levels = counts_p3$Code[sort(as.numeric(counts_p3$`Cases averted`), decreasing = TRUE, index.return = TRUE)$ix])
#
#    p1 <- ggplot2::ggplot(data = counts_p1, ggplot2::aes(x=Code, y=as.numeric(`Cases averted`))) +
#     ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                       stat="identity", fill = "steelblue")+
#     ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#     ggplot2::xlab("Country (ISO code)")+
#     ggplot2::ylab("Incidents averted")
#    ggplot2::ggsave(filename = paste(fileStart, "_countsp1.jpg", sep = ""), plot = p1, height = 6, width = 9.5)
#
#    p2 <- ggplot2::ggplot(data = counts_p2, ggplot2::aes(x=Code, y=as.numeric(`Cases averted`))) +
#     ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                       stat="identity", fill = "steelblue")+
#     ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#     ggplot2::xlab("Country (ISO code)")+
#     ggplot2::ylab("Incidents averted")
#    ggplot2::ggsave(filename = paste(fileStart, "_countsp2.jpg", sep = ""), plot = p2, height = 6, width = 9.5)
#
#    p3 <- ggplot2::ggplot(data = counts_p3, ggplot2::aes(x=Code, y=as.numeric(`Cases averted`))) +
#     ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                       stat="identity", fill = "steelblue")+
#     ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#     ggplot2::xlab("Country (ISO code)")+
#     ggplot2::ylab("Incidents averted")
#    ggplot2::ggsave(filename = paste(fileStart, "_countsp3.jpg", sep = ""), plot = p3, height = 6, width = 9.5)
#
#    #DALYs
#    dalys_plot <- table[, c("Code", "DALYs averted")]
#    dalys_plot <- dalys_plot[sort(as.numeric(dalys_plot[,"DALYs averted"]), decreasing = TRUE, index.return = TRUE)$ix,]
#    dalys_plot <- as.data.frame(dalys_plot)
#
#    if(nrow(dalys_plot) == 186)
#    {
#     dalys_p1<- dalys_plot[1:62,]
#     dalys_p2<- dalys_plot[63:124,]
#     dalys_p3<- dalys_plot[125:186,]
#    }else{
#     dalys_p1<- dalys_plot[1:60,]
#     dalys_p2<- dalys_plot[61:120,]
#     dalys_p3<- dalys_plot[121:181,]
#    }
#
#    dalys_p1$Code <- factor(dalys_p1$Code, levels = dalys_p1$Code[sort(as.numeric(dalys_p1$`DALYs averted`), decreasing = TRUE, index.return = TRUE)$ix])
#    dalys_p2$Code <- factor(dalys_p2$Code, levels = dalys_p2$Code[sort(as.numeric(dalys_p2$`DALYs averted`), decreasing = TRUE, index.return = TRUE)$ix])
#    dalys_p3$Code <- factor(dalys_p3$Code, levels = dalys_p3$Code[sort(as.numeric(dalys_p3$`DALYs averted`), decreasing = TRUE, index.return = TRUE)$ix])
#
#    p1 <- ggplot2::ggplot(data = dalys_p1, ggplot2::aes(x=Code, y=as.numeric(`DALYs averted`))) +
#     ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                       stat="identity", fill = "steelblue")+
#     ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#     ggplot2::xlab("Country (ISO code)")+
#     ggplot2::ylab("DALYs averted")
#    ggplot2::ggsave(filename = paste(fileStart, "_dalysp1.jpg", sep = ""), plot = p1, height = 6, width = 9.5)
#
#    p2 <- ggplot2::ggplot(data = dalys_p2, ggplot2::aes(x=Code, y=as.numeric(`DALYs averted`))) +
#     ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                       stat="identity", fill = "steelblue")+
#     ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#     ggplot2::xlab("Country (ISO code)")+
#     ggplot2::ylab("DALYs averted")
#    ggplot2::ggsave(filename = paste(fileStart, "_dalysp2.jpg", sep = ""), plot = p2, height = 6, width = 9.5)
#
#    p3 <- ggplot2::ggplot(data = dalys_p3, ggplot2::aes(x=Code, y=as.numeric(`DALYs averted`))) +
#     ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                       stat="identity", fill = "steelblue")+
#     ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#     ggplot2::xlab("Country (ISO code)")+
#     ggplot2::ylab("DALYs averted")
#    ggplot2::ggsave(filename = paste(fileStart, "_dalysp3.jpg", sep = ""), plot = p3, height = 6, width = 9.5)
#
#    #Deaths
#    if("Deaths" %in% avert_output$Metric){
#     deaths_plot <- table[, c("Code", "Deaths averted")]
#     deaths_plot <- deaths_plot[sort(as.numeric(deaths_plot[,"Deaths averted"]), decreasing = TRUE, index.return = TRUE)$ix,]
#     deaths_plot <- as.data.frame(deaths_plot)
#
#     if(nrow(deaths_plot) == 186)
#     {
#      deaths_p1<- deaths_plot[1:62,]
#      deaths_p2<- deaths_plot[63:124,]
#      deaths_p3<- deaths_plot[125:186,]
#     }else{
#      deaths_p1<- deaths_plot[1:60,]
#      deaths_p2<- deaths_plot[61:120,]
#      deaths_p3<- deaths_plot[121:181,]
#     }
#
#     deaths_p1$Code <- factor(deaths_p1$Code, levels = deaths_p1$Code[sort(as.numeric(deaths_p1$`Deaths averted`), decreasing = TRUE, index.return = TRUE)$ix])
#     deaths_p2$Code <- factor(deaths_p2$Code, levels = deaths_p2$Code[sort(as.numeric(deaths_p2$`Deaths averted`), decreasing = TRUE, index.return = TRUE)$ix])
#     deaths_p3$Code <- factor(deaths_p3$Code, levels = deaths_p3$Code[sort(as.numeric(deaths_p3$`Deaths averted`), decreasing = TRUE, index.return = TRUE)$ix])
#
#     p1 <- ggplot2::ggplot(data = deaths_p1, ggplot2::aes(x=Code, y=as.numeric(`Deaths averted`))) +
#      ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                       stat="identity", fill = "steelblue")+
#      ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#      ggplot2::xlab("Country (ISO code)")+
#      ggplot2::ylab("Deaths averted")
#     ggplot2::ggsave(filename = paste(fileStart, "_deathsp1.jpg", sep = ""), plot = p1, height = 6, width = 9.5)
#
#     p2 <- ggplot2::ggplot(data = deaths_p2, ggplot2::aes(x=Code, y=as.numeric(`Deaths averted`))) +
#      ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                       stat="identity", fill = "steelblue")+
#      ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#      ggplot2::xlab("Country (ISO code)")+
#      ggplot2::ylab("Deaths averted")
#     ggplot2::ggsave(filename = paste(fileStart, "_deathsp2.jpg", sep = ""), plot = p2, height = 6, width = 9.5)
#
#     p3 <- ggplot2::ggplot(data = deaths_p3, ggplot2::aes(x=Code, y=as.numeric(`Deaths averted`))) +
#      ggplot2::geom_bar(position=ggplot2::position_dodge(),
#                       stat="identity", fill = "steelblue")+
#      ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))+
#      ggplot2::xlab("Country (ISO code)")+
#      ggplot2::ylab("Deaths averted")
#      ggplot2::ggsave(filename = paste(fileStart, "_deathsp3.jpg", sep = ""), plot = p3, height = 6, width = 9.5)
#     }#if deaths
#    }#if plot
#
#   }#end a
#  }#end s
#   assign(paste(c, "_income_table", sep = ''), value = income_table)
# }#end c
#
# #Income plots
# colP <- brewer.pal(6,"Pastel1")
#
# ##################################################################################################
# #Cellulitis
# #Cases Age 0
# cellulitis_cases <- Cellulitis_income_table[, paste(rep("CasesAvertedAge0", 6), 1:6, sep='_')]
# colnames(cellulitis_cases) <- paste(rep("Scenario", 6), 1:6)
# melt_cases <- reshape2::melt(cellulitis_cases)
# colnames(melt_cases) <- c("Income", "Scenario", "Cases averted")
#
# p1 <- ggplot2::ggplot(melt_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Cellulitis", x = 0.75,
#                     y = max(melt_cases$`Cases averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "CellulitisAge0_incomeCases.jpg", plot = p1, height = 4.6, width = 9)
#
# #Cases Age 5
# cellulitis_cases <- Cellulitis_income_table[, paste(rep("CasesAvertedAge5", 6), 1:6, sep='_')]
# colnames(cellulitis_cases) <- paste(rep("Scenario", 6), 1:6)
# melt_cases <- reshape2::melt(cellulitis_cases)
# colnames(melt_cases) <- c("Income", "Scenario", "Cases averted")
#
# p1 <- ggplot2::ggplot(melt_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Cellulitis", x = 0.75,
#                     y = max(melt_cases$`Cases averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "CellulitisAge5_incomeCases.jpg", plot = p1, height = 4.6, width = 9)
#
# #DALYs Age 0
# cellulitis_dalys <- Cellulitis_income_table[, paste(rep("DALYsAvertedAge0", 6), 1:6, sep='_')]
# colnames(cellulitis_dalys) <- paste(rep("Scenario", 6), 1:6)
# melt_dalys <- reshape2::melt(cellulitis_dalys)
# colnames(melt_dalys) <- c("Income", "Scenario", "DALYs averted")
#
# p1 <- ggplot2::ggplot(melt_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Cellulitis", x = 0.75,
#                     y = max(melt_dalys$`DALYs averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "CellulitisAge0_incomeDALYs.jpg", plot = p1, height = 4.6, width = 9)
#
# #DALYs Age 5
# cellulitis_dalys <- Cellulitis_income_table[, paste(rep("DALYsAvertedAge5", 6), 1:6, sep='_')]
# colnames(cellulitis_dalys) <- paste(rep("Scenario", 6), 1:6)
# melt_dalys <- reshape2::melt(cellulitis_dalys)
# colnames(melt_dalys) <- c("Income", "Scenario", "DALYs averted")
#
# p1 <- ggplot2::ggplot(melt_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Cellulitis", x = 0.75,
#                     y = max(melt_dalys$`DALYs averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "CellulitisAge5_incomeDALYs.jpg", plot = p1, height = 4.6, width = 9)
#
# #################################################################################################
# #Impetigo
# #Cases Age 0
# impetigo_cases <- Impetigo_income_table[, paste(rep("CasesAvertedAge0", 6), 1:6, sep='_')]
# colnames(impetigo_cases) <- paste(rep("Scenario", 6), 1:6)
# melt_cases <- reshape2::melt(impetigo_cases)
# colnames(melt_cases) <- c("Income", "Scenario", "Cases averted")
#
# p1 <- ggplot2::ggplot(melt_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Impetigo", x = 0.75,
#                     y = max(melt_cases$`Cases averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "ImpetigoAge0_incomeCases.jpg", plot = p1, height = 4.6, width = 9)
#
# #Cases Age 5
# impetigo_cases <- Impetigo_income_table[, paste(rep("CasesAvertedAge5", 6), 1:6, sep='_')]
# colnames(impetigo_cases) <- paste(rep("Scenario", 6), 1:6)
# melt_cases <- reshape2::melt(impetigo_cases)
# colnames(melt_cases) <- c("Income", "Scenario", "Cases averted")
#
# p1 <- ggplot2::ggplot(melt_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Impetigo", x = 0.75,
#                     y = max(melt_cases$`Cases averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "ImpetigoAge5_incomeCases.jpg", plot = p1, height = 4.6, width = 9)
#
# #DALYs Age 0
# impetigo_dalys <- Impetigo_income_table[, paste(rep("DALYsAvertedAge0", 6), 1:6, sep='_')]
# colnames(impetigo_dalys) <- paste(rep("Scenario", 6), 1:6)
# melt_dalys <- reshape2::melt(impetigo_dalys)
# colnames(melt_dalys) <- c("Income", "Scenario", "DALYs averted")
#
# p1 <- ggplot2::ggplot(melt_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Impetigo", x = 0.75,
#                     y = max(melt_dalys$`DALYs averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "ImpetigoAge0_incomeDALYs.jpg", plot = p1, height = 4.6, width = 9)
#
# #DALYs Age 5
# impetigo_dalys <- Impetigo_income_table[, paste(rep("DALYsAvertedAge5", 6), 1:6, sep='_')]
# colnames(impetigo_dalys) <- paste(rep("Scenario", 6), 1:6)
# melt_dalys <- reshape2::melt(impetigo_dalys)
# colnames(melt_dalys) <- c("Income", "Scenario", "DALYs averted")
#
# p1 <- ggplot2::ggplot(melt_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Impetigo", x = 0.75,
#                     y = max(melt_dalys$`DALYs averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "ImpetigoAge5_incomeDALYs.jpg", plot = p1, height = 4.6, width = 9)
#
# #################################################################################################
# #Pharyngitis
# #Cases Age 0
# pharyngitis_cases <- Pharyngitis_income_table[, paste(rep("CasesAvertedAge0", 6), 1:6, sep='_')]
# colnames(pharyngitis_cases) <- paste(rep("Scenario", 6), 1:6)
# melt_cases <- reshape2::melt(pharyngitis_cases)
# colnames(melt_cases) <- c("Income", "Scenario", "Cases averted")
#
# p1 <- ggplot2::ggplot(melt_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Pharyngitis", x = 0.75,
#                     y = max(melt_cases$`Cases averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "PharyngitisAge0_incomeCases.jpg", plot = p1, height = 4.6, width = 9)
#
# #Cases Age 5
# pharyngitis_cases <- Pharyngitis_income_table[, paste(rep("CasesAvertedAge5", 6), 1:6, sep='_')]
# colnames(pharyngitis_cases) <- paste(rep("Scenario", 6), 1:6)
# melt_cases <- reshape2::melt(pharyngitis_cases)
# colnames(melt_cases) <- c("Income", "Scenario", "Cases averted")
#
# p1 <- ggplot2::ggplot(melt_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Pharyngitis", x = 0.75,
#                     y = max(melt_cases$`Cases averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "PharyngitisAge5_incomeCases.jpg", plot = p1, height = 4.6, width = 9)
#
# #DALYs Age 0
# pharyngitis_dalys <- Pharyngitis_income_table[, paste(rep("DALYsAvertedAge0", 6), 1:6, sep='_')]
# colnames(pharyngitis_dalys) <- paste(rep("Scenario", 6), 1:6)
# melt_dalys <- reshape2::melt(pharyngitis_dalys)
# colnames(melt_dalys) <- c("Income", "Scenario", "DALYs averted")
#
# p1 <- ggplot2::ggplot(melt_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Pharyngitis", x = 0.75,
#                     y = max(melt_dalys$`DALYs averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "PharyngitisAge0_incomeDALYs.jpg", plot = p1, height = 4.6, width = 9)
#
# #DALYs Age 5
# pharyngitis_dalys <- Pharyngitis_income_table[, paste(rep("DALYsAvertedAge5", 6), 1:6, sep='_')]
# colnames(pharyngitis_dalys) <- paste(rep("Scenario", 6), 1:6)
# melt_dalys <- reshape2::melt(pharyngitis_dalys)
# colnames(melt_dalys) <- c("Income", "Scenario", "DALYs averted")
#
# p1 <- ggplot2::ggplot(melt_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Pharyngitis", x = 0.75,
#                     y = max(melt_dalys$`DALYs averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "PharyngitisAge5_incomeDALYs.jpg", plot = p1, height = 4.6, width = 9)
#
# #################################################################################################
# #Invasive
# #Cases Age 0
# invasive_cases <- `Invasive infection_income_table`[, paste(rep("CasesAvertedAge0", 6), 1:6, sep='_')]
# colnames(invasive_cases) <- paste(rep("Scenario", 6), 1:6)
# melt_cases <- reshape2::melt(invasive_cases)
# colnames(melt_cases) <- c("Income", "Scenario", "Cases averted")
#
# p1 <- ggplot2::ggplot(melt_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Invasive", x = 0.75,
#                     y = max(melt_cases$`Cases averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "InvasiveAge0_incomeCases.jpg", plot = p1, height = 4.6, width = 9)
#
# #Cases Age 5
# invasive_cases <- `Invasive infection_income_table`[, paste(rep("CasesAvertedAge5", 6), 1:6, sep='_')]
# colnames(invasive_cases) <- paste(rep("Scenario", 6), 1:6)
# melt_cases <- reshape2::melt(invasive_cases)
# colnames(melt_cases) <- c("Income", "Scenario", "Cases averted")
#
# p1 <- ggplot2::ggplot(melt_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Invasive", x = 0.75,
#                     y = max(melt_cases$`Cases averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "InvasiveAge5_incomeCases.jpg", plot = p1, height = 4.6, width = 9)
#
# #DALYs Age 0
# invasive_dalys <- `Invasive infection_income_table`[, paste(rep("DALYsAvertedAge0", 6), 1:6, sep='_')]
# colnames(invasive_dalys) <- paste(rep("Scenario", 6), 1:6)
# melt_dalys <- reshape2::melt(invasive_dalys)
# colnames(melt_dalys) <- c("Income", "Scenario", "DALYs averted")
#
# p1 <- ggplot2::ggplot(melt_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Invasive", x = 0.75,
#                     y = max(melt_dalys$`DALYs averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "InvasiveAge0_incomeDALYs.jpg", plot = p1, height = 4.6, width = 9)
#
# #DALYs Age 5
# invasive_dalys <- `Invasive infection_income_table`[, paste(rep("DALYsAvertedAge5", 6), 1:6, sep='_')]
# colnames(invasive_dalys) <- paste(rep("Scenario", 6), 1:6)
# melt_dalys <- reshape2::melt(invasive_dalys)
# colnames(melt_dalys) <- c("Income", "Scenario", "DALYs averted")
#
# p1 <- ggplot2::ggplot(melt_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Invasive", x = 0.75,
#                     y = max(melt_dalys$`DALYs averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "InvasiveAge5_incomeDALYs.jpg", plot = p1, height = 4.6, width = 9)
#
# #Deaths Age 0
# invasive_deaths <- `Invasive infection_income_table`[, paste(rep("DeathsAvertedAge0", 6), 1:6, sep='_')]
# colnames(invasive_deaths) <- paste(rep("Scenario", 6), 1:6)
# melt_deaths <- reshape2::melt(invasive_deaths)
# colnames(melt_deaths) <- c("Income", "Scenario", "Deaths averted")
#
# p1 <- ggplot2::ggplot(melt_deaths, ggplot2::aes(fill=Scenario, y=`Deaths averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Invasive", x = 0.75,
#                     y = max(melt_deaths$`Deaths averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "InvasiveAge0_incomeDeaths.jpg", plot = p1, height = 4.6, width = 9)
#
# #deaths Age 5
# invasive_deaths <- `Invasive infection_income_table`[, paste(rep("DeathsAvertedAge5", 6), 1:6, sep='_')]
# colnames(invasive_deaths) <- paste(rep("Scenario", 6), 1:6)
# melt_deaths <- reshape2::melt(invasive_deaths)
# colnames(melt_deaths) <- c("Income", "Scenario", "Deaths averted")
#
# p1 <- ggplot2::ggplot(melt_deaths, ggplot2::aes(fill=Scenario, y=`Deaths averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Invasive", x = 0.75,
#                     y = max(melt_deaths$`Deaths averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "InvasiveAge5_incomeDeaths.jpg", plot = p1, height = 4.6, width = 9)
#
# #################################################################################################
# #RHD
# #Cases Age 0
# rhd_cases <- `Rheumatic heart disease_income_table`[, paste(rep("CasesAvertedAge0", 6), 1:6, sep='_')]
# colnames(rhd_cases) <- paste(rep("Scenario", 6), 1:6)
# melt_cases <- reshape2::melt(rhd_cases)
# colnames(melt_cases) <- c("Income", "Scenario", "Cases averted")
#
# p1 <- ggplot2::ggplot(melt_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 1.1,
#                     y = max(melt_cases$`Cases averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "RHDAge0_incomeCases.jpg", plot = p1, height = 4.6, width = 9)
#
# high_cases <- melt_cases[melt_cases$Income == "High income",]
# p1 <- ggplot2::ggplot(high_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 0.75,
#                     y = max(high_cases$`Cases averted`)*1.1, size = 5)
# ggplot2::ggsave(filename = "RHDAge0_incomeCases_high.jpg", plot = p1, height = 4.6, width = 9)
#
# #Cases Age 5
# rhd_cases <- `Rheumatic heart disease_income_table`[, paste(rep("CasesAvertedAge5", 6), 1:6, sep='_')]
# colnames(rhd_cases) <- paste(rep("Scenario", 6), 1:6)
# melt_cases <- reshape2::melt(rhd_cases)
# colnames(melt_cases) <- c("Income", "Scenario", "Cases averted")
#
# p1 <- ggplot2::ggplot(melt_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 1.1,
#                     y = max(melt_cases$`Cases averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "RHDAge5_incomeCases.jpg", plot = p1, height = 4.6, width = 9)
#
# high_cases <- melt_cases[melt_cases$Income == "High income",]
# p1 <- ggplot2::ggplot(high_cases, ggplot2::aes(fill=Scenario, y=`Cases averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 0.75,
#                     y = max(high_cases$`Cases averted`)*1.1, size = 5)
# ggplot2::ggsave(filename = "RHDAge5_incomeCases_high.jpg", plot = p1, height = 4.6, width = 9)
#
#
# #DALYs Age 0
# rhd_dalys <- `Rheumatic heart disease_income_table`[, paste(rep("DALYsAvertedAge0", 6), 1:6, sep='_')]
# colnames(rhd_dalys) <- paste(rep("Scenario", 6), 1:6)
# melt_dalys <- reshape2::melt(rhd_dalys)
# colnames(melt_dalys) <- c("Income", "Scenario", "DALYs averted")
#
# p1 <- ggplot2::ggplot(melt_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 1.1,
#                     y = max(melt_dalys$`DALYs averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "RHDAge0_incomeDALYs.jpg", plot = p1, height = 4.6, width = 9)
#
# high_dalys <- melt_dalys[melt_dalys$Income == "High income",]
# p1 <- ggplot2::ggplot(high_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 0.75,
#                     y = max(high_dalys$`DALYs averted`)*1.1, size = 5)
# ggplot2::ggsave(filename = "RHDAge0_incomeDalys_high.jpg", plot = p1, height = 4.6, width = 9)
#
#
# #DALYs Age 5
# rhd_dalys <- `Rheumatic heart disease_income_table`[, paste(rep("DALYsAvertedAge5", 6), 1:6, sep='_')]
# colnames(rhd_dalys) <- paste(rep("Scenario", 6), 1:6)
# melt_dalys <- reshape2::melt(rhd_dalys)
# colnames(melt_dalys) <- c("Income", "Scenario", "DALYs averted")
#
# p1 <- ggplot2::ggplot(melt_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 1.1,
#                     y = max(melt_dalys$`DALYs averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "RHDAge5_incomeDALYs.jpg", plot = p1, height = 4.6, width = 9)
#
# high_dalys <- melt_dalys[melt_dalys$Income == "High income",]
# p1 <- ggplot2::ggplot(high_dalys, ggplot2::aes(fill=Scenario, y=`DALYs averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 0.75,
#                     y = max(high_dalys$`DALYs averted`)*1.1, size = 5)
# ggplot2::ggsave(filename = "RHDAge5_incomeDalys_high.jpg", plot = p1, height = 4.6, width = 9)
#
#
# #Deaths Age 0
# rhd_deaths <- `Rheumatic heart disease_income_table`[, paste(rep("DeathsAvertedAge0", 6), 1:6, sep='_')]
# colnames(rhd_deaths) <- paste(rep("Scenario", 6), 1:6)
# melt_deaths <- reshape2::melt(rhd_deaths)
# colnames(melt_deaths) <- c("Income", "Scenario", "Deaths averted")
#
# p1 <- ggplot2::ggplot(melt_deaths, ggplot2::aes(fill=Scenario, y=`Deaths averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 1.1,
#                     y = max(melt_deaths$`Deaths averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "RHDAge0_incomeDeaths.jpg", plot = p1, height = 4.6, width = 9)
#
# high_deaths <- melt_deaths[melt_deaths$Income == "High income",]
# p1 <- ggplot2::ggplot(high_deaths, ggplot2::aes(fill=Scenario, y=`Deaths averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 0.75,
#                     y = max(high_deaths$`Deaths averted`)*1.1, size = 5)
# ggplot2::ggsave(filename = "RHDAge0_incomeDeaths_high.jpg", plot = p1, height = 4.6, width = 9)
#
#
# #deaths Age 5
# rhd_deaths <- `Rheumatic heart disease_income_table`[, paste(rep("DeathsAvertedAge5", 6), 1:6, sep='_')]
# colnames(rhd_deaths) <- paste(rep("Scenario", 6), 1:6)
# melt_deaths <- reshape2::melt(rhd_deaths)
# colnames(melt_deaths) <- c("Income", "Scenario", "Deaths averted")
#
# p1 <- ggplot2::ggplot(melt_deaths, ggplot2::aes(fill=Scenario, y=`Deaths averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 1.1,
#                     y = max(melt_deaths$`Deaths averted`)*1.01, size = 5)
# ggplot2::ggsave(filename = "RHDAge5_incomeDeaths.jpg", plot = p1, height = 4.6, width = 9)
#
# high_deaths <- melt_deaths[melt_deaths$Income == "High income",]
# p1 <- ggplot2::ggplot(high_deaths, ggplot2::aes(fill=Scenario, y=`Deaths averted`, x=Income)) +
#   ggplot2::geom_bar(position="dodge", stat="identity", colour = "black")+
#   ggplot2::theme(legend.title = ggplot2::element_blank())+
#   ggplot2::xlab("")+
#   #ggplot2::ylab("Incidence (rate per 100,000)")+
#   ggplot2::scale_fill_manual(values = colP)+
#   ggplot2::annotate("text", label = "Rheumatic heart disease", x = 0.75,
#                     y = max(high_deaths$`Deaths averted`)*1.1, size = 5)
# ggplot2::ggsave(filename = "RHDAge5_incomeDeaths_high.jpg", plot = p1, height = 4.6, width = 9)
#
#
#
#
#
#
#
#
#
#
#
