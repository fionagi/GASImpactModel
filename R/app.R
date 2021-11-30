# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #    Country
# #    Year of vaccine introduction
# #    Age of vaccination
# #    Vaccine coverage
# #    Vaccine efficacy
#
#
# library(shiny)
# library(shinyWidgets)
# library(ggpubr)
# library(GASImpactModel)
# library(xlsx)
# library(stringr)
#
#
# ##################################################################################################
# #User interface code
#
# ui <- fluidPage(
#
#
#        titlePanel(img(src = "Savac-logo.png", height = 140, width = 400)),
#
#        sidebarLayout(
#
#         sidebarPanel(
#
#           # Input: Selector for choosing dataset
#           h4("Region settings"),
#           selectInput(inputId = "region",
#                       label = "World region:",
#                       choices = c("All", unique(data.region$Region))),
#
#           uiOutput("countryChoice"),
#
#           h4("Condition settings"),
#           selectInput(inputId = "condition",
#                       label = "Condition:",
#                       choices = c("Rheumatic Heart Disease", "Cellulitis",
#                                   "Invasive infection", "Pharyngitis",
#                                   "Impetigo")),
#
#           conditionalPanel(condition = "input.condition == 'Cellulitis'",
#                             sliderInput(inputId = "propAttrC",
#                             label = "Proportion attributable to GAS:",
#                             min = 0, max = 1, step = 0.01, value = 0)),
#
#           conditionalPanel(condition = "input.condition == 'Impetigo'",
#                            sliderInput(inputId = "propAttrI",
#                                        label = "Proportion attributable to GAS:",
#                                        min = 0, max = 1, step = 0.01, value = 0)),
#
#           h4("Vaccine settings"),
#           sliderTextInput(inputId = "yearV",
#                             label = "Year of vaccine introduction",
#                             choices = as.character(2020:2050), selected = "2020"),
#           sliderInput(inputId = "ageV",
#                       label = "Age of vaccination",
#                       min = 0, max = 80, value = 0, step = 1),
#           sliderInput(inputId = "duration",
#                       label = "Durability",
#                       min = 1, max = 80, value = 1, step = 1),
#           checkboxInput(inputId = "waning",
#                         label = "Waning immunity", value = FALSE),
#           sliderInput(inputId = "coverage",
#                       label = "Coverage %",
#                       min = 1, max = 100, value = 1, step = 1),
#           checkboxInput(inputId = "ramp",
#                         label = "Ramp to maximum", value = FALSE),
#           sliderInput(inputId = "efficacy",
#                       label = "Efficacy %",
#                       min = 1, max = 100, value = 1, step = 1),
#
#           actionButton("submitButton1", "Run analysis", class = "btn-success"),
#
#       ), #end sidebarPanel
#
#       mainPanel(
#
#         tabsetPanel(type = "tabs",
#
#           tabPanel("Impact analysis",
#                     br(),
#                     p("Comparisons of number of cases or incidence rates
#                        per 100,000 persons for selected vaccination scenario.
#                       Using data from selected country and condition."),
#                     br(),
#
#
#                     radioGroupButtons(inputId = "impactChoice",
#                                                   choices = c("Calendar year", "Year of birth", "Year of vaccination"),
#                                                   selected = "Calendar year",
#                                                   checkIcon = list(yes = icon("check"))),
#
#                     tags$script("$(\"input:radio[name='impactChoice'][value='Calendar year']\").parent().css('background-color', 'lightblue');"),
#                     tags$script("$(\"input:radio[name='impactChoice'][value='Year of birth']\").parent().css('background-color', 'lightblue');"),
#                     tags$script("$(\"input:radio[name='impactChoice'][value='Year of vaccination']\").parent().css('background-color', 'lightblue');"),
#
#                     radioGroupButtons(inputId = "plotChoice",
#                                      choices = c("Cases/DALYs/Deaths", "Cases/DALYs/Deaths averted"),
#                                      selected = "Cases/DALYs/Deaths",
#                                      checkIcon = list(yes = icon("check"))),
#
#
#                     selectInput(inputId = "plotYears",
#                                             label = "Number of years to plot:",
#                                              choices = c(10, 20, 30)),
#                     downloadButton("saveImpactPlot", "Save plot"),
#                     downloadButton("saveImpactTable", "Save table"),
#
#
#                     plotOutput("impactPlot")),
#
#          tabPanel("Age-specific parameters",
#                     br(),
#                     p("Incidents, deaths and DALYs (numbers or rates per 100,000
#                        persons) for selected country and condition. Error bars show
#                       95% confidence intervals. Data is from Global Health Data Exchange (2019)"),
#                     br(),
#
#                     downloadButton("saveCurrentPlot", "Save plot"),
#
#                     plotOutput("currentPlot")),
#
#          tabPanel("Help"),
#
#          tabPanel("About"))
#
#       ) #end mainPanel
#
#   ),#end sidebarLayout
#
# )#end fluidPage
#
#
# server <- function(input, output) {
#
#   output$countryChoice <- renderUI({
#     countries <- getCountries(input$region)
#     selectInput(inputId = "country", label = "Country", choices = countries)
#   })
#
#
# currentPlot <- eventReactive(c(input$submitButton1, input$outputChoice2), {
#
#   country <- isolate(input$country)
#   condition <- isolate(input$condition)
#   prop <- 1 #proportion of incidents attributable to Strep A
#
#   if(condition == "Impetigo") prop <- isolate(input$propAttrI)
#   if(condition == "Cellulitis") prop <- isolate(input$propAttrC)
#
#   if(condition == "Cellulitis" || condition == "Rheumatic Heart Disease")
#   {
#     incR <- getConditionData(country, condition, "Number", prop)[[1]]
#     deaths <- getConditionData(country, condition, "Number", prop)[[2]]
#     dalys <- getConditionData(country, condition, "Number", prop)[[3]]
#
#     p1 <- makeBarPlot(incR, ylabel = "Number of cases", colFill = "steelblue")
#     p2 <- makeBarPlot(deaths, ylabel = "Deaths", colFill = "steelblue")
#     p3 <- makeBarPlot(dalys, ylabel = "DALYs", colFill = "steelblue")
#
#     ggarrange(p1, p2, p3, ncol = 1, nrow = 3)
#
#   }else{
#     incR <- getConditionData(country, condition, "Rate", prop)
#     incR_per100 <- incR
#     incR_per100[,"val"] <- 100*incR_per100[,"val"]
#     makeBarPlot(incR_per100, ylabel = "Number of cases per 100 persons", colFill = "steelblue")
#   }
#
# })
#
# output$currentPlot <- renderPlot({
#   currentPlot()
# }, height = 600, width = 900)
#
# output$saveCurrentPlot <- downloadHandler(
#   filename = function() {
#     paste("currentPlot", Sys.Date(), ".jpeg", sep="")
#   },
#   content = function(file) {
#     ggsave(file, currentPlot(), height = 15, width = 15)
#   }
# )
#
#
# impactData <- eventReactive(input$submitButton1, {
#
#     country <- isolate(input$country)
#     condition <- isolate(input$condition)
#     impType <- input$impactChoice
#     yearV <- isolate(input$yearV)
#     ageV <- isolate(input$ageV)
#     duration <- isolate(input$duration)
#     waning <- isolate(input$waning)
#     coverage <- isolate(input$coverage)
#     ramp <- isolate(input$ramp)
#     efficacy <- isolate(input$efficacy)
#     overallEff <- (efficacy*coverage)/100 #as a percentage
#     prop <- 1 #proportion of incidents attributable to Strep A
#
#     if(condition == "Impetigo") prop <- isolate(input$propAttrI)
#     if(condition == "Cellulitis") prop <- isolate(input$propAttrC)
#
#     impType <- isolate(input$impactChoice)
#     plotYears <- isolate(as.numeric(input$plotYears))
#
#     if(condition == "Cellulitis" || condition == "Rheumatic Heart Disease"){
#       incR <- getConditionData(country, condition, "Rate", prop)[[1]]
#       rate <- 100000
#     }else{
#       incR <- getConditionData(country, condition, "Rate", prop)
#       rate <- 1
#     }
#
#     mProb <- getMorData(location = country, yearV = yearV, pYears = plotYears,
#                         ageV = ageV, impType = impType, )
#
#     initPop <- getInitPop(location = country, yearV = yearV,
#                             pYears = plotYears, ageV = ageV)
#
#     impModels <- runModel(location = country, condition = condition, inc = incR,
#                             rate = rate, mortality = mProb, yearV = yearV,
#                             vaccAge = ageV, vaccEff = overallEff,
#                             vaccDur = duration, waning = waning, ramp = ramp,
#                             impType = impType, pYears = plotYears,
#                             initPop = initPop)
#
#     impModels
# })
#
# impactPlot <- reactive({
#
#   impModels <- impactData()
#   impType <- input$impactChoice
#   plotType <- input$plotChoice
#   plotYears <- isolate(as.numeric(input$plotYears))
#
#   country <- isolate(input$country)
#   yearV <- isolate(input$yearV)
#   ageV <- isolate(input$ageV)
#   duration <- isolate(input$duration)
#   condition <-isolate(input$condition)
#   coverage <- isolate(input$coverage)
#   efficacy <- isolate(input$efficacy)
#   overallEff <- (efficacy*coverage)/100 #as a percentage
#
#   noVacc_counts <- impModels[[1]]
#   vacc_counts <- impModels[[2]]
#   noVacc_dalys <- impModels[[3]]
#   vacc_dalys <- impModels[[4]]
#   noVacc_deaths <- impModels[[5]]
#   vacc_deaths <- impModels[[6]]
#   noVacc_pop <- impModels[[7]]
#
#   if(plotType == "Cases/DALYs/Deaths")
#   {
#     p_counts <- makePlot(noVacc_counts, vacc_counts, ylabel = "Number of cases",
#                  vAge = ageV, vYear = yearV, impType = impType,
#                  pYears = plotYears)
#
#     if(!is.na(noVacc_deaths)[1]){
#       p_dalys <- makePlot(noVacc_dalys, vacc_dalys, ylabel = "DALYs",
#                         vAge = ageV, vYear = yearV, impType = impType,
#                         pYears = plotYears)
#       p_deaths <- makePlot(noVacc_deaths, vacc_deaths, ylabel = "Deaths",
#                          vAge = ageV, vYear = yearV, impType = impType,
#                          pYears = plotYears)
#       ggarrange(p_counts[[1]], p_counts[[2]], p_dalys[[1]], p_dalys[[2]],
#               p_deaths[[1]], p_deaths[[2]], ncol = 2, nrow = 3)
#     }else{
#       if(!is.na(noVacc_dalys)[1]){
#         p_dalys <- makePlot(noVacc_dalys, vacc_dalys, ylabel = "DALYs",
#                           vAge = ageV, vYear = yearV, impType = impType,
#                           pYears = plotYears)
#         ggarrange(p_counts[[1]], p_counts[[2]], p_dalys[[1]], p_dalys[[2]],
#                 ncol = 2, nrow = 2)
#       }else{
#         ggarrange(p_counts[[1]], p_counts[[2]], ncol = 2, nrow = 1)
#       }
#     }
#   }else{
#     p_countsA <- makePlotAvert(noVacc_counts, vacc_counts, ylabel = "Number of cases",
#                          vAge = ageV, vYear = yearV, impType = impType,
#                          pYears = plotYears)
#
#     if(!is.na(noVacc_deaths)[1]){
#       p_dalysA <- makePlotAvert(noVacc_dalys, vacc_dalys, ylabel = "DALYs",
#                           vAge = ageV, vYear = yearV, impType = impType,
#                           pYears = plotYears)
#       p_deathsA <- makePlotAvert(noVacc_deaths, vacc_deaths, ylabel = "Deaths",
#                            vAge = ageV, vYear = yearV, impType = impType,
#                            pYears = plotYears)
#       ggarrange(p_countsA, p_dalysA, p_deathsA, ncol = 1, nrow = 3)
#     }else{
#       if(!is.na(noVacc_dalys)[1]){
#         p_dalysA <- makePlotAvert(noVacc_dalys, vacc_dalys, ylabel = "DALYs",
#                             vAge = ageV, vYear = yearV, impType = impType,
#                             pYears = plotYears)
#         ggarrange(p_countsA, p_dalysA, ncol = 1, nrow = 2)
#       }else{
#         ggarrange(p_countsA, ncol = 1, nrow = 1)
#       }
#     }
#   }
#
# })
#
# observeEvent(input$outputChoice1, {
#   impactPlot()
# })
#
# observeEvent(input$impactChoice, {
#   impactPlot()
# })
#
# output$impactPlot <- renderPlot({
#   impactPlot()
# }, height = 600, width = 900)
#
#
# output$saveImpactTable <- downloadHandler(
#   filename = function() {
#     country <- isolate(input$country)
#     yearV <- isolate(input$yearV)
#     ageV <- isolate(input$ageV)
#     duration <- isolate(input$duration)
#     condition <-isolate(input$condition)
#     coverage <- isolate(input$coverage)
#     efficacy <- isolate(input$efficacy)
#
#     paste(str_replace_all(country, " ", "_"), condition, "Age", ageV, "Year",
#           yearV, "Dur", duration, "Cov", coverage, "Eff", efficacy, ".xlsx", sep="")
#   },
#   content = function(file) {
#     impM <- impactData()
#
#     years <- colnames(impM[[1]])
#
#     colnames(impM[[1]]) <- paste("Year:", years)
#     colnames(impM[[2]]) <- paste("Year:", years)
#
#     write.xlsx(impM[[1]], file, sheetName = "Counts_PreVacc",
#                       col.names = TRUE, row.names = TRUE, append = FALSE)
#     write.xlsx(impM[[2]], file, sheetName = "Counts_Vacc",
#                col.names = TRUE, row.names = TRUE, append = TRUE)
#     #write.xlsx(groupResults(impM[[1]]), file, sheetName = "Counts_PreVacc",
#     #           col.names = TRUE, row.names = TRUE, append = FALSE)
#     #write.xlsx(groupResults(impM[[2]]), file, sheetName = "Counts_Vacc",
#     #           col.names = TRUE, row.names = TRUE, append = TRUE)
#
#     if(!is.na(impM[[3]])[1])
#     {
#      #write.xlsx(groupResults(impM[[3]]), file, sheetName = "DALYs_PreVacc",
#      #          col.names = TRUE, row.names = TRUE, append = TRUE)
#      #write.xlsx(groupResults(impM[[4]]), file, sheetName = "DALYs_Vacc",
#      #          col.names = TRUE, row.names = TRUE, append = TRUE)
#      colnames(impM[[3]]) <- paste("Year:", years)
#      colnames(impM[[4]]) <- paste("Year:", years)
#
#      write.xlsx(impM[[3]], file, sheetName = "DALYs_PreVacc",
#                 col.names = TRUE, row.names = TRUE, append = TRUE)
#      write.xlsx(impM[[4]], file, sheetName = "DALYs_Vacc",
#                 col.names = TRUE, row.names = TRUE, append = TRUE)
#     }
#
#     if(!is.na(impM[[5]])[1])
#     {
#      #write.xlsx(groupResults(impM[[5]]), file, sheetName = "Deaths_PreVacc",
#      #          col.names = TRUE, row.names = TRUE, append = TRUE)
#      #write.xlsx(groupResults(impM[[6]]), file, sheetName = "Deaths_Vacc",
#      #          col.names = TRUE, row.names = TRUE, append = TRUE)
#      colnames(impM[[5]]) <- paste("Year:", years)
#      colnames(impM[[6]]) <- paste("Year:", years)
#
#      write.xlsx(impM[[5]], file, sheetName = "Deaths_PreVacc",
#                 col.names = TRUE, row.names = TRUE, append = TRUE)
#      write.xlsx(impM[[6]], file, sheetName = "Deaths_Vacc",
#                 col.names = TRUE, row.names = TRUE, append = TRUE)
#     }
#   }
# )
#
# output$saveImpactPlot <- downloadHandler(
#   filename = function() {
#     country <- isolate(input$country)
#     yearV <- isolate(input$yearV)
#     ageV <- isolate(input$ageV)
#     duration <- isolate(input$duration)
#     condition <-isolate(input$condition)
#     coverage <- isolate(input$coverage)
#     efficacy <- isolate(input$efficacy)
#
#     paste(str_replace_all(country, " ", "_"), condition, "Age", ageV, "Year",
#           yearV, "Dur", duration, "Cov", coverage, "Eff", efficacy, ".jpeg", sep="")
#   },
#   content = function(file) {
#     ggsave(file, impactPlot())
#   }
# )
#
#
# }
#
# # Run the application
# shinyApp(ui = ui, server = server)
