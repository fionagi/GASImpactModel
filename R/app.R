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
#
#
# ##################################################################################################
# #User interface code
#
# ui <- fluidPage(
#
#
#             titlePanel(img(src = "Savac-logo.png", height = 140, width = 400)),
#
#             sidebarLayout(
#
#               sidebarPanel(
#
#                 # Input: Selector for choosing dataset ----
#                 h4("Region settings"),
#                 selectInput(inputId = "region",
#                             label = "World region:",
#                             choices = c("All", unique(data.region$Region))),
#
#                 uiOutput("countryChoice"),
#
#                 h4("Condition settings"),
#                 selectInput(inputId = "condition",
#                             label = "Condition:",
#                             choices = c("Rheumatic Heart Disease", "Cellulitis")),
#
#                 conditionalPanel(
#                   condition = "input.condition == 'Cellulitis'",
#                   sliderInput("a",
#                               label = "Proportion attributable to GAS:",
#                               min = 0, max = 1,step = 0.1, value = 0)
#                 ),
#
#                 h4("Vaccine settings"),
#                 sliderInput(inputId = "ageV",
#                             label = "Age of vaccination",
#                             min = 0, max = 80, value = 0, step = 1
#                 ),
#                 sliderInput(inputId = "duration",
#                             label = "Durability",
#                             min = 0, max = 80, value = 0, step = 1
#                 ),
#                 sliderInput(inputId = "coverage",
#                             label = "Coverage %",
#                             min = 0, max = 100, value = 0, step = 1
#                 ),
#                 sliderInput(inputId = "efficacy",
#                             label = "Efficacy %",
#                             min = 0, max = 100, value = 0, step = 1
#                 ),
#
#                 actionButton("submitButton1", "Run analysis", class = "btn-success"),
#                 downloadButton("saveImpactTable", "Save table"),
#                 downloadButton("saveImpactPlot", "Save plot")
#
#               ),
#
#               mainPanel(
#                 tabsetPanel(type = "tabs",
#                             tabPanel("Impact analysis",
#                                      br(),
#                                      p("Comparison of incidence and
#                                         DALYs per 100,000 persons, from age of
#                                         vaccination to end of durability. Using
#                                         data from selected country and condition"),
#                                      br(),
#                                      radioGroupButtons(
#                                        inputId = "outputChoice1",
#                                        choices = c("Numbers", "Rate"),
#                                        selected = "Rate",
#                                        checkIcon = list(yes = icon("check")),
#                                      ),
#                                      tags$script("$(\"input:radio[name='outputChoice1'][value='Numbers']\").parent().css('background-color', 'lightgrey');"),
#                                      tags$script("$(\"input:radio[name='outputChoice1'][value='Rate']\").parent().css('background-color', 'lightgrey');"),
#                                      plotOutput("impactPlot")),
#                             tabPanel("Age-specific parameters",
#                                      br(),
#                                      p("Incidence, deaths and DALYs per 100,000 persons
#                                        for selected country and condition. Error bars show
#                                        95% confidence intervals. Data is from Global Health
#                                        Data Exchange (2019)"),
#                                      br(),
#                                      radioGroupButtons(
#                                        inputId = "outputChoice2",
#                                        choices = c("Numbers", "Rate"),
#                                        selected = "Rate",
#                                        checkIcon = list(yes = icon("check")),
#                                      ),
#                                      tags$script("$(\"input:radio[name='outputChoice2'][value='Numbers']\").parent().css('background-color', 'lightgrey');"),
#                                      tags$script("$(\"input:radio[name='outputChoice2'][value='Rate']\").parent().css('background-color', 'lightgrey');"),
#                                      plotOutput("currentPlot")),
#                             tabPanel("Help"),
#                             tabPanel("About")
#                 )
#
#               )
#
#             ),#end sidebarLayout
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
# currentPlot <- eventReactive(input$submitButton1, {
#
#   country <- isolate(input$country)
#   condition <-isolate(input$condition)
#
#   incR <- getRateData(country, condition)[[1]]
#   deaths <- getRateData(country, condition)[[2]]
#   dalys <- getRateData(country, condition)[[3]]
#
#   p1 <- makeBarPlot(incR, ylabel = "Incidence",
#                       colFill = "steelblue")
#   p2 <- makeBarPlot(deaths, ylabel = "Deaths",
#                     colFill = "steelblue")
#   p3 <- makeBarPlot(dalys, ylabel = "DALYs",
#                     colFill = "steelblue")
#
#   ggarrange(p1, p2, p3, ncol = 1, nrow = 3)
#
# })
#
# output$currentPlot <- renderPlot({
#   currentPlot()
# }, height = 600, width = 900)
#
# impactData <- eventReactive(input$submitButton1, {
#
#     country <- isolate(input$country)
#     condition <-isolate(input$condition)
#     ageV <- isolate(input$ageV)
#     duration <- isolate(input$duration)
#     coverage <- isolate(input$coverage)
#     efficacy <- isolate(input$efficacy)
#     overallEff <- (efficacy*coverage)/100 #as a percentage
#
#     incR <- getRateData(country, condition)[[1]]
#     dalys <- getRateData(country, condition)[[3]]
#     mProb <- getMorData(country)
#
#     impModels <- runModel(conditions = condition, inc = incR, dalys = dalys,
#                           mortality = mProb, nyears = -1, vaccAge = ageV,
#                           vaccEff = overallEff, vaccDur = duration)
#     impModels
# })
#
# impactPlot <- reactive({
#   impModels <- impactData()
#
#   country <- isolate(input$country)
#   ageV <- isolate(input$ageV)
#   duration <- isolate(input$duration)
#   condition <-isolate(input$condition)
#   coverage <- isolate(input$coverage)
#   efficacy <- isolate(input$efficacy)
#   overallEff <- (efficacy*coverage)/100 #as a percentage
#
#   deaths <- getRateData(country, condition)[[2]]
#
#
#
#   deaths_mod <-findDeaths(noVaccDeaths = deaths, conditions = condition,
#                           vaccAge = ageV, vaccEff = overallEff,
#                           vaccDur = duration)
#
#   p <- makePlot(noVacc_mod = impModels[[1]], vacc_mod = impModels[[2]],
#                 deaths_mod, conditions = condition, vAge = ageV, vDur = duration)
#   ggarrange(p[[1]], p[[2]], p[[3]], ncol = 1, nrow = 3)
# })
#
# impactTable <- reactive({
#   impModels <- impactData()
#   condition <-isolate(input$condition)
#   impT <- makeTable(noVacc_mod = impModels[[1]], vacc_mod = impModels[[2]],
#                     conditions = condition, initPop = 100000,
#                     valueVac = -1)
#   impT <- round(impT, digits=0)
#   impT
# })
#
# output$impactPlot <- renderPlot({
#   impactPlot()
# }, height = 600, width = 900)
#
# output$impactTable <- renderTable({
#   impactTable()
# }, rownames = TRUE)
#
#
# output$saveImpactTable <- downloadHandler(
#   filename = function() {
#     paste("impactTable", Sys.Date(), ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(impactTable(), file, row.names = T)
#   }
# )
#
# output$saveImpactPlot <- downloadHandler(
#   filename = function() {
#     paste("impactPlot", Sys.Date(), ".jpeg", sep="")
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
