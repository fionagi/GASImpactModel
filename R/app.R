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
#                             choices = c("All", "East Asia & Pacific", "Europe & Central Asia",
#                                         "Latin America & Carribbean", "Middle East & North Africa",
#                                         "Northern America", "South Asia", "Sub-Saharan Africa")),
#                 selectInput(inputId = "country",
#                             label = "Country:",
#                             choices = c("Australia", "Fiji", "New Zealand")),
#
#                 h4("Condition settings"),
#                 selectInput(inputId = "condition",
#                             label = "Condition:",
#                             choices = c("Rheumatic Heart Disease", "Cellulitis")),
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
#                             tabPanel("Plot", plotOutput("impactPlot")),
#                             tabPanel("Table", tableOutput("impactTable")),
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
# impactPlot <- reactiveVal()
#
# impactData <- eventReactive(input$submitButton1, {
#
#     country <- isolate(input$country)
#     condition <-isolate(input$condition)
#     ageV <- isolate(input$ageV)
#     duration <- isolate(input$duration)
#     coverage <- isolate(input$coverage)
#     efficacy <- isolate(input$efficacy)
#
#     incR <- getRateData(country, condition)[[1]]
#     mProb <- getMorData(country)
#
#     impModels <- runModel(conditions = condition, inc = incR, mortality = mProb,
#                      nyears = -1, vaccAge = ageV, vaccEff = efficacy,
#                      vaccDur = duration)
#
#     impP <- makePlot(noVacc_mod = impModels[[1]], vacc_mod = impModels[[2]],
#                       conditions = condition)
#
#     impactPlot(impP)
#
#     impT <- makeTable(noVacc_mod = impModels[[1]], vacc_mod = impModels[[2]],
#                       conditions = condition, initPop = 100000,
#                           valueVac = -1)
#     impT <- round(impT, digits=0)
#
#     impT
#
# })
#
# output$impactPlot <- renderPlot({
#   impactPlot()
# })
#
# output$impactTable <- renderTable({
#   impactData()
# }, rownames = TRUE)
#
#
# output$saveImpactTable <- downloadHandler(
#   filename = function() {
#     paste("impactTable", Sys.Date(), ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(impactData(), file, row.names = F)
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
