#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    Country
#    Year of vaccine introduction
#    Age of vaccination
#    Vaccine coverage
#    Vaccine efficacy


library(shiny)
library(ggpubr)
library(GASImpactModel)


##################################################################################################
#User interface code

ui <- fluidPage(


            titlePanel(img(src = "Savac-logo.png", height = 140, width = 400)),

            sidebarLayout(

              sidebarPanel(

                # Input: Selector for choosing dataset ----
                h4("Region settings"),
                selectInput(inputId = "region",
                            label = "World region:",
                            choices = c("All", unique(data.region$Region))),

                uiOutput("countryChoice"),

                h4("Condition settings"),
                selectInput(inputId = "condition",
                            label = "Condition:",
                            choices = c("Rheumatic Heart Disease", "Cellulitis")),

                h4("Vaccine settings"),
                sliderInput(inputId = "ageV",
                            label = "Age of vaccination",
                            min = 0, max = 80, value = 0, step = 1
                ),
                sliderInput(inputId = "duration",
                            label = "Durability",
                            min = 0, max = 80, value = 0, step = 1
                ),
                sliderInput(inputId = "coverage",
                            label = "Coverage %",
                            min = 0, max = 100, value = 0, step = 1
                ),
                sliderInput(inputId = "efficacy",
                            label = "Efficacy %",
                            min = 0, max = 100, value = 0, step = 1
                ),

                actionButton("submitButton1", "Run analysis", class = "btn-success"),
                downloadButton("saveImpactTable", "Save table"),
                downloadButton("saveImpactPlot", "Save plot")

              ),

              mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Impact analysis",
                                     br(),
                                     p("Comparison of numbers of incidents and
                                        DALYs per 100,000 persons, from age of
                                        vaccination to end of durability. Using
                                        data from selected country and condition"),
                                     br(),
                                     plotOutput("impactPlot")),
                            tabPanel("Age-specific parameters",
                                     br(),
                                     p("Incidents, deaths and DALYs per 100,000 persons
                                       for selected country and condition. Error bars show
                                       95% confidence intervals. Data is from Global Health
                                       Data Exchange (2019)"),
                                     br(),
                                     plotOutput("currentPlot")),
                            tabPanel("Help"),
                            tabPanel("About")
                )

              )

            ),#end sidebarLayout

)#end fluidPage


server <- function(input, output) {

  output$countryChoice <- renderUI({
    countries <- getCountries(input$region)
    selectInput(inputId = "country", label = "Country", choices = countries)
  })


currentPlot <- eventReactive(input$submitButton1, {

  country <- isolate(input$country)
  condition <-isolate(input$condition)

  incR <- getRateData(country, condition)[[1]]
  deaths <- getRateData(country, condition)[[2]]
  dalys <- getRateData(country, condition)[[3]]

  p1 <- makeBarPlot(incR, ylabel = "Incidents",
                      colFill = "steelblue")
  p2 <- makeBarPlot(deaths, ylabel = "Deaths",
                    colFill = "steelblue")
  p3 <- makeBarPlot(dalys, ylabel = "DALYs",
                    colFill = "steelblue")

  ggarrange(p1, p2, p3, ncol = 3, nrow = 1)

})

output$currentPlot <- renderPlot({
  currentPlot()
})

impactData <- eventReactive(input$submitButton1, {

    country <- isolate(input$country)
    condition <-isolate(input$condition)
    ageV <- isolate(input$ageV)
    duration <- isolate(input$duration)
    coverage <- isolate(input$coverage)
    efficacy <- isolate(input$efficacy)

    incR <- getRateData(country, condition)[[1]]
    dalys <- getRateData(country, condition)[[3]]
    mProb <- getMorData(country)

    impModels <- runModel(conditions = condition, inc = incR, dalys = dalys,
                          mortality = mProb, nyears = -1, vaccAge = ageV,
                          vaccEff = efficacy, vaccDur = duration)
    impModels
})

impactPlot <- reactive({
  impModels <- impactData()
  ageV <- isolate(input$ageV)
  duration <- isolate(input$duration)
  condition <-isolate(input$condition)
  p <- makePlot(noVacc_mod = impModels[[1]], vacc_mod = impModels[[2]],
           conditions = condition, vAge = ageV, vDur = duration)
  ggarrange(p[[1]], p[[2]], ncol = 1, nrow = 2)
})

impactTable <- reactive({
  impModels <- impactData()
  condition <-isolate(input$condition)
  impT <- makeTable(noVacc_mod = impModels[[1]], vacc_mod = impModels[[2]],
                    conditions = condition, initPop = 100000,
                    valueVac = -1)
  impT <- round(impT, digits=0)
  impT
})

output$impactPlot <- renderPlot({
  impactPlot()
})

output$impactTable <- renderTable({
  impactTable()
}, rownames = TRUE)


output$saveImpactTable <- downloadHandler(
  filename = function() {
    paste("impactTable", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(impactTable(), file, row.names = T)
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
