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
# #Set constants
# rate <- 100000
# dRate <- 0
# cycle <-85
#
#
#
# #Incident rates tab
# impactPlot <- reactiveVal()
#
# impactData <- reactive({
#
#     input$submitButton1
#
#     country <- isolate(input$country)
#     condition <-isolate(input$condition)
#     ageV <- isolate(input$ageV)
#     duration <- isolate(input$duration)
#     coverage <- isolate(input$coverage)
#     efficacy <- isolate(input$efficacy)
#
#     incR <- getRateData(country, condition)
#
#
#     impT<-markovModel(ageV, incR, 100000, imp_cost, imp_burden,
#                        imp_bur_rate, imp_mor, imp_popsize, imp_d_rate, imp_cycle,
#                        imp_vaccAge, imp_vaccEff, imp_vaccDur, imp_valueVacc)
#
#     meltInc<-melt(impT[c("Incidents (No Vacc)", "Incidents (Vacc)"),-which(colnames(impT)=="Total")])
#     colnames(meltInc)<-c("Scenario", "Condition", "NoInc")
#     plot1<-ggplot(data=meltInc, aes(x=Condition, y=NoInc, fill=Scenario )) +
#       geom_bar(position="stack", stat="identity")+
#       scale_fill_discrete(name = "", labels = c("No vaccine", "Vaccine"))+
#       ylab("Healthcare episodes")
#
#     meltBur<-melt(impT[c("DALYs (No Vacc)", "DALYs (Vacc)"),-which(colnames(impT)=="Total")])
#     colnames(meltBur)<-c("Scenario", "Condition", "NoDALYS")
#     plot2<-ggplot(data=meltBur, aes(x=Condition, y=NoDALYS, fill=Scenario )) +
#       geom_bar(position="stack", stat="identity")+
#       scale_fill_discrete(name = "", labels = c("No vaccine", "Vaccine"))+
#       ylab("DALYs")
#
#     meltCost<-melt(impT[c("Cost (No Vacc)", "Cost (Vacc)"),-which(colnames(impT)=="Total")]/10000)
#     colnames(meltCost)<-c("Scenario", "Condition", "Cost")
#     plot3<-ggplot(data=meltCost, aes(x=Condition, y=Cost, fill=Scenario )) +
#       geom_bar(position="stack", stat="identity")+
#       scale_fill_discrete(name = "", labels = c("No vaccine", "Vaccine"))+
#       ylab("Cost* (x 10,000)")+
#       labs(caption = "* not taking into account cost of vaccine")
#
#
#     CostDiff<-imp_valueVacc*imp_popsize+impT["Cost (Vacc)",]-impT["Cost (No Vacc)",]
#     DALYSGained<-impT["DALYs (No Vacc)",]-impT["DALYs (Vacc)",]
#     ICERdata<-data.frame(CostDiff=CostDiff/100000, DALYSGained)
#
#     m<-imp_ICER/100000
#
#     ymax<-max(abs(ICERdata$CostDiff))
#     xmax<-max(abs(ICERdata$DALYSGained))
#     plot4<-ggplot(ICERdata, aes(x=DALYSGained, y=CostDiff, label=rownames(ICERdata)))+
#       geom_point(aes(colour = factor(rownames(ICERdata))), size=3)+
#       labs(colour="")+
#       geom_abline(slope=m, linetype="dashed")+
#       geom_text(x=-ymax/m, y=-ymax+ymax/10, label="Maximum acceptable ICER", size=4)+
#       geom_hline(yintercept = 0)+
#       geom_vline(xintercept = 0)+
#       xlim(-xmax, xmax)+
#       ylim(-ymax, ymax)+
#       xlab("DALYs gained")+
#       ylab("Cost difference (x 100,000)")
#
#     impactPlot(ggarrange(plot1, plot2, plot3, plot4,
#                          ncol = 2, nrow = 2))
#
#
#     impT<-round(impT, digits=0)
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
