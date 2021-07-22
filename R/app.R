# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
#
# library(shiny)
# library(stringr)
# library(ggplot2)
# library(reshape2)
# library(ggpubr)
# library(rCAT)
# #library(GASImpactModel)
#
#
# ##################################################################################################
# #User interface code
#
# ui <- navbarPage(title = "",
#
#     tabPanel(title = "Incident rates",
#
#      img(src = "Savac-logo.png", height = 140, width = 400),
#
#      sidebarLayout(
#         sidebarPanel(
#             h4("Health care"),
#             fileInput('care_incidents_raw', 'Care episodes',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             fileInput('care_pop', 'Care population',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             fileInput('care_prop', 'Proportion of episodes attributable to GAS',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             fileInput('care_group', 'Display results using disease groups',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             numericInput(inputId = "care_yrs", label = "Episodes over how many years?", value = 1, min=1),
#             selectInput(inputId = "care_rate", label = "Rate (incidents per n persons):",
#                         choices = c("100", "100,000")),
#             actionButton("submitButton1", "Find incident rates", class = "btn-success"),
#             downloadButton("saveTable", "Save table"),
#             downloadButton("savePlot", "Save plot")
#         ),
#
#
#         mainPanel(
#             tableOutput('incRatesTable'),
#             plotOutput('incRatesPlot')
#
#         )
#       ),#end sidebarLayout
#     ),#end tabPanel
#
#    tabPanel(title = "Cost",
#             img(src = "Savac-logo.png", height = 140, width = 400),
#    ),
#
#    tabPanel(title = "Health burden",
#
#     img(src = "Savac-logo.png", height = 140, width = 400),
#
#     sidebarLayout(
#         sidebarPanel(
#
#             h4("Population"),
#             fileInput('pop_age', 'Population',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             fileInput('life_ex', 'Life expectancy',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             fileInput('inc_rates', 'Incident rates',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             selectInput(inputId = "i_rate", label = "Rate used (incidents per n persons):",
#                         choices = c("100", "100,000")),
#             fileInput('deaths', 'Death rates',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             selectInput(inputId = "death_rate", label = "Rate used (deaths per n persons):",
#                         choices = c("100", "100,000")),
#             h4("Disease"),
#             fileInput('dw_acute', 'Acute disability weights',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             fileInput('dw_chronic', 'Chronic disability weights',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             fileInput('dis_prog', 'Proportion of episodes that progress to chronic condition',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             fileInput('dis_dur', 'Disease duration',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#             numericInput(inputId = "d_rate", label = "Discount rate %", value = 0, min=0, max=10, step=0.5),
#             fileInput('daly_group', 'Display results using disease groups',
#                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#
#             actionButton("submitButton2", "Find DALYs", class = "btn-success"),
#             downloadButton("saveDALYTable", "Save table"),
#             downloadButton("saveDALYPlot", "Save plot")
#     ),
#
#         mainPanel(
#             tableOutput('dalyTable'),
#             plotOutput('dalyPlot')
#         )
#
#     ),#end sidebarLayout
#    ),#end tabPanel
#
#
#    tabPanel(title= "Impact analysis",
#             img(src = "Savac-logo.png", height = 140, width = 400),
#
#             sidebarLayout(
#               sidebarPanel(
#
#                 fileInput('imp_age', 'Age groups',
#                           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#                 fileInput('imp_inc_rates', 'Incident rates',
#                           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#                 selectInput(inputId = "imp_rate", label = "Rate used (incidents per n persons):",
#                             choices = c("100", "100,000")),
#                 fileInput('imp_cost', 'Cost per person',
#                           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#                 fileInput('imp_burden', 'DALYs',
#                           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#                 selectInput(inputId = "imp_bur_rate", label = "Rate used (DALYs per n persons):",
#                             choices = c("100", "100,000")),
#                 fileInput('imp_mor', 'Mortality rates at age',
#                           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#                 numericInput(inputId = "imp_popsize", label = "Initial population size", value=100000, min=100),
#                 numericInput(inputId = "imp_cycle", label = "Project over n years", value = 10, min=10, max=85, step=5),
#                 numericInput(inputId = "imp_d_rate", label = "Discount rate %", value = 0, min=0, max=10, step=0.5),
#                 numericInput(inputId = "imp_vaccEff", label = "Vaccine effectiveness %", value = 50, min=0, max=100, step=5),
#                 numericInput(inputId = "imp_vaccDur", label = "Duration of effectiveness", value = 1, min=1, max=15, step=1),
#                 numericInput(inputId = "imp_vaccAge", label = "Age of vaccination", value = 0, min=0, max=65, step=1),
#                 numericInput(inputId = "imp_valueVacc", label = "Value of vaccine", value = 0, min=0, max=200, step=1),
#                 numericInput(inputId = "imp_ICER", label = "Maximum acceptable ICER", value = 50000, min=0, max=100000, step=10000),
#
#                 actionButton("submitButton3", "Run analysis", class = "btn-success"),
#                 downloadButton("saveImpactTable", "Save table"),
#                 downloadButton("saveImpactPlot", "Save plot")
#
#               ),
#
#               mainPanel(
#                 tableOutput('impactTable'),
#                 plotOutput('impactPlot')
#               )
#
#             ),#end sidebarLayout
#   )#end tabpanel
# )#end navbarPage
#
#
# server <- function(input, output) {
#
# #Incident rates tab
# incRatesPlot <- reactiveVal()
#
# incRatesData <- reactive({
#
#     input$submitButton1
#
#     epFile <- isolate(input$care_incidents_raw)
#     popFile <-isolate(input$care_pop)
#     propFile <- isolate(input$care_prop)
#     groupFile <- isolate(input$care_group)
#     yrs <- isolate(input$care_yrs)
#     rate <- isolate(as.numeric(str_replace(input$care_rate, ",", "")))
#
#     if(is.null(epFile)||is.null(popFile)||is.null(propFile))
#         return(NULL)
#
#     ep <-read.csv(epFile$datapath, check.names=FALSE)
#     pop <-read.csv(popFile$datapath, check.names=FALSE)
#     prop <-read.csv(propFile$datapath, check.names=FALSE)
#     group <-read.csv(groupFile$datapath, check.names=FALSE)
#
#     incRatesT<-incRates(careEp = ep, pop = pop, propAttr = prop, perPopSize = rate, years = yrs, grps=group)
#
#     if(is.data.frame(group))
#     {
#         incRatesMeans<-t(incRatesT[, colnames(group)])
#     }else{
#         incRatesMeans<-t(incRatesT[, colnames(ep)[-1]])
#     }
#
#     meltIncRates<-melt(incRatesMeans)
#     colnames(meltIncRates)<-c("Condition", "Age", "Rate")
#     incRatesPlot(ggplot(data=meltIncRates, aes(x=Age, y=Rate, fill=Condition )) +
#                       geom_bar(position="stack", stat="identity"))
#
#     incRatesT<-cbind(rownames(incRatesT), round(incRatesT, digits=2))
#     colnames(incRatesT)[1]<-"Age"
#     for(i in 1:length(colnames(incRatesT)))
#     {
#         if(length(grep("Lower", colnames(incRatesT)[i]))) colnames(incRatesT)[i]<-"L 95%"
#         if(length(grep("Upper", colnames(incRatesT)[i]))) colnames(incRatesT)[i]<-"U 95%"
#     }
#
#     incRatesT
# })
#
# output$incRatesPlot <- renderPlot({
#     incRatesPlot()
# })
#
# output$incRatesTable <- renderTable({
#     incRatesData()
# })
#
#
# output$saveTable <- downloadHandler(
#     filename = function() {
#         paste("incidentRatesTable", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#         write.csv(incRatesData(), file, row.names = F)
#     }
#  )
#
# output$savePlot <- downloadHandler(
#     filename = function() {
#         paste("incidentRatesPlot", Sys.Date(), ".jpeg", sep="")
#     },
#     content = function(file) {
#         ggsave(file, incRatesPlot())
#     }
# )
#
# ##Health burden tab
# dalyPlot <- reactiveVal()
#
# dalyData <- reactive({
#
#     input$submitButton2
#
#     popFile <- isolate(input$pop_age)
#     lifeFile <- isolate(input$life_ex)
#     incFile <- isolate(input$inc_rates)
#     rate <- isolate(as.numeric(str_replace(input$i_rate, ",", "")))
#     dwAFile <- isolate(input$dw_acute)
#     dwCFile <- isolate(input$dw_chronic)
#     deathFile <- isolate(input$deaths)
#     death_rate <- isolate(as.numeric(str_replace(input$death_rate, ",", "")))
#     progFile <- isolate(input$dis_prog)
#     durFile <- isolate(input$dis_dur)
#     dRate <- isolate(input$d_rate)
#     grpFile <- isolate(input$daly_group)
#
#     if(is.null(popFile)||is.null(lifeFile)||is.null(incFile)||is.null(dwAFile)||
#        is.null(dwCFile)||is.null(deathFile)||is.null(progFile)||is.null(durFile)||
#        is.null(grpFile))
#         return(NULL)
#
#     pop <-read.csv(popFile$datapath, check.names=FALSE)
#     lifeEx <-read.csv(lifeFile$datapath, check.names = FALSE)
#     inc_rates <-read.csv(incFile$datapath, check.names=FALSE)
#     dw_acute <-read.csv(dwAFile$datapath, check.names=FALSE)
#     dw_chronic <-read.csv(dwCFile$datapath, check.names=FALSE)
#     deaths <-read.csv(deathFile$datapath, check.names = FALSE)
#     prog <-read.csv(progFile$datapath, check.names=FALSE)
#     dur <-read.csv(durFile$datapath, check.names=FALSE)
#     grp <-read.csv(grpFile$datapath, check.names=FALSE)
#
#     dalysT<-dalys(incR = inc_rates, rate = rate, pop =pop, life = lifeEx, dw_A = dw_acute,
#                   dw_C = dw_chronic, prog = prog, dRate = dRate,
#                   duration =dur, deaths = deaths, deathRate = death_rate)
#
#     dalysT<-100000*dalysT/pop[,"n"] #get DALYs per 100,000
#     dalyMeans<-t(dalysT[, colnames(inc_rates)[-1]])
#
#     meltDALYS<-melt(dalyMeans)
#     colnames(meltDALYS)<-c("Condition", "Age", "DALYS")
#     dalyPlot(ggplot(data=meltDALYS, aes(x=Age, y=DALYS, fill=Condition )) +
#                      geom_bar(position="stack", stat="identity")+
#                      ylab("DALYs per 100,000 persons"))
#
#     dalysT<-cbind(rownames(dalysT), round(dalysT, digits=2))
#     colnames(dalysT)[1]<-"Age"
#     for(i in 1:length(colnames(dalysT)))
#     {
#         if(length(grep("Lower", colnames(dalysT)[i]))) colnames(dalysT)[i]<-"L 95%"
#         if(length(grep("Upper", colnames(dalysT)[i]))) colnames(dalysT)[i]<-"U 95%"
#     }
#
#     dalysT
# })
#
# output$dalyPlot <- renderPlot({
#     dalyPlot()
# })
#
# output$dalyTable <- renderTable({
#     dalyData()
# })
#
#
# output$saveDALYTable <- downloadHandler(
#     filename = function() {
#         paste("dalyTable", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#         write.csv(dalyData(), file, row.names = F)
#     }
# )
#
# output$saveDALYPlot <- downloadHandler(
#     filename = function() {
#         paste("dalyPlot", Sys.Date(), ".jpeg", sep="")
#     },
#     content = function(file) {
#         ggsave(file, dalyPlot())
#     }
# )
#
# ##Impact analysis tab
# impactPlot <- reactiveVal()
#
# impactData <- reactive({
#
#   input$submitButton3
#
#   imp_ageFile <- isolate(input$imp_age)
#   imp_incFile <- isolate(input$imp_inc_rates)
#   imp_rate <- isolate(as.numeric(str_replace(input$imp_rate, ",", "")))
#   imp_costFile <- isolate(input$imp_cost)
#   imp_burdenFile <- isolate(input$imp_burden)
#   imp_bur_rate <- isolate(as.numeric(str_replace(input$imp_bur_rate, ",", "")))
#   imp_morFile <- isolate(input$imp_mor)
#   imp_popsize <- isolate(input$imp_popsize)
#   imp_cycle <- isolate(input$imp_cycle)
#   imp_d_rate <- isolate(input$imp_d_rate)
#   imp_vaccEff <- isolate(input$imp_vaccEff)
#   imp_vaccDur <- isolate(input$imp_vaccDur)
#   imp_vaccAge <- isolate(input$imp_vaccAge)
#   imp_valueVacc <- isolate(input$imp_valueVacc)
#   imp_ICER <-isolate(input$imp_ICER)
#
#   if(is.null(imp_ageFile)||is.null(imp_incFile)||is.null(imp_costFile)||
#      is.null(imp_burdenFile)||is.null(imp_morFile))
#     return(NULL)
#
#   imp_age <- read.csv(imp_ageFile$datapath, check.names=FALSE)
#   imp_inc_rates <-read.csv(imp_incFile$datapath, check.names=FALSE)
#   imp_cost <-read.csv(imp_costFile$datapath, check.names=FALSE)
#   imp_burden <-read.csv(imp_burdenFile$datapath, check.names=FALSE)
#   imp_mor <-read.csv(imp_morFile$datapath, check.names=FALSE)
#
#   impT<-markovModel(imp_age, imp_inc_rates, imp_rate, imp_cost, imp_burden,
#                     imp_bur_rate, imp_mor, imp_popsize, imp_d_rate, imp_cycle,
#                     imp_vaccAge, imp_vaccEff, imp_vaccDur, imp_valueVacc)
#
#   meltInc<-melt(impT[c("Incidents (No Vacc)", "Incidents (Vacc)"),-which(colnames(impT)=="Total")])
#   colnames(meltInc)<-c("Scenario", "Condition", "NoInc")
#   plot1<-ggplot(data=meltInc, aes(x=Condition, y=NoInc, fill=Scenario )) +
#            geom_bar(position="stack", stat="identity")+
#            scale_fill_discrete(name = "", labels = c("No vaccine", "Vaccine"))+
#            ylab("Healthcare episodes")
#
#   meltBur<-melt(impT[c("DALYs (No Vacc)", "DALYs (Vacc)"),-which(colnames(impT)=="Total")])
#   colnames(meltBur)<-c("Scenario", "Condition", "NoDALYS")
#   plot2<-ggplot(data=meltBur, aes(x=Condition, y=NoDALYS, fill=Scenario )) +
#     geom_bar(position="stack", stat="identity")+
#     scale_fill_discrete(name = "", labels = c("No vaccine", "Vaccine"))+
#     ylab("DALYs")
#
#   meltCost<-melt(impT[c("Cost (No Vacc)", "Cost (Vacc)"),-which(colnames(impT)=="Total")]/10000)
#   colnames(meltCost)<-c("Scenario", "Condition", "Cost")
#   plot3<-ggplot(data=meltCost, aes(x=Condition, y=Cost, fill=Scenario )) +
#     geom_bar(position="stack", stat="identity")+
#     scale_fill_discrete(name = "", labels = c("No vaccine", "Vaccine"))+
#     ylab("Cost* (x 10,000)")+
#     labs(caption = "* not taking into account cost of vaccine")
#
#
#   CostDiff<-imp_valueVacc*imp_popsize+impT["Cost (Vacc)",]-impT["Cost (No Vacc)",]
#   DALYSGained<-impT["DALYs (No Vacc)",]-impT["DALYs (Vacc)",]
#   ICERdata<-data.frame(CostDiff=CostDiff/100000, DALYSGained)
#
#   m<-imp_ICER/100000
#
#   ymax<-max(abs(ICERdata$CostDiff))
#   xmax<-max(abs(ICERdata$DALYSGained))
#   plot4<-ggplot(ICERdata, aes(x=DALYSGained, y=CostDiff, label=rownames(ICERdata)))+
#     geom_point(aes(colour = factor(rownames(ICERdata))), size=3)+
#     labs(colour="")+
#     geom_abline(slope=m, linetype="dashed")+
#     geom_text(x=-ymax/m, y=-ymax+ymax/10, label="Maximum acceptable ICER", size=4)+
#     geom_hline(yintercept = 0)+
#     geom_vline(xintercept = 0)+
#     xlim(-xmax, xmax)+
#     ylim(-ymax, ymax)+
#     xlab("DALYs gained")+
#     ylab("Cost difference (x 100,000)")
#
#   impactPlot(ggarrange(plot1, plot2, plot3, plot4,
#                        ncol = 2, nrow = 2))
#
#
#   impT<-round(impT, digits=0)
#
#   impT
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
