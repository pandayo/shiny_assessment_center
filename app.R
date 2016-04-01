library(shiny)
library(shinydashboard)

if (!exists("createAssessment", mode = "function"))
  source("assessmentRenderer.R")

##### Parameter ################################################################
title <- "Placeholder"
assessment1 <- createAssessment("Assessment1", "data/ass1.csv")

##### User Interface ###########################################################
ui <- shinyUI(dashboardPage(
  dashboardHeader(title = title),
  dashboardSidebar(
    sidebarMenu(
    assessment1[[1]]
  )),
  dashboardBody(
    tabItems(
      assessment1[[2]]
    )
  )
))

##### Server ###################################################################
server <- shinyServer(function(input, output, session) {
  
  output$outputAssessment1 <- renderText({
    "Hello"
  })
  
})

##### Run the application ######################################################
shinyApp(ui = ui, server = server)