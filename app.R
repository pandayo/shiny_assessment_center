library(shiny)
library(shinydashboard)

if (!exists("createAssessment", mode = "function"))
  source("assessmentRenderer.R")

##### User Interface ###########################################################
title <- "Placeholder"
assessment1 <- createAssessment(title, "data/ass1.csv")
print(assessment1)

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
  
})

##### Run the application ######################################################
shinyApp(ui = ui, server = server)