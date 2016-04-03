library(shiny)
library(shinydashboard)

if (!exists("createAssessment", mode = "function"))
  source("assessmentRenderer.R")

##### Parameter ################################################################
title <- "Placeholder"
assessment1 <- createAssessment("Assessment1", "data/ass1.csv", sep = ",",quote = "\"")

##### User Interface ###########################################################
ui <- shinyUI(dashboardPage(
  dashboardHeader(title = title),
  dashboardSidebar(sidebarMenu(assessment1$sidebar)),
  dashboardBody(tabItems(assessment1$main))
))

##### Server ###################################################################
server <- shinyServer(function(input, output, session) {
  output$outputAssessment1 <- renderUI({
    correct <- 0
    for (i in 1:length(assessment1$choiceAnswers)) {
      if (assessment1$answerType[[i]] == "Math") {
        if (abs(as.integer(input[[paste(assessment1$title,i,sep = "_")]])-assessment1$mathAnswer[[i]]) <= assessment1$delta[[i]]) {
          correct <- correct + 1
        }
      } else {
        if (!is.null(input[[paste(assessment1$title,i,sep = "_")]])) {
          if (substr(input[[paste(assessment1$title,i,sep = "_")]],1,1) == assessment1$choiceAnswers[[i]]) {
            correct <- correct + 1
          }
        }
      }
    }
    p(paste(
      "You have",correct,"of",length(assessment1$mathAnswers),"correct. (",correct*100/length(assessment1$mathAnswers), "%)"
    ))
  })
  
})

##### Run the application ######################################################
shinyApp(ui = ui, server = server)