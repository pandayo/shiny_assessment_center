library(shiny)
library(markdown)
library(shinydashboard)

if (!exists("createAssessment", mode = "function"))
  source("assessmentRenderer.R")

##### Parameter ################################################################
title <- "Training"
assessment1 <-
  createAssessment(
    "Assessment1", "data/ass1.csv", sep = ",",quote = "\"", sbs = T
  )
assessment2 <-
  createAssessment(
    "Distribution", "data/ass2.csv", sep = ",",quote = "\"", norm.meanlog = 2,
    norm.sdlog = 2, choice.inline = F
  )
assessment3 <-
  createAssessment(
    "Placeholder", "data/ass1.csv", sep = ",",quote = "\"", enable.hints = T
  )
assessment4 <-
  createAssessment(
    "Insert_Title_Here", "data/ass2.csv", sep = ",",quote = "\"",
    norm.meanlog = 2, norm.sdlog = 2, choice.inline = F, sbs = T, enable.hints = T
  )
assessment5 <-
  createAssessment(
    "ok", "data/ass1.csv", sep = ",",quote = "\"", choice.inline = F, sbs = T,
    enable.hints = T
  )

##### User Interface ###########################################################
ui <- shinyUI(dashboardPage(
  dashboardHeader(title = title),
  dashboardSidebar(
    sidebarMenu(
      assessment1$sidebar,assessment2$sidebar,assessment3$sidebar,
      assessment4$sidebar,assessment5$sidebar
    )
  ),
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
    tags$script(src = "script.js")
  ),withMathJax(
    tabItems(
      assessment1$main,assessment2$main,assessment3$main,assessment4$main,assessment5$main
    )
  ))
))

##### Server ###################################################################
server <- shinyServer(function(input, output, session) {
  output$outputAssessment1 <- renderUI({
    correct <- 0
    correct.statement <- list()
    for (i in 1:length(assessment1$choiceAnswers)) {
      if (assessment1$answerType[[i]] == "Math") {
        if (abs(as.double(input[[paste(assessment1$title,i,sep = "_")]])
                - assessment1$mathAnswer[[i]]) <= assessment1$delta[[i]]) {
          correct <- correct + 1
          correct.statement <- c(correct.statement, correct.math.statement(i, assessment1$mathAnswer[[i]]))
        }else{
          correct.statement <- c(correct.statement, correct.statement.text(i, incorrect = T))
        }
      } else {
        if (!is.null(input[[paste(assessment1$title,i,sep = "_")]])) {
          if (substr(input[[paste(assessment1$title,i,sep = "_")]],1,1)
              == assessment1$choiceAnswers[[i]]) {
            correct <- correct + 1
            correct.statement <- c(correct.statement, correct.statement.text(i))
          }else{
            correct.statement <- c(correct.statement, correct.statement.text(i, incorrect = T))
          }
        }else{
          correct.statement <- c(correct.statement, correct.statement.text(i, incorrect = T))
        }
      }
    }
    return(c(list(p(
      paste(
        "You have",correct,"of",length(assessment1$mathAnswers),"correct. (",correct *
          100 / length(assessment1$mathAnswers), "%)"
      )
    )),correct.statement))
  })
  
  output$outputDistribution <- renderUI({
    correct <- 0
    for (i in 1:length(assessment2$choiceAnswers)) {
      if (assessment2$answerType[[i]] == "Math") {
        if (abs(as.double(input[[paste(assessment2$title,i,sep = "_")]])
                - assessment2$mathAnswer[[i]]) <= assessment2$delta[[i]]) {
          correct <- correct + 1
        }
      } else {
        if (!is.null(input[[paste(assessment2$title,i,sep = "_")]])) {
          if (substr(input[[paste(assessment2$title,i,sep = "_")]],1,1)
              == assessment2$choiceAnswers[[i]]) {
            correct <- correct + 1
          }
        }
      }
    }
    p(paste(
      "You have",correct,"of",length(assessment2$mathAnswers),"correct. (",correct *
        100 / length(assessment2$mathAnswers), "%)"
    ))
  })
  
  output$outputPlaceholder <- renderUI({
    correct <- 0
    for (i in 1:length(assessment3$choiceAnswers)) {
      if (assessment3$answerType[[i]] == "Math") {
        if (abs(as.double(input[[paste(assessment3$title,i,sep = "_")]])
                - assessment3$mathAnswer[[i]]) <= assessment3$delta[[i]]) {
          correct <- correct + 1
        }
      } else {
        if (!is.null(input[[paste(assessment3$title,i,sep = "_")]])) {
          if (substr(input[[paste(assessment3$title,i,sep = "_")]],1,1)
              == assessment3$choiceAnswers[[i]]) {
            correct <- correct + 1
          }
        }
      }
    }
    p(paste(
      "You have",correct,"of",length(assessment3$mathAnswers),"correct. (",correct *
        100 / length(assessment3$mathAnswers), "%)"
    ))
  })
  
  output$outputInsert_Title_Here <- renderUI({
    correct <- 0
    for (i in 1:length(assessment4$choiceAnswers)) {
      if (assessment4$answerType[[i]] == "Math") {
        if (abs(as.double(input[[paste(assessment4$title,i,sep = "_")]])
                - assessment4$mathAnswer[[i]]) <= assessment4$delta[[i]]) {
          correct <- correct + 1
        }
      } else {
        if (!is.null(input[[paste(assessment4$title,i,sep = "_")]])) {
          if (substr(input[[paste(assessment4$title,i,sep = "_")]],1,1)
              == assessment4$choiceAnswers[[i]]) {
            correct <- correct + 1
          }
        }
      }
    }
    p(paste(
      "You have",correct,"of",length(assessment4$mathAnswers),"correct. (",correct *
        100 / length(assessment4$mathAnswers), "%)"
    ))
  })
  
  output$outputok <- renderUI({
    correct <- 0
    for (i in 1:length(assessment5$choiceAnswers)) {
      if (assessment5$answerType[[i]] == "Math") {
        if (abs(as.double(input[[paste(assessment5$title,i,sep = "_")]])
                - assessment5$mathAnswer[[i]]) <= assessment5$delta[[i]]) {
          correct <- correct + 1
        }
      } else {
        if (!is.null(input[[paste(assessment5$title,i,sep = "_")]])) {
          if (substr(input[[paste(assessment5$title,i,sep = "_")]],1,1)
              == assessment5$choiceAnswers[[i]]) {
            correct <- correct + 1
          }
        }
      }
    }
    p(paste(
      "You have",correct,"of",length(assessment5$mathAnswers),"correct. (",correct *
        100 / length(assessment5$mathAnswers), "%)"
    ))
  })
  
  correct.math.statement <- function(i, answer){
    return(list(p(
      paste(
        "Du hast Frage",i,"korrekt beantwortet. Die exakte Lösung ist",
        answer,"."
      )
    )))
  }
  
  correct.statement.text <- function(i,incorrect = F) {
    if (incorrect) {
      return(list(p(
        paste("Du hast Frage",i,"inkorrekt beantwortet.")
      )))
    }else{
      return(list(p(paste(
        "Du hast Frage",i,"korrekt beantwortet."
      ))))
    }
  }
  
})

##### Run the application ######################################################
shinyApp(ui = ui, server = server)