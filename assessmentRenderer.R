require(shiny)
require(shinydashboard)

submitText <- "submitButton"

createAssessment <-
  function(title, file, ..., assessmentIcon = icon("minus")) {
    data <- read.csv(file, ...)
    print(data)
    sidebar <-
      menuItem(title, tabName = title, icon = assessmentIcon)
    body <- list()
    answers <- c()
    for (i in 1:length(data$QuestionInputType)) {
      if (data$QuestionInputType[[i]] == "MD") {
        question <- p(withMathJax(includeMarkdown(data$Question)))
      }else{
        question <- p(data$Question)
      }
      if (data$AnswerType[[i]] == "Math") {
        answer <- numericInput(inputId = paste(title,i,sep = ""), value = 0, label = "Answer:")
        answers <- c(answers, data$MathAnswer[[i]])
      }else{
        answer <-
          radioButtons(
            inputId = paste(title,i,sep = ""), label = "Answer:",
            choices = c(
              paste("A",data$ChoiceA[[i]],sep = ": "),
              paste("B",data$ChoiceB[[i]],sep = ": "),
              paste("C",data$ChoiceC[[i]],sep = ": "),
              paste("D",data$ChoiceD[[i]],sep = ": ")
            ),
            selected = NULL, inline = F
          )
        answers <- c(answers, data$ChoiceAnswer[[i]])
      }
      body <- c(body, list(question, answer))
    }
    body <- c(body, submitButton(text = submitText,icon = icon("check")))
    print(paste("output",title,sep = ""))
    body <- c(body, htmlOutput(paste("output",title,sep = "")))
    main <- tabItem(tabName = title, h2(title), div(body))
    assessment <- list(sidebar, main, answers)
    return(assessment)
  }