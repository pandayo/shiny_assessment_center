require(shiny)
require(shinydashboard)

submitText <- "submitButton"

createAssessment <-
  function(title, file, ..., assessmentIcon = icon("minus")) {
    data <- read.csv(file, na.strings = "NA", header = T, ...)
    sidebar <-
      menuItem(title, tabName = title, icon = assessmentIcon)
    body <- list()
    mathAnswers <- c()
    choiceAnswers <- list()
    for (i in 1:length(data$QuestionInputType)) {
      if (data$QuestionInputType[[i]] == "MD") {
        question <- withMathJax(p(includeMarkdown(paste(data$Question[[i]]))))
      }else{
        question <- p(data$Question[[i]])
      }
      if (data$AnswerType[[i]] == "Math") {
        answer <-
          numericInput(
            inputId = paste(title,i,sep = "_"), value = 0, label = "Answer:"
          )
        mathAnswers <- c(mathAnswers, as.integer(data$MathAnswer[i]))
        choiceAnswers <- c(choiceAnswers, list(NULL))
      }else{
        answer <-
          radioButtons(
            inputId = paste(title,i,sep = "_"), label = "Answer:",
            choices = c(
              paste("A",data$ChoiceA[[i]],sep = ": "),
              paste("B",data$ChoiceB[[i]],sep = ": "),
              paste("C",data$ChoiceC[[i]],sep = ": "),
              paste("D",data$ChoiceD[[i]],sep = ": ")
            ),
            selected = F, inline = F
          )
        mathAnswers <- c(mathAnswers, NA)
        choiceAnswers <-
          c(choiceAnswers, list(data$ChoiceAnswer[i]))
      }
      body <- c(body, list(question, answer))
    }
    body <-
      c(body, list(submitButton(
        text = submitText,icon = icon("check")
      )))
    body <-
      c(body, list(br(),br(),htmlOutput(paste(
        "output",title,sep = ""
      ))))
    main <- tabItem(tabName = title, h2(title), body)
    assessment <-
      list(
        sidebar = sidebar, main = main, mathAnswers = mathAnswers, 
        choiceAnswers = choiceAnswers, answerType = data$AnswerType, 
        delta = data$MathDelta, title = title
      )
    return(assessment)
  }