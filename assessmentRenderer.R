require(shiny)
require(markdown)
require(shinydashboard)

submitText <- "Submit Solutions"

createAssessment <-
  function(title, file, ..., assessmentIcon = icon("minus"), norm.meanlog = 0, 
           norm.sdlog = 1, choice.inline = T, sbs = F, enable.hints = F) {
    data <- read.csv(file, na.strings = "NA", header = T, ...)
    sidebar <-
      menuItem(title, tabName = title, icon = assessmentIcon)
    body <- list()
    mathAnswers <- c()
    choiceAnswers <- list()
    for (i in 1:length(data$QuestionInputType)) {
      if (data$QuestionInputType[[i]] == "MD") {
        if (sbs) {
          question <-
            withMathJax(div(includeMarkdown(paste(
              data$Question[[i]]
            )),
            class = "answerquestion"))
        }else{
          question <-
            withMathJax(div(includeMarkdown(paste(
              data$Question[[i]]
            ))))
        }
      }else{
        if (sbs) {
          question <- p(data$Question[[i]],
                        class = "answerquestion")
        }else{
          question <- p(data$Question[[i]])
        }
      }
      if (data$AnswerType[[i]] == "Math") {
        answer <-
          numericInput(
            inputId = paste(title,i,sep = "_"), 
            value = rlnorm(1,meanlog = norm.meanlog,sdlog = norm.sdlog), 
            label = "Answer:"
          )
        mathAnswers <- c(mathAnswers, as.double(data$MathAnswer[i]))
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
            selected = F, inline = choice.inline
          )
        mathAnswers <- c(mathAnswers, NA)
        choiceAnswers <-
          c(choiceAnswers, list(data$ChoiceAnswer[i]))
      }
      if (enable.hints) {
        if (!is.null(data$Hint[[i]])) {
          if (data$Hint[[i]] != "null" && data$Hint[[i]] != "NULL") {
            if(data$HintType[[i]] == "MD"){
              answer <- div(answer, HTML(
                paste(
                  "<div><div class='button' onclick='changeClass(",'"',
                  paste(title,"hint",i, sep = "_"),'"',
                  ")'>Hint</div><div class='hidden' id=",
                  paste(title,"hint",i, sep = "_"),">",includeMarkdown(paste(data$Hint[[i]])),
                  "</div></div>",sep = ""
                )
              ))
            }else{
              answer <- div(answer, HTML(
                paste(
                  "<div><div class='button' onclick='changeClass(",'"',
                  paste(title,"hint",i, sep = "_"),'"',
                  ")'>Hint</div><div class='hidden' id=",
                  paste(title,"hint",i, sep = "_"),">",data$Hint[[i]],
                  "</div></div>",sep = ""
                )
              ))
            }
          }
        }
      }
      if (sbs == T) {
        body <-
          c(body, list(div(
            question, div(answer, class = "answerquestion"), class = "sidebyside"
          )))
      } else {
        body <- c(body, list(div(question, answer)))
      }
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